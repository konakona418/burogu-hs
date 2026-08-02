module Lexer (Token (..), TokenKind (..), lexTokens) where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Expr (Op (..))

{- | A token. Position is (line, column), 1-based. String tokens carry
their content as text segments alternating with interpolated source
slices (the code between `#{` and its matching `}`); the parser
tokenises those slices recursively. Nested interpolation is handled
recursively.
-}
data Token = Token {tokPos :: (Int, Int), tokKind :: TokenKind}
    deriving (Show)

data TokenKind
    = TNumber Text
    | TIdent Text
    | TString [(Text, Maybe Text)]
    | TTrue
    | TFalse
    | TNil
    | TDef
    | TEnd
    | TIf
    | TThen
    | TElse
    | TArrow
    | TArrowMap
    | TLbrace
    | TRbrace
    | TLparen
    | TRparen
    | TLbracket
    | TRbracket
    | TComma
    | TOp Op
    | TNot
    | TEof
    deriving (Show)

lexTokens :: Text -> Either Text [Token]
lexTokens input = do
    (toks, lastPos) <- go (1, 1) input
    pure (toks <> [Token lastPos TEof])
  where
    go :: (Int, Int) -> Text -> Either Text ([Token], (Int, Int))
    go pos src = case lexTokenAt pos src of
        Left err -> Left err
        Right (k, rest, pos') -> case k of
            TEof -> pure ([], pos')
            _ -> do
                (toks, lastPos) <- go pos' rest
                pure (Token pos k : toks, lastPos)

{- | Lex one token. Returns the kind, the remaining input and the position
just after the token. Whitespace and comments are skipped. Stops at end
of input with TEof.
-}
lexTokenAt :: (Int, Int) -> Text -> Either Text (TokenKind, Text, (Int, Int))
lexTokenAt pos src = case T.uncons src of
    Nothing -> Right (TEof, T.empty, pos)
    Just (c, rest) -> case c of
        ' ' -> lexTokenAt (advance pos c) rest
        '\t' -> lexTokenAt (advance pos c) rest
        '\n' -> lexTokenAt (advance pos c) rest
        '\r' -> lexTokenAt (advance pos c) rest
        '#' -> skipComment pos rest
        '"' -> do
            (chunks, rest', pos') <- scanString (advance pos c) (advance pos c) rest
            pure (TString chunks, rest', pos')
        _ | isDigit c -> scanNumber pos src
        _ | isAlpha c || c == '_' -> scanIdent pos src
        '(' -> tok TLparen pos c rest
        ')' -> tok TRparen pos c rest
        '[' -> tok TLbracket pos c rest
        ']' -> tok TRbracket pos c rest
        '{' -> tok TLbrace pos c rest
        '}' -> tok TRbrace pos c rest
        ',' -> tok TComma pos c rest
        '-' -> case T.uncons rest of
            Just ('>', rest') -> tok TArrow pos '-' rest'
            _ -> tok (TOp Sub) pos c rest
        '+' -> tok (TOp Add) pos c rest
        '*' -> tok (TOp Mul) pos c rest
        '/' -> tok (TOp Div) pos c rest
        '%' -> tok (TOp Mod) pos c rest
        '!' -> case T.uncons rest of
            Just ('=', rest') -> tok (TOp Ne) pos '!' rest'
            _ -> tok TNot pos c rest
        '=' -> case T.uncons rest of
            Just ('=', rest') -> tok (TOp Eq) pos '=' rest'
            Just ('>', rest') -> tok TArrowMap pos '=' rest'
            _ -> Left (posErr pos "unexpected '=' (did you mean '=='?)")
        '<' -> case T.uncons rest of
            Just ('=', rest') -> tok (TOp Le) pos '<' rest'
            _ -> tok (TOp Lt) pos c rest
        '>' -> case T.uncons rest of
            Just ('=', rest') -> tok (TOp Ge) pos '>' rest'
            _ -> tok (TOp Gt) pos c rest
        '&' -> case T.uncons rest of
            Just ('&', rest') -> tok (TOp And) pos '&' rest'
            _ -> Left (posErr pos "unexpected '&' (did you mean '&&'?)")
        '|' -> case T.uncons rest of
            Just ('|', rest') -> tok (TOp Or) pos '|' rest'
            _ -> Left (posErr pos "unexpected '|' (did you mean '||'?)")
        _ -> Left (posErr pos ("unexpected character " <> T.singleton c))
  where
    tok k p ch r = Right (k, r, advance p ch)

    skipComment p r = case T.span (/= '\n') r of
        (_, after) -> case T.uncons after of
            Just (c, rest') -> lexTokenAt (advance p c) rest'
            Nothing -> Right (TEof, T.empty, p)

    scanNumber p r =
        let (int, rest') = T.span isDigit r
            (fraction, rest'') = case T.uncons rest' of
                Just ('.', r') | not (T.null r') && isDigit (T.head r') -> T.span isDigit r'
                _ -> (T.empty, rest')
            num = int <> if T.null fraction then T.empty else "." <> fraction
         in tok (TNumber num) p (T.head num) rest''

    scanIdent p r =
        let (word, rest') = T.span (\x -> isAlphaNum x || x == '_') r
         in tok (keyword word) p (T.head word) rest'

    keyword w = case w of
        "def" -> TDef
        "end" -> TEnd
        "if" -> TIf
        "then" -> TThen
        "else" -> TElse
        "true" -> TTrue
        "false" -> TFalse
        "nil" -> TNil
        _ -> TIdent w

advance :: (Int, Int) -> Char -> (Int, Int)
advance (l, c) ch
    | ch == '\n' = (l + 1, 1)
    | otherwise = (l, c + 1)

posErr :: (Int, Int) -> Text -> Text
posErr (l, c) msg = "line " <> T.pack (show l) <> ", column " <> T.pack (show c) <> ": " <> msg

{- | Scan a string literal starting just after the opening quote. Returns
the content chunks (text or interpolated source slices), the remaining
input and the position after the closing quote.
-}
scanString :: (Int, Int) -> (Int, Int) -> Text -> Either Text ([(Text, Maybe Text)], Text, (Int, Int))
scanString startPos pos src = go [] [] pos src
  where
    go :: [Text] -> [(Text, Maybe Text)] -> (Int, Int) -> Text -> Either Text ([(Text, Maybe Text)], Text, (Int, Int))
    go buf chunks p r = case T.uncons r of
        Nothing -> Left (posErr startPos "unterminated string literal")
        Just ('"', rest) -> do
            let chunks' = pushBuf buf chunks
            pure (reverse chunks', rest, advance p '"')
        Just ('#', rest) -> case T.uncons rest of
            Just ('{', rest') -> do
                (code, rest'', p') <- scanInterp p rest'
                go [] ((T.empty, Just code) : pushBuf buf chunks) p' rest''
            _ -> go (T.singleton '#' : buf) chunks (advance p '#') rest
        Just ('\\', rest) -> case T.uncons rest of
            Just (e, rest') -> go (escape e : buf) chunks (advance (advance p '\\') e) rest'
            Nothing -> Left (posErr startPos "unterminated string literal")
        Just (c, rest) -> go (T.singleton c : buf) chunks (advance p c) rest

    pushBuf :: [Text] -> [(Text, Maybe Text)] -> [(Text, Maybe Text)]
    pushBuf buf chunks = case buf of
        [] -> chunks
        _ -> (T.concat (reverse buf), Nothing) : chunks

    escape e = case e of
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        '\\' -> "\\"
        '"' -> "\""
        _ -> T.singleton e

{- | Scan the source of one interpolation: everything after `#{` up to
the matching `}`. Braces inside nested blocks and maps count towards
the depth; braces inside nested strings do not (string scanning runs
its own interpolation recursion).
-}
scanInterp :: (Int, Int) -> Text -> Either Text (Text, Text, (Int, Int))
scanInterp = go 1 []
  where
    go :: Int -> [Text] -> (Int, Int) -> Text -> Either Text (Text, Text, (Int, Int))
    go depth buf p r = case T.uncons r of
        Nothing -> Left (posErr p "unterminated interpolation")
        Just ('{', rest) -> go (depth + 1) (T.singleton '{' : buf) (advance p '{') rest
        Just ('}', rest)
            | depth == 1 -> Right (T.concat (reverse buf), rest, advance p '}')
            | otherwise -> go (depth - 1) (T.singleton '}' : buf) (advance p '}') rest
        Just ('"', rest) -> do
            (chunks, rest', p') <- scanString (advance p '"') (advance p '"') rest
            let raw = "\"" <> T.concat [case m of { Nothing -> t; Just c -> t <> "#{" <> c <> "}" } | (t, m) <- chunks] <> "\""
            go depth (raw : buf) p' rest'
        Just ('\\', rest) -> case T.uncons rest of
            Just (e, rest') -> go depth (T.singleton '\\' : T.singleton e : buf) (advance (advance p '\\') e) rest'
            Nothing -> Left (posErr p "unterminated interpolation")
        Just (c, rest) -> go depth (T.singleton c : buf) (advance p c) rest
