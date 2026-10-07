{- | The reader: turns source text into S-expression forms. Lists are
@( ... )@, array literals @[ ... ]@, map literals @{ key value ... }@,
quote is @'form@ (sugar for @(quote form)@), keywords @:name@ read as
strings, and @;@ starts a line comment. Commas are whitespace.
-}
module Script.Reader (readProgram) where

import Data.Char (isSpace)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T

import Script.Form (Form (..), Node (..), Pos)

-- | Read a whole program: a sequence of forms up to end of input.
readProgram :: Text -> Either Text [Form]
readProgram = goTop (1, 1)

goTop :: Pos -> Text -> Either Text [Form]
goTop pos src =
    let (pos', src') = skipSpace pos src
     in case T.uncons src' of
            Nothing -> Right []
            Just _ -> do
                (f, pos'', rest) <- readForm pos' src'
                (f :) <$> goTop pos'' rest

-- | Read one form. Returns the form, the position just after it and
-- the remaining input.
readForm :: Pos -> Text -> Either Text (Form, Pos, Text)
readForm pos src = case T.uncons src of
    Nothing -> Left (errAt pos "unexpected end of input")
    Just (c, rest) -> case c of
        '(' -> do
            (fs, pos', rest') <- readSeq pos ')' rest
            pure (Form pos (NList fs), pos', rest')
        '[' -> do
            (fs, pos', rest') <- readSeq pos ']' rest
            pure (Form pos (NArray fs), pos', rest')
        '{' -> do
            (fs, pos', rest') <- readSeq pos '}' rest
            kvs <- pairUp pos fs
            pure (Form pos (NMap kvs), pos', rest')
        '\'' -> do
            (f, pos', rest') <- readForm (advance pos c) rest
            pure (Form pos (NList [Form pos (NSym "quote"), f]), pos', rest')
        '`' -> do
            (f, pos', rest') <- readForm (advance pos c) rest
            pure (Form pos (NList [Form pos (NSym "quasiquote"), f]), pos', rest')
        ',' -> case T.uncons rest of
            Just ('@', rest') -> do
                (f, pos', rest'') <- readForm (advance (advance pos c) '@') rest'
                pure (Form pos (NList [Form pos (NSym "unquote-splicing"), f]), pos', rest'')
            _ -> do
                (f, pos', rest') <- readForm (advance pos c) rest
                pure (Form pos (NList [Form pos (NSym "unquote"), f]), pos', rest')
        '"' -> do
            (s, pos', rest') <- readString (advance pos c) rest
            pure (Form pos (NStr s), pos', rest')
        _ -> do
            (atom, pos', rest') <- readAtom pos src
            pure (atom, pos', rest')

-- | Read forms until the closing delimiter.
readSeq :: Pos -> Char -> Text -> Either Text ([Form], Pos, Text)
readSeq openPos close = go openPos []
  where
    go pos acc src =
        let (pos', src') = skipSpace pos src
         in case T.uncons src' of
                Nothing -> Left (errAt openPos ("unterminated form: expected '" <> T.singleton close <> "'"))
                Just (c, rest)
                    | c == close -> Right (reverse acc, advance pos' c, rest)
                    | otherwise -> do
                        (f, pos'', rest') <- readForm pos' src'
                        go pos'' (f : acc) rest'

pairUp :: Pos -> [Form] -> Either Text [(Form, Form)]
pairUp pos = go
  where
    go [] = Right []
    go (k : v : rest) = ((k, v) :) <$> go rest
    go [_] = Left (errAt pos "map literal needs an even number of forms")

readString :: Pos -> Text -> Either Text (Text, Pos, Text)
readString startPos = go startPos []
  where
    go pos buf src = case T.uncons src of
        Nothing -> Left (errAt startPos "unterminated string literal")
        Just ('"', rest) -> Right (T.concat (reverse buf), advance pos '"', rest)
        Just ('\\', rest) -> case T.uncons rest of
            Just (e, rest') -> go (advance (advance pos '\\') e) (escape e : buf) rest'
            Nothing -> Left (errAt startPos "unterminated string literal")
        Just (c, rest) -> go (advance pos c) (T.singleton c : buf) rest

    escape :: Char -> Text
    escape e = case e of
        'n' -> "\n"
        't' -> "\t"
        'r' -> "\r"
        '\\' -> "\\"
        '"' -> "\""
        _ -> T.singleton e

-- | Read an atom up to the next delimiter and classify it.
readAtom :: Pos -> Text -> Either Text (Form, Pos, Text)
readAtom pos src =
    let (raw, rest) = T.span (not . isDelim) src
        pos' = T.foldl' advance pos raw
     in case classify raw of
            Left err -> Left (errAt pos err)
            Right node -> Right (Form pos node, pos', rest)
  where
    classify t
        | t == "nil" = Right NNil
        | t == "true" = Right (NBool True)
        | t == "false" = Right (NBool False)
        | isNumber t = Right (NNum (parseNumber t))
        | ":" `T.isPrefixOf` t = Right (NStr (T.drop 1 t))
        | otherwise = Right (NSym t)

    isDelim c = isSpace c || c == ',' || c `elem` ("()[]{}\";'" :: String)

isNumber :: Text -> Bool
isNumber t = case reads (T.unpack t) :: [(Scientific, String)] of
    [(_, "")] -> True
    _ -> False

parseNumber :: Text -> Scientific
parseNumber t = case reads (T.unpack t) :: [(Scientific, String)] of
    [(s, "")] -> s
    _ -> 0

-- | Skip whitespace, commas and @;@ line comments.
skipSpace :: Pos -> Text -> (Pos, Text)
skipSpace pos src = case T.uncons src of
    Just (c, rest)
        | isSpace c -> skipSpace (advance pos c) rest
        | c == ';' ->
            let (_, after) = T.break (== '\n') rest
             in case T.uncons after of
                    Just ('\n', rest') -> skipSpace (advance pos '\n') rest'
                    _ -> (advance pos c, rest)
    _ -> (pos, src)

advance :: Pos -> Char -> Pos
advance (l, c) ch
    | ch == '\n' = (l + 1, 1)
    | otherwise = (l, c + 1)

errAt :: Pos -> Text -> Text
errAt (l, c) msg = "line " <> T.pack (show l) <> ", column " <> T.pack (show c) <> ": " <> msg
