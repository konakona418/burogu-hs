module Parser (parseProgram) where

import Data.Text (Text)
import Data.Text qualified as T
import Expr (Chunk (..), Expr (..), Op (..))
import Lexer (Token (..), TokenKind (..), lexTokens)

{- | Parse a whole program: a sequence of expressions (the last one is
the program's value).
-}
parseProgram :: [Token] -> Either Text [Expr]
parseProgram toks = fst <$> parseExprs isEof toks
  where
    isEof TEof = True
    isEof _ = False

{- | Parse a sequence of expressions up to a stop token. Expressions are
adjacent; the sequence value is the last one.
-}
parseExprs :: (TokenKind -> Bool) -> [Token] -> Either Text ([Expr], [Token])
parseExprs stop = go []
  where
    go acc toks = case toks of
        Token _ k : _
            | stop k -> pure (reverse acc, toks)
        Token pos TEof : _ -> Left (parseErr pos "unexpected end of input")
        [] -> Left "unexpected end of input"
        _ -> do
            (e, rest) <- parseExpr toks
            go (e : acc) rest

parseExpr :: [Token] -> Either Text (Expr, [Token])
parseExpr = parseBin 0

parseBin :: Int -> [Token] -> Either Text (Expr, [Token])
parseBin minPrec toks = do
    (lhs, rest) <- parsePrefix toks
    binLoop minPrec lhs rest

binLoop :: Int -> Expr -> [Token] -> Either Text (Expr, [Token])
binLoop minPrec lhs toks = case toks of
    Token _ (TOp op) : rest
        | prec op >= minPrec -> do
            (rhs, rest') <- parseBin (prec op + 1) rest
            binLoop minPrec (combine op lhs rhs) rest'
    _ -> pure (lhs, toks)

combine :: Op -> Expr -> Expr -> Expr
combine op l r = case op of
    And -> EAnd l r
    Or -> EOr l r
    _ -> EBinOp op l r

prec :: Op -> Int
prec op = case op of
    Or -> 2
    And -> 3
    Eq -> 4
    Ne -> 4
    Lt -> 4
    Gt -> 4
    Le -> 4
    Ge -> 4
    Add -> 5
    Sub -> 5
    Mul -> 6
    Div -> 6
    Mod -> 6

parsePrefix :: [Token] -> Either Text (Expr, [Token])
parsePrefix toks = case toks of
    Token _ (TNumber n) : rest -> postfix (ENumber n) rest
    Token _ TTrue : rest -> postfix (EBool True) rest
    Token _ TFalse : rest -> postfix (EBool False) rest
    Token _ TNil : rest -> postfix ENil rest
    Token _ (TString chunks) : rest -> do
        chunks' <- mapM chunkAt chunks
        postfix (EString chunks') rest
    Token _ (TIdent name) : rest -> postfix (EIdent name) rest
    Token _ TDef : rest -> parseDef rest
    Token _ TLparen : rest -> do
        (e, rest') <- parseBin 0 rest
        case rest' of
            Token _ TRparen : rest'' -> postfix e rest''
            _ -> Left "expected ')' after expression"
    Token _ TLbracket : rest -> do
        (arr, rest') <- arrayLiteral rest
        postfix arr rest'
    Token _ TLbrace : rest -> do
        (blk, rest') <- braceLiteral rest
        postfix blk rest'
    Token _ TIf : rest -> ifExpr rest
    Token _ (TOp Sub) : rest -> do
        (e, rest') <- parseBin 7 rest
        postfix (EBinOp Sub (ENumber "0") e) rest'
    Token _ TNot : rest -> do
        (e, rest') <- parseBin 7 rest
        postfix (ENot e) rest'
    Token pos TEof : _ -> Left (parseErr pos "unexpected end of input")
    Token pos k : _ -> Left (parseErr pos ("unexpected " <> describe k))
    [] -> Left "unexpected end of input"

chunkAt :: (Text, Maybe Text) -> Either Text Chunk
chunkAt (t, Nothing) = pure (SLit t)
chunkAt (_, Just src) = do
    toks' <- lexTokens src
    case parseProgram toks' of
        Right [e] -> pure (SInterp e)
        Right _ -> Left "interpolation must contain exactly one expression"
        Left err -> Left ("bad interpolation: " <> err)

parseDef :: [Token] -> Either Text (Expr, [Token])
parseDef toks = case toks of
    Token _ (TIdent n) : Token _ TLparen : rest -> do
        (params, rest') <- paramList [] rest
        (body, rest'') <- parseExprs isEnd rest'
        case rest'' of
            Token _ TEnd : rest''' -> pure (EDef n params body, rest''')
            _ -> Left "expected 'end' after def body"
    _ -> Left "expected 'def <name>(<params>)'"
  where
    isEnd TEnd = True
    isEnd _ = False

    paramList acc toks' = case toks' of
        Token _ TRparen : rest -> pure (reverse acc, rest)
        Token _ (TIdent p) : Token _ TComma : rest -> paramList (p : acc) rest
        Token _ (TIdent p) : Token _ TRparen : rest -> pure (reverse (p : acc), rest)
        _ -> Left "expected parameter or ')' in def"

arrayLiteral :: [Token] -> Either Text (Expr, [Token])
arrayLiteral toks = go [] toks
  where
    go acc toks' = case toks' of
        Token _ TRbracket : rest -> pure (EArray (reverse acc), rest)
        Token pos TEof : _ -> Left (parseErr pos "unterminated array literal")
        _ -> do
            (e, rest') <- parseBin 0 toks'
            case rest' of
                Token _ TComma : rest'' -> go (e : acc) rest''
                Token _ TRbracket : rest'' -> pure (EArray (reverse (e : acc)), rest'')
                Token pos TEof : _ -> Left (parseErr pos "unterminated array literal")
                _ -> Left "expected ',' or ']' in array literal"

braceLiteral :: [Token] -> Either Text (Expr, [Token])
braceLiteral toks = case toks of
    Token _ TRbrace : rest -> pure (EMap [], rest)
    Token _ TArrow : rest -> lambdaBody [] rest
    Token _ (TIdent p) : Token _ TArrow : rest -> lambdaBody [p] rest
    Token _ (TIdent p) : Token _ TComma : rest -> lambdaParams [p] rest
    _ -> mapLiteral [] toks
  where
    lambdaParams acc toks' = case toks' of
        Token _ (TIdent p) : Token _ TArrow : rest -> lambdaBody (reverse (p : acc)) rest
        Token _ (TIdent p) : Token _ TComma : rest -> lambdaParams (p : acc) rest
        _ -> Left "expected parameter or '->' in lambda"
    lambdaBody ps toks' = do
        (body, rest') <- parseExprs isRbrace toks'
        case rest' of
            Token _ TRbrace : rest'' -> pure (ELambda ps body, rest'')
            _ -> Left "expected '}' after lambda body"
      where
        isRbrace TRbrace = True
        isRbrace _ = False

mapLiteral :: [(Expr, Expr)] -> [Token] -> Either Text (Expr, [Token])
mapLiteral acc toks = do
    (k, rest') <- parseBin 0 toks
    case rest' of
        Token _ TArrowMap : rest'' -> do
            (v, rest''') <- parseBin 0 rest''
            case rest''' of
                Token _ TComma : rest'''' -> mapLiteral ((k, v) : acc) rest''''
                Token _ TRbrace : rest'''' -> pure (EMap (reverse ((k, v) : acc)), rest'''')
                _ -> Left "expected ',' or '}' in map literal"
        _ -> Left "expected '=>' in map literal"

ifExpr :: [Token] -> Either Text (Expr, [Token])
ifExpr toks = do
    (cond, rest) <- parseBin 0 toks
    case rest of
        Token _ TThen : rest' -> do
            (th, rest'') <- parseBin 0 rest'
            case rest'' of
                Token _ TElse : rest''' -> do
                    (el, rest'''') <- parseBin 0 rest'''
                    case rest'''' of
                        Token _ TEnd : rest''''' -> pure (EIf cond th (Just el), rest''''')
                        _ -> Left "expected 'end' after if"
                Token _ TEnd : rest''' -> pure (EIf cond th Nothing, rest''')
                _ -> Left "expected 'else' or 'end' in if"
        _ -> Left "expected 'then' in if"

postfix :: Expr -> [Token] -> Either Text (Expr, [Token])
postfix e toks = case toks of
    Token _ TLparen : rest -> do
        (args, rest') <- callArgs [] rest
        postfix (ECall e args) rest'
    _ -> pure (e, toks)

callArgs :: [Expr] -> [Token] -> Either Text ([Expr], [Token])
callArgs acc toks = case toks of
    Token _ TRparen : rest -> pure (reverse acc, rest)
    Token pos TEof : _ -> Left (parseErr pos "unterminated call")
    _ -> do
        (e, rest') <- parseBin 0 toks
        case rest' of
            Token _ TComma : rest'' -> callArgs (e : acc) rest''
            Token _ TRparen : rest'' -> pure (reverse (e : acc), rest'')
            _ -> Left "expected ',' or ')' in call"

describe :: TokenKind -> Text
describe k = case k of
    TNumber _ -> "number"
    TIdent _ -> "identifier"
    TString _ -> "string"
    TTrue -> "'true'"
    TFalse -> "'false'"
    TNil -> "'nil'"
    TDef -> "'def'"
    TEnd -> "'end'"
    TIf -> "'if'"
    TThen -> "'then'"
    TElse -> "'else'"
    TArrow -> "'->'"
    TArrowMap -> "'=>'"
    TLbrace -> "'{'"
    TRbrace -> "'}'"
    TLparen -> "'('"
    TRparen -> "')'"
    TLbracket -> "'['"
    TRbracket -> "']'"
    TComma -> "','"
    TOp op -> "'" <> opName op <> "'"
    TNot -> "'!'"
    TEof -> "end of input"

opName :: Op -> Text
opName op = case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
    Eq -> "=="
    Ne -> "!="
    Lt -> "<"
    Gt -> ">"
    Le -> "<="
    Ge -> ">="
    And -> "&&"
    Or -> "||"

parseErr :: (Int, Int) -> Text -> Text
parseErr (l, c) msg = "line " <> T.pack (show l) <> ", column " <> T.pack (show c) <> ": " <> msg
