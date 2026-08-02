module Expr (Chunk (..), Expr (..), Op (..)) where

import Data.Text (Text)

-- | A string literal with interpolated expressions, e.g. `"a #{x} b"`.
data Chunk
    = SLit Text
    | SInterp Expr
    deriving (Show, Eq)

data Op
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | Ne
    | Lt
    | Gt
    | Le
    | Ge
    | And
    | Or
    deriving (Show, Eq)

data Expr
    = ENumber Text
    | EString [Chunk]
    | EBool Bool
    | ENil
    | EIdent Text
    | EArray [Expr]
    | EMap [(Expr, Expr)]
    | ECall Expr [Expr]
    | ELambda [Text] [Expr]
    | EDef Text [Text] [Expr]
    | EIf Expr Expr (Maybe Expr)
    | EAnd Expr Expr
    | EOr Expr Expr
    | ENot Expr
    | EBinOp Op Expr Expr
    deriving (Show, Eq)
