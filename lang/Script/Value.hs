{- | Runtime values of the script language: numbers, strings, booleans,
nil, symbols, arrays, maps, closures, native functions and structured
HTML nodes. HTML is a first-class value, so templates build a tree that
the renderer turns into escaped, well-formed markup.
-}
module Script.Value (
    Attr,
    Env,
    Fun (..),
    Html (..),
    LangError (..),
    Macro (..),
    Value (..),
    numericToText,
    showValue,
    strOf,
    valueToJson,
) where

import Data.Map.Strict (Map)
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V

import Script.Form (Form)

type Env = Map Text Value

{- | A runtime error: a message, the call stack (innermost first) and
the source position of the form being evaluated, when known.
-}
data LangError = LangError
    { leMsg :: Text
    , leStack :: [Text]
    , lePos :: Maybe (Int, Int)
    }
    deriving (Show)

instance Eq LangError where
    a == b = leMsg a == leMsg b && leStack a == leStack b

{- | A user-defined closure: its name (used for recursion and stack
traces), parameters, body and the captured definition environment.
-}
data Fun = Fun
    { funName :: Maybe Text
    , funParams :: [Text]
    , funBody :: [Form]
    , funEnv :: Env
    }
    deriving (Eq)

{- | A macro: like a closure, but its arguments are the unevaluated
source forms (as data) and its body returns the expansion (also as
data). Non-hygienic; use `gensym` to avoid capture.
-}
data Macro = Macro
    { macroName :: Maybe Text
    , macroParams :: [Text]
    , macroBody :: [Form]
    , macroEnv :: Env
    }
    deriving (Eq)

{- | An attribute value: `true` renders a bare attribute, `false` and
`nil` omit it, anything else renders as an escaped value.
-}
type Attr = Value

{- | A structured HTML node: an element with attributes and children,
or a raw, unescaped fragment (used to inject already-rendered bodies
and embedded scripts).
-}
data Html
    = HElem Text [(Text, Value)] [Value]
    | HRaw Text
    deriving (Eq)

data Value
    = VNum Scientific
    | VStr Text
    | VBool Bool
    | VNil
    | VSym Text
    | VArr (Vector Value)
    | VMap [(Text, Value)]
    | VFun Fun
    | VNative Text ([Value] -> Either LangError (Value, [Text]))
    | VMacro Macro
    | VHtml Html

instance Eq Value where
    VNum a == VNum b = a == b
    VStr a == VStr b = a == b
    VBool a == VBool b = a == b
    VNil == VNil = True
    VSym a == VSym b = a == b
    VArr a == VArr b = a == b
    VMap a == VMap b = a == b
    VFun a == VFun b = a == b
    VMacro a == VMacro b = a == b
    VHtml a == VHtml b = a == b
    _ == _ = False

-- | Format a number without a trailing decimal part for whole numbers.
numericToText :: Scientific -> Text
numericToText s = case floatingOrInteger s of
    Right i -> T.pack (show (i :: Integer))
    Left d -> T.pack (show (d :: Double))

{- | A debug rendering of a value, used in error messages (strings are
quoted).
-}
showValue :: Value -> Text
showValue v = case v of
    VNum s -> numericToText s
    VStr t -> T.concat ["\"", t, "\""]
    VBool b -> if b then "true" else "false"
    VNil -> "nil"
    VSym s -> s
    VArr vs -> T.concat ["[", T.intercalate " " (map showValue (V.toList vs)), "]"]
    VMap m -> T.concat ["{", T.intercalate " " (map (\(k, x) -> ":" <> k <> " " <> showValue x) m), "}"]
    VFun f -> "<function " <> maybe "lambda" id (funName f) <> ">"
    VNative n _ -> "<function " <> n <> ">"
    VMacro m -> "<macro " <> maybe "lambda" id (macroName m) <> ">"
    VHtml _ -> "<html>"

{- | The plain text rendering of a value for string interpolation and
string operations. Collections, functions and HTML are not convertible.
-}
strOf :: Value -> Either Text Text
strOf v = case v of
    VNum s -> Right (numericToText s)
    VStr t -> Right t
    VBool b -> Right (if b then "true" else "false")
    VNil -> Right "nil"
    _ -> Left ("cannot convert " <> showValue v <> " to string")

{- | Pretty JSON (two-space indent) for `to-json`. Map keys are in
alphabetical order. Functions and HTML are not serialisable.
-}
valueToJson :: Value -> Either Text Text
valueToJson = go 0
  where
    go :: Int -> Value -> Either Text Text
    go depth x = case x of
        VNum s -> Right (numericToText s)
        VStr t -> Right (jsonString t)
        VBool b -> Right (if b then "true" else "false")
        VNil -> Right "null"
        VSym s -> Right (jsonString s)
        VArr vs -> case V.toList vs of
            [] -> Right "[]"
            items -> do
                parts <- mapM (go (depth + 1)) items
                Right ("[\n" <> indent (depth + 1) <> T.intercalate (",\n" <> indent (depth + 1)) parts <> "\n" <> indent depth <> "]")
        VMap m -> case m of
            [] -> Right "{}"
            entries -> do
                parts <- mapM (entry (depth + 1)) entries
                Right ("{\n" <> indent (depth + 1) <> T.intercalate (",\n" <> indent (depth + 1)) parts <> "\n" <> indent depth <> "}")
        VFun _ -> Left "cannot serialize a function"
        VNative _ _ -> Left "cannot serialize a function"
        VMacro _ -> Left "cannot serialize a macro"
        VHtml _ -> Left "cannot serialize html"
      where
        entry d (k, val) = do
            j <- go d val
            Right (jsonString k <> ": " <> j)

    indent :: Int -> Text
    indent n = T.replicate n "  "

    jsonString :: Text -> Text
    jsonString t =
        "\""
            <> T.concatMap escape t
            <> "\""
      where
        escape c = case c of
            '"' -> "\\\""
            '\\' -> "\\\\"
            '\n' -> "\\n"
            '\t' -> "\\t"
            '\r' -> "\\r"
            _ -> T.singleton c
