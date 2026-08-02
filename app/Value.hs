module Value (Env, LangError (..), Value (..), FunValue (..), numericToText, showValue, strOf, valueToJson) where

import Data.Map.Strict qualified as Map
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Expr (Expr)

{- | A runtime error: a message and the call stack (innermost first),
e.g. ["map", "<lambda>", "main"].
-}
data LangError = LangError
    { leMsg :: Text
    , leStack :: [Text]
    }
    deriving (Show)

instance Eq LangError where
    a == b = leMsg a == leMsg b && leStack a == leStack b

type Env = Map.Map Text Value

{- | A user-defined closure: name (for recursion, Nothing for lambdas),
parameters, body and the captured definition-time environment.
-}
data FunValue = FunValue
    { fName :: Maybe Text
    , fParams :: [Text]
    , fBody :: [Expr]
    , fEnv :: Env
    }
    deriving (Eq)

data Value
    = VNum Scientific
    | VStr Text
    | VBool Bool
    | VNil
    | VArr (V.Vector Value)
    | VMap (Map.Map Text Value)
    | VFun FunValue
    | VNative Text ([Value] -> Either LangError (Value, [Text]))

instance Eq Value where
    VNum a == VNum b = a == b
    VStr a == VStr b = a == b
    VBool a == VBool b = a == b
    VNil == VNil = True
    VArr a == VArr b = a == b
    VMap a == VMap b = a == b
    VFun a == VFun b = a == b
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
    VArr vs -> T.concat ["[", T.intercalate ", " (map showValue (V.toList vs)), "]"]
    VMap m -> T.concat ["{", T.intercalate ", " (map (\(k, x) -> showValue (VStr k) <> " => " <> showValue x) (Map.toList m)), "}"]
    VFun f -> "<function " <> maybe "lambda" id (fName f) <> ">"
    VNative n _ -> "<function " <> n <> ">"

{- | The plain text rendering of a value for string interpolation and
string operations. Collections and functions are not convertible.
-}
strOf :: Value -> Either Text Text
strOf v = case v of
    VNum s -> Right (numericToText s)
    VStr t -> Right t
    VBool b -> Right (if b then "true" else "false")
    VNil -> Right "nil"
    _ -> Left ("cannot convert " <> showValue v <> " to string")

{- | Pretty JSON (two-space indent) for `toJson`. Map keys are in
alphabetical order. Functions are not serialisable.
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
        VArr vs -> case V.toList vs of
            [] -> Right "[]"
            items -> do
                parts <- mapM (go (depth + 1)) items
                Right ("[\n" <> indent (depth + 1) <> T.intercalate (",\n" <> indent (depth + 1)) parts <> "\n" <> indent depth <> "]")
        VMap m -> case Map.toList m of
            [] -> Right "{}"
            entries -> do
                parts <- mapM (entry (depth + 1)) entries
                Right ("{\n" <> indent (depth + 1) <> T.intercalate (",\n" <> indent (depth + 1)) parts <> "\n" <> indent depth <> "}")
        VFun _ -> Left "cannot serialize a function"
        VNative _ _ -> Left "cannot serialize a function"
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
