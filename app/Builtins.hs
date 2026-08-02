module Builtins (initialEnv) where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Eval (applyValueOut, truthy)
import Value (Env, LangError (..), Value (..), numericToText, showValue, strOf)

{- | The builtin function table. `puts` collects its output (returned
in the output channel) instead of printing.
-}
initialEnv :: Env
initialEnv = Map.fromList $ map (\(n, f) -> (n, VNative n f)) builtins

builtins :: [(Text, [Value] -> Either LangError (Value, [Text]))]
builtins =
    [ ("puts", \args -> (VNil,) <$> mapM (either (Left . msg) Right . strOf) args)
    , ("len", b1 "len" $ \v -> VNum . fromIntegral <$> lenOf v)
    , ("at", b2 "at" atOf)
    , ("get", b2 "get" getOf)
    , ("append", b2 "append" appendOf)
    , ("concat", b2 "concat" concatOf)
    , ("join", b2 "join" joinOf)
    , ("split", b2 "split" splitOf)
    , ("map", b2 "map" mapOf)
    , ("filter", b2 "filter" filterOf)
    , ("sort", b1 "sort" sortOf)
    , ("reverse", b1 "reverse" reverseOf)
    , ("first", b1 "first" firstOf)
    , ("last", b1 "last" lastOf)
    , ("keys", b1 "keys" keysOf)
    , ("values", b1 "values" valuesOf)
    , ("toStr", b1 "toStr" $ \v -> either (Left . msg) (Right . VStr) (strOf v))
    , ("contains", b2 "contains" containsOf)
    , ("trim", b1 "trim" $ \v -> either (Left . msg) (Right . VStr . T.strip) (strOf v))
    , ("lower", b1 "lower" $ \v -> either (Left . msg) (Right . VStr . T.toLower) (strOf v))
    , ("upper", b1 "upper" $ \v -> either (Left . msg) (Right . VStr . T.toUpper) (strOf v))
    , ("replace", b3 "replace" replaceOf)
    , ("take", b2 "take" takeOf)
    , ("drop", b2 "drop" dropOf)
    ]

ok :: Value -> Either LangError (Value, [Text])
ok v = Right (v, [])

b1 :: Text -> (Value -> Either LangError Value) -> [Value] -> Either LangError (Value, [Text])
b1 name f args = case args of
    [v] -> ok =<< f v
    _ -> arityErr name 1 args

b2 :: Text -> (Value -> Value -> Either LangError (Value, [Text])) -> [Value] -> Either LangError (Value, [Text])
b2 name f args = case args of
    [a, b] -> f a b
    _ -> arityErr name 2 args

b3 :: Text -> (Value -> Value -> Value -> Either LangError (Value, [Text])) -> [Value] -> Either LangError (Value, [Text])
b3 name f args = case args of
    [a, b, c] -> f a b c
    _ -> arityErr name 3 args

arityErr :: Text -> Int -> [Value] -> Either LangError (Value, [Text])
arityErr name n args = Left (msg (name <> ": expected " <> T.pack (show n) <> " argument(s), got " <> T.pack (show (length args))))

lenOf :: Value -> Either LangError Int
lenOf v = case v of
    VStr s -> Right (T.length s)
    VArr vs -> Right (V.length vs)
    VMap m -> Right (Map.size m)
    _ -> typeErr "len" ("string, array or map" <> ", got " <> showValue v)

getOf :: Value -> Value -> Either LangError (Value, [Text])
getOf m k = case (m, k) of
    (VMap mp, VStr key) -> ok (Map.findWithDefault VNil key mp)
    _ -> typeErr "get" ("map and string key" <> ", got " <> showValue m <> " and " <> showValue k)

{- | Element access: `at arr 0` (arrays and strings). Negative or
out-of-range indices are errors.
-}
atOf :: Value -> Value -> Either LangError (Value, [Text])
atOf a i = case (a, i) of
    (VArr vs, VNum n) -> case toBoundedInteger n of
        Just j
            | j >= 0 && j < V.length vs -> ok (vs V.! j)
        _ -> Left (msg ("index " <> numericToText n <> " out of bounds (length " <> T.pack (show (V.length vs)) <> ")"))
    (VStr s, VNum n) -> case toBoundedInteger n of
        Just j
            | j >= 0 && j < T.length s -> ok (VStr (T.singleton (T.index s j)))
        _ -> Left (msg ("index " <> numericToText n <> " out of bounds (length " <> T.pack (show (T.length s)) <> ")"))
    _ -> typeErr "at" ("array or string and number" <> ", got " <> showValue a <> " and " <> showValue i)

appendOf :: Value -> Value -> Either LangError (Value, [Text])
appendOf a v = case a of
    VArr vs -> ok (VArr (vs <> V.singleton v))
    _ -> typeErr "append" ("array" <> ", got " <> showValue a)

concatOf :: Value -> Value -> Either LangError (Value, [Text])
concatOf a b = case (a, b) of
    (VArr xs, VArr ys) -> ok (VArr (xs <> ys))
    (VStr x, VStr y) -> ok (VStr (x <> y))
    _ -> typeErr "concat" ("array or string" <> ", got " <> showValue a <> " and " <> showValue b)

joinOf :: Value -> Value -> Either LangError (Value, [Text])
joinOf a s = case (a, s) of
    (VArr vs, VStr sep) -> do
        ts <- mapM (either (Left . msg) Right . strOf) (V.toList vs)
        ok (VStr (T.intercalate sep ts))
    _ -> typeErr "join" ("array and string separator" <> ", got " <> showValue a <> " and " <> showValue s)

splitOf :: Value -> Value -> Either LangError (Value, [Text])
splitOf s sep = case (s, sep) of
    (VStr x, VStr y)
        | T.null y -> typeErr "split" ("non-empty separator" <> ", got \"\"")
        | otherwise -> ok (VArr (V.fromList (map VStr (T.splitOn y x))))
    _ -> typeErr "split" ("string and string separator" <> ", got " <> showValue s <> " and " <> showValue sep)

mapOf :: Value -> Value -> Either LangError (Value, [Text])
mapOf arr fn = case arr of
    VArr vs -> do
        (results, outs) <- foldM step ([], []) (V.toList vs)
        pure (VArr (V.fromList (reverse results)), outs)
    _ -> typeErr "map" ("array and function" <> ", got " <> showValue arr)
  where
    step (acc, outs) v = do
        (r, o) <- applyValueOut Map.empty fn [v]
        pure (r : acc, outs <> o)

filterOf :: Value -> Value -> Either LangError (Value, [Text])
filterOf arr fn = case arr of
    VArr vs -> do
        (keep, outs) <- foldM step ([], []) (V.toList vs)
        pure (VArr (V.fromList (reverse keep)), outs)
    _ -> typeErr "filter" ("array and function" <> ", got " <> showValue arr)
  where
    step (acc, outs) v = do
        (r, o) <- applyValueOut Map.empty fn [v]
        pure (if truthy r then v : acc else acc, outs <> o)

sortOf :: Value -> Either LangError Value
sortOf a = case a of
    VArr vs -> do
        sorted <- quickSort (V.toList vs)
        Right (VArr (V.fromList sorted))
    _ -> typeErr "sort" ("array of numbers or strings" <> ", got " <> showValue a)
  where
    quickSort [] = Right []
    quickSort (x : xs) = do
        (smaller, bigger) <- partitionLT x xs
        small <- quickSort smaller
        big <- quickSort bigger
        Right (small <> [x] <> big)
    partitionLT _ [] = Right ([], [])
    partitionLT x (y : ys) = do
        c <- compareVs y x
        (s, b) <- partitionLT x ys
        Right (if c == LT then (y : s, b) else (s, y : b))
    compareVs x y = case (x, y) of
        (VNum n1, VNum n2) -> Right (compare n1 n2)
        (VStr s1, VStr s2) -> Right (compare s1 s2)
        _ -> Left (msg ("cannot sort mixed values: " <> showValue x <> " vs " <> showValue y))

reverseOf :: Value -> Either LangError Value
reverseOf a = case a of
    VArr vs -> Right (VArr (V.reverse vs))
    _ -> typeErr "reverse" ("array" <> ", got " <> showValue a)

firstOf :: Value -> Either LangError Value
firstOf a = case a of
    VArr vs
        | V.null vs -> Right VNil
        | otherwise -> Right (V.head vs)
    _ -> typeErr "first" ("array" <> ", got " <> showValue a)

lastOf :: Value -> Either LangError Value
lastOf a = case a of
    VArr vs
        | V.null vs -> Right VNil
        | otherwise -> Right (V.last vs)
    _ -> typeErr "last" ("array" <> ", got " <> showValue a)

keysOf :: Value -> Either LangError Value
keysOf m = case m of
    VMap mp -> Right (VArr (V.fromList (map (VStr . fst) (Map.toList mp))))
    _ -> typeErr "keys" ("map" <> ", got " <> showValue m)

valuesOf :: Value -> Either LangError Value
valuesOf m = case m of
    VMap mp -> Right (VArr (V.fromList (map snd (Map.toList mp))))
    _ -> typeErr "values" ("map" <> ", got " <> showValue m)

containsOf :: Value -> Value -> Either LangError (Value, [Text])
containsOf a v = case (a, v) of
    (VStr s, VStr sub) -> ok (VBool (T.isInfixOf sub s))
    (VArr vs, _) -> ok (VBool (v `V.elem` vs))
    _ -> typeErr "contains" ("string or array" <> ", got " <> showValue a)

replaceOf :: Value -> Value -> Value -> Either LangError (Value, [Text])
replaceOf s from to = case (s, from, to) of
    (VStr x, VStr f, VStr t)
        | T.null f -> typeErr "replace" ("non-empty pattern" <> ", got \"\"")
        | otherwise -> ok (VStr (T.replace f t x))
    _ -> typeErr "replace" ("string, string pattern and string replacement" <> ", got " <> showValue s <> ", " <> showValue from <> " and " <> showValue to)

takeOf :: Value -> Value -> Either LangError (Value, [Text])
takeOf c n = case (c, n) of
    (VArr vs, VNum k) -> ok (VArr (V.take (clamp k) vs))
    (VStr s, VNum k) -> ok (VStr (T.take (clamp k) s))
    _ -> typeErr "take" ("array or string and number" <> ", got " <> showValue c <> " and " <> showValue n)
  where
    clamp k = max 0 (fromInteger (truncate k))

dropOf :: Value -> Value -> Either LangError (Value, [Text])
dropOf c n = case (c, n) of
    (VArr vs, VNum k) -> ok (VArr (V.drop (clamp k) vs))
    (VStr s, VNum k) -> ok (VStr (T.drop (clamp k) s))
    _ -> typeErr "drop" ("array or string and number" <> ", got " <> showValue c <> " and " <> showValue n)
  where
    clamp k = max 0 (fromInteger (truncate k))

typeErr :: Text -> Text -> Either LangError a
typeErr name want = Left (msg ("cannot " <> name <> ": expected " <> want))

msg :: Text -> LangError
msg t = LangError t []
