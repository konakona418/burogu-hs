{- | The language's standard library: string, array and map operations,
plus @puts@ (collected to stderr by the host) and the HTML tag
constructors. There is no I/O and no mutation; `map`, `filter` and
`reduce` take functions.
-}
module Script.Builtins (initialEnv, resetGensym) where

import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator)
import Data.Scientific (Scientific, toBoundedInteger)
import Data.Scientific qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, fromGregorianValid, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

import Script.Eval (applyValue, truthy)
import Script.Html (htmlBuiltins)
import Script.Value (Env, LangError (..), Value (..), numericToText, showValue, strOf, valueToJson)

-- | The full builtin environment: standard library plus HTML tags.
initialEnv :: Env
initialEnv = Map.fromList (coreBuiltins <> htmlBuiltins)

coreBuiltins :: [(Text, Value)]
coreBuiltins =
    [ native "puts" putsFn
    , native "+" plusFn
    , native "-" minusFn
    , native "*" timesFn
    , native "/" divideFn
    , native "%" moduloFn
    , native "==" (eqFn "==" (==))
    , native "!=" (eqFn "!=" (/=))
    , native "<" (cmpFn "<" (<))
    , native ">" (cmpFn ">" (>))
    , native "<=" (cmpFn "<=" (<=))
    , native ">=" (cmpFn ">=" (>=))
    , native "not" notFn
    , native "len" (b1 "len" lenOf)
    , native "get" (b2 "get" getOf)
    , native "at" (b2 "at" atOf)
    , native "append" (b2 "append" appendOf)
    , native "concat" (b2 "concat" concatOf)
    , native "join" (b2 "join" joinOf)
    , native "split" (b2 "split" splitOf)
    , native "map" (b2 "map" mapOf)
    , native "filter" (b2 "filter" filterOf)
    , native "reduce" (b3 "reduce" reduceOf)
    , native "sort" (b1 "sort" sortOf)
    , native "reverse" (b1 "reverse" reverseOf)
    , native "first" (b1 "first" firstOf)
    , native "last" (b1 "last" lastOf)
    , native "keys" (b1 "keys" keysOf)
    , native "values" (b1 "values" valuesOf)
    , native "contains" (b2 "contains" containsOf)
    , native "trim" (strFn "trim" T.strip)
    , native "lower" (strFn "lower" T.toLower)
    , native "upper" (strFn "upper" T.toUpper)
    , native "replace" (b3 "replace" replaceOf)
    , native "take" (b2 "take" takeOf)
    , native "drop" (b2 "drop" dropOf)
    , native "str" strVarFn
    , native "to-json" (b1 "to-json" $ \v -> either (Left . msg) (Right . VStr) (valueToJson v))
    , native "format-date" (b2 "format-date" formatDateOf)
    , native "gensym" gensymFn
    ]
  where
    native n f = (n, VNative n f)

ok :: Value -> Either LangError (Value, [Text])
ok v = Right (v, [])

b1 :: Text -> (Value -> Either LangError Value) -> [Value] -> Either LangError (Value, [Text])
b1 name f args = case args of
    [v] -> ok =<< f v
    _ -> arity name 1 args

b2 :: Text -> (Value -> Value -> Either LangError (Value, [Text])) -> [Value] -> Either LangError (Value, [Text])
b2 name f args = case args of
    [a, b] -> f a b
    _ -> arity name 2 args

b3 :: Text -> (Value -> Value -> Value -> Either LangError (Value, [Text])) -> [Value] -> Either LangError (Value, [Text])
b3 name f args = case args of
    [a, b, c] -> f a b c
    _ -> arity name 3 args

strFn :: Text -> (Text -> Text) -> [Value] -> Either LangError (Value, [Text])
strFn name f args = case args of
    [v] -> either (Left . msg) (ok . VStr . f) (strOf v)
    _ -> arity name 1 args

putsFn :: [Value] -> Either LangError (Value, [Text])
putsFn args = (VNil,) <$> mapM (either (Left . msg) Right . strOf) args

plusFn :: [Value] -> Either LangError (Value, [Text])
plusFn args = do
    ns <- nums "+" args
    ok (VNum (sum ns))

timesFn :: [Value] -> Either LangError (Value, [Text])
timesFn args = do
    ns <- nums "*" args
    ok (VNum (product ns))

minusFn :: [Value] -> Either LangError (Value, [Text])
minusFn args = case args of
    [] -> arity "-" 1 args
    [a] -> do
        n <- num "-" a
        ok (VNum (negate n))
    (a : rest) -> do
        n <- num "-" a
        ns <- nums "-" rest
        ok (VNum (foldl (-) n ns))

divideFn :: [Value] -> Either LangError (Value, [Text])
divideFn args = case args of
    [] -> arity "/" 1 args
    [a] -> do
        n <- num "/" a
        if n == 0 then Left (msg "division by zero") else ok (VNum (divSci 1 n))
    (a : rest) -> do
        n <- num "/" a
        ns <- nums "/" rest
        v <- foldM step n ns
        ok (VNum v)
  where
    step acc d = if d == 0 then Left (msg "division by zero") else Right (divSci acc d)

moduloFn :: [Value] -> Either LangError (Value, [Text])
moduloFn args = case args of
    [a, b] -> do
        n <- num "%" a
        m <- num "%" b
        if m == 0 then Left (msg "division by zero") else ok (VNum (modSci n m))
    _ -> arity "%" 2 args

cmpFn :: Text -> (Scientific -> Scientific -> Bool) -> [Value] -> Either LangError (Value, [Text])
cmpFn name f args = case args of
    [a, b] -> do
        n <- num name a
        m <- num name b
        ok (VBool (f n m))
    _ -> arity name 2 args

-- | General equality: any two values can be compared (functions are
-- never equal).
eqFn :: Text -> (Value -> Value -> Bool) -> [Value] -> Either LangError (Value, [Text])
eqFn name f args = case args of
    [a, b] -> ok (VBool (f a b))
    _ -> arity name 2 args

notFn :: [Value] -> Either LangError (Value, [Text])
notFn args = case args of
    [v] -> ok (VBool (not (truthy v)))
    _ -> arity "not" 1 args

strVarFn :: [Value] -> Either LangError (Value, [Text])
strVarFn args = ok . VStr . T.concat =<< mapM (either (Left . msg) Right . strOf) args

num :: Text -> Value -> Either LangError Scientific
num _ (VNum s) = Right s
num name v = Left (typeError name "a number" [v])

nums :: Text -> [Value] -> Either LangError [Scientific]
nums name = mapM (num name)

divSci :: Scientific -> Scientific -> Scientific
divSci m n =
    let r = toRational m / toRational n
     in if denominator r == 1
            then fromRational r
            else fromRational (toRational ((S.toRealFloat m / S.toRealFloat n) :: Double))

modSci :: Scientific -> Scientific -> Scientific
modSci m n =
    let q = truncate (toRational m / toRational n) :: Integer
     in m - fromRational (toRational q) * n

arity :: Text -> Int -> [Value] -> Either LangError (Value, [Text])
arity name n args = Left (msg (name <> ": expected " <> T.pack (show n) <> " argument(s), got " <> T.pack (show (length args))))

lenOf :: Value -> Either LangError Value
lenOf v = case v of
    VStr s -> Right (VNum (fromIntegral (T.length s)))
    VArr vs -> Right (VNum (fromIntegral (V.length vs)))
    VMap m -> Right (VNum (fromIntegral (length m)))
    _ -> Left (typeError "len" "a string, array or map" [v])

getOf :: Value -> Value -> Either LangError (Value, [Text])
getOf m k = case (m, k) of
    (VMap mp, VStr key) -> ok (Map.findWithDefault VNil key (Map.fromList mp))
    _ -> Left (typeError "get" "a map and a string key" [m, k])

-- | Element access: `(at arr i)` for arrays and strings.
atOf :: Value -> Value -> Either LangError (Value, [Text])
atOf a i = case (a, i) of
    (VArr vs, VNum n) -> case toBoundedInteger n of
        Just j | j >= 0 && j < V.length vs -> ok (vs V.! j)
        _ -> Left (msg ("index " <> numericToText n <> " out of bounds (length " <> T.pack (show (V.length vs)) <> ")"))
    (VStr s, VNum n) -> case toBoundedInteger n of
        Just j | j >= 0 && j < T.length s -> ok (VStr (T.singleton (T.index s j)))
        _ -> Left (msg ("index " <> numericToText n <> " out of bounds (length " <> T.pack (show (T.length s)) <> ")"))
    _ -> Left (typeError "at" "an array or string and a number" [a, i])

appendOf :: Value -> Value -> Either LangError (Value, [Text])
appendOf a v = case a of
    VArr vs -> ok (VArr (vs <> V.singleton v))
    _ -> Left (typeError "append" "an array" [a])

concatOf :: Value -> Value -> Either LangError (Value, [Text])
concatOf a b = case (a, b) of
    (VArr xs, VArr ys) -> ok (VArr (xs <> ys))
    (VStr x, VStr y) -> ok (VStr (x <> y))
    _ -> Left (typeError "concat" "two arrays or two strings" [a, b])

joinOf :: Value -> Value -> Either LangError (Value, [Text])
joinOf a s = case (a, s) of
    (VArr vs, VStr sep) -> do
        ts <- mapM (either (Left . msg) Right . strOf) (V.toList vs)
        ok (VStr (T.intercalate sep ts))
    _ -> Left (typeError "join" "an array and a string separator" [a, s])

splitOf :: Value -> Value -> Either LangError (Value, [Text])
splitOf s sep = case (s, sep) of
    (VStr x, VStr y)
        | T.null y -> Left (typeError "split" "a non-empty separator" [sep])
        | otherwise -> ok (VArr (V.fromList (map VStr (T.splitOn y x))))
    _ -> Left (typeError "split" "two strings" [s, sep])

mapOf :: Value -> Value -> Either LangError (Value, [Text])
mapOf arr fn = case arr of
    VArr vs -> do
        (results, outs) <- foldM step ([], []) (V.toList vs)
        pure (VArr (V.fromList (reverse results)), outs)
    _ -> Left (typeError "map" "an array and a function" [arr, fn])
  where
    step (acc, outs) v = do
        (r, o) <- applyValue Map.empty fn [v]
        pure (r : acc, outs <> o)

filterOf :: Value -> Value -> Either LangError (Value, [Text])
filterOf arr fn = case arr of
    VArr vs -> do
        (keep, outs) <- foldM step ([], []) (V.toList vs)
        pure (VArr (V.fromList (reverse keep)), outs)
    _ -> Left (typeError "filter" "an array and a function" [arr, fn])
  where
    step (acc, outs) v = do
        (r, o) <- applyValue Map.empty fn [v]
        pure (if truthy r then v : acc else acc, outs <> o)

reduceOf :: Value -> Value -> Value -> Either LangError (Value, [Text])
reduceOf arr fn init0 = case arr of
    VArr vs -> foldM step (init0, []) (V.toList vs)
    _ -> Left (typeError "reduce" "an array, a function and an initial value" [arr, fn, init0])
  where
    step (acc, outs) v = do
        (r, o) <- applyValue Map.empty fn [acc, v]
        pure (r, outs <> o)

sortOf :: Value -> Either LangError Value
sortOf a = case a of
    VArr vs -> do
        sorted <- sortByM compareVs (V.toList vs)
        Right (VArr (V.fromList sorted))
    _ -> Left (typeError "sort" "an array of numbers or strings" [a])

sortByM :: (a -> a -> Either LangError Ordering) -> [a] -> Either LangError [a]
sortByM cmp = foldM insert []
  where
    insert [] x = Right [x]
    insert (y : ys) x = do
        c <- cmp x y
        case c of
            GT -> (y :) <$> insert ys x
            _ -> Right (x : y : ys)

compareVs :: Value -> Value -> Either LangError Ordering
compareVs x y = case (x, y) of
    (VNum n1, VNum n2) -> Right (compare n1 n2)
    (VStr s1, VStr s2) -> Right (compare s1 s2)
    _ -> Left (msg ("cannot sort mixed values: " <> showValue x <> " vs " <> showValue y))

reverseOf :: Value -> Either LangError Value
reverseOf a = case a of
    VArr vs -> Right (VArr (V.reverse vs))
    _ -> Left (typeError "reverse" "an array" [a])

firstOf :: Value -> Either LangError Value
firstOf a = case a of
    VArr vs | V.null vs -> Right VNil
    VArr vs -> Right (V.head vs)
    _ -> Left (typeError "first" "an array" [a])

lastOf :: Value -> Either LangError Value
lastOf a = case a of
    VArr vs | V.null vs -> Right VNil
    VArr vs -> Right (V.last vs)
    _ -> Left (typeError "last" "an array" [a])

keysOf :: Value -> Either LangError Value
keysOf m = case m of
    VMap mp -> Right (VArr (V.fromList (map (VStr . fst) mp)))
    _ -> Left (typeError "keys" "a map" [m])

valuesOf :: Value -> Either LangError Value
valuesOf m = case m of
    VMap mp -> Right (VArr (V.fromList (map snd mp)))
    _ -> Left (typeError "values" "a map" [m])

containsOf :: Value -> Value -> Either LangError (Value, [Text])
containsOf a v = case (a, v) of
    (VStr s, VStr sub) -> ok (VBool (T.isInfixOf sub s))
    (VArr vs, _) -> ok (VBool (v `V.elem` vs))
    _ -> Left (typeError "contains" "a string or array" [a])

replaceOf :: Value -> Value -> Value -> Either LangError (Value, [Text])
replaceOf s from to = case (s, from, to) of
    (VStr x, VStr f, VStr t)
        | T.null f -> Left (typeError "replace" "a non-empty pattern" [from])
        | otherwise -> ok (VStr (T.replace f t x))
    _ -> Left (typeError "replace" "three strings" [s, from, to])

takeOf :: Value -> Value -> Either LangError (Value, [Text])
takeOf c n = case (c, n) of
    (VArr vs, VNum k) -> ok (VArr (V.take (clamp k) vs))
    (VStr s, VNum k) -> ok (VStr (T.take (clamp k) s))
    _ -> Left (typeError "take" "an array or string and a number" [c, n])
  where
    clamp k = max 0 (fromInteger (truncate k))

dropOf :: Value -> Value -> Either LangError (Value, [Text])
dropOf c n = case (c, n) of
    (VArr vs, VNum k) -> ok (VArr (V.drop (clamp k) vs))
    (VStr s, VNum k) -> ok (VStr (T.drop (clamp k) s))
    _ -> Left (typeError "drop" "an array or string and a number" [c, n])
  where
    clamp k = max 0 (fromInteger (truncate k))

{- | strftime-style date formatting for an ISO date (YYYY-MM-DD).
Directives: %Y %y %m %d %b %B %a %A %% and %-m/%-d (no zero pad).
Unknown directives are errors.
-}
formatDateOf :: Value -> Value -> Either LangError (Value, [Text])
formatDateOf d f = case (d, f) of
    (VStr date, VStr fmt) -> do
        day <- either (Left . msg) Right (parseIsoDate date)
        formatted <- either (Left . msg) Right (formatWith fmt day)
        ok (VStr formatted)
    _ -> Left (typeError "format-date" "a date string and a format string" [d, f])

parseIsoDate :: Text -> Either Text Day
parseIsoDate t = case T.splitOn "-" t of
    [y, m, d] -> case (readMaybe (T.unpack y) :: Maybe Integer, readMaybe (T.unpack m) :: Maybe Int, readMaybe (T.unpack d) :: Maybe Int) of
        (Just yy, Just mm, Just dd) -> maybe (Left ("invalid date '" <> t <> "'")) Right (fromGregorianValid yy mm dd)
        _ -> Left ("invalid date '" <> t <> "'")
    _ -> Left ("invalid date '" <> t <> "'")

formatWith :: Text -> Day -> Either Text Text
formatWith fmt day = go (T.unpack fmt)
  where
    (y, m, d) = toGregorian day

    go [] = Right ""
    go ('%' : c : rest) = case c of
        'Y' -> (T.pack (show y) <>) <$> go rest
        'y' -> (pad2 (fromInteger (y `mod` 100) :: Int) <>) <$> go rest
        'm' -> (pad2 m <>) <$> go rest
        'd' -> (pad2 d <>) <$> go rest
        '-' -> case rest of
            'm' : rest' -> (T.pack (show m) <>) <$> go rest'
            'd' : rest' -> (T.pack (show d) <>) <$> go rest'
            c' : _ -> Left ("unsupported format directive '%-" <> T.singleton c' <> "'")
            [] -> Left "unterminated format directive"
        'b' -> (shortMonth <>) <$> go rest
        'B' -> (longMonth <>) <$> go rest
        'a' -> (shortWeek <>) <$> go rest
        'A' -> (longWeek <>) <$> go rest
        '%' -> ("%" <>) <$> go rest
        c' -> Left ("unsupported format directive '%" <> T.singleton c' <> "'")
    go ('%' : []) = Left "unterminated format directive"
    go (c : rest) = (T.singleton c <>) <$> go rest

    pad2 n = if n < 10 then "0" <> T.pack (show n) else T.pack (show n)
    shortMonth = T.pack (formatTime defaultTimeLocale "%b" day)
    longMonth = T.pack (formatTime defaultTimeLocale "%B" day)
    shortWeek = T.pack (formatTime defaultTimeLocale "%a" day)
    longWeek = T.pack (formatTime defaultTimeLocale "%A" day)

-- | Collect `puts` output while passing a value through.
msg :: Text -> LangError
msg t = LangError t [] Nothing

{- | A fresh symbol for non-hygienic macros. The counter is global to
the process; scripts are build-time only, so this is safe. The action
forces `args` so GHC cannot float the counter out into a shared
constant (which would make every call return the same symbol).
-}
gensymFn :: [Value] -> Either LangError (Value, [Text])
gensymFn args = unsafePerformIO $ do
    _ <- evaluate (length args)
    n <- atomicModifyIORef' gensymCounter (\i -> (i + 1, i))
    pure $ case args of
        [] -> ok (VSym ("G__" <> T.pack (show n)))
        [VStr prefix] -> ok (VSym (prefix <> "G__" <> T.pack (show n)))
        [other] -> Left (typeError "gensym" "a string prefix" [other])
        _ -> arity "gensym" 0 args

{-# NOINLINE gensymFn #-}

{-# NOINLINE gensymCounter #-}
gensymCounter :: IORef Int
gensymCounter = unsafePerformIO (newIORef 0)

-- | Reset the `gensym` counter so a build run is reproducible.
resetGensym :: IO ()
resetGensym = writeIORef gensymCounter 0

typeError :: Text -> Text -> [Value] -> LangError
typeError name want args =
    LangError ("cannot " <> name <> ": expected " <> want <> ", got " <> T.intercalate ", " (map showValue args)) [] Nothing
