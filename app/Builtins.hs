module Builtins (initialEnv) where

import Control.Monad (foldM)
import Data.Map.Strict qualified as Map
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day, fromGregorianValid, toGregorian)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Vector qualified as V
import Eval (applyValueOut, truthy)
import Text.Read (readMaybe)
import Value (Env, LangError (..), Value (..), numericToText, showValue, strOf, valueToJson)

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
    , ("toJson", b1 "toJson" $ \v -> either (Left . msg) (Right . VStr) (valueToJson v))
    , ("contains", b2 "contains" containsOf)
    , ("trim", b1 "trim" $ \v -> either (Left . msg) (Right . VStr . T.strip) (strOf v))
    , ("lower", b1 "lower" $ \v -> either (Left . msg) (Right . VStr . T.toLower) (strOf v))
    , ("upper", b1 "upper" $ \v -> either (Left . msg) (Right . VStr . T.toUpper) (strOf v))
    , ("replace", b3 "replace" replaceOf)
    , ("take", b2 "take" takeOf)
    , ("drop", b2 "drop" dropOf)
    , ("formatDate", b2 "formatDate" formatDateOf)
    , ("el", b3 "el" elOf)
    , ("esc", b1 "esc" escOf)
    , ("h1", b1 "h1" (tagOf "h1"))
    , ("h2", b1 "h2" (tagOf "h2"))
    , ("p", b1 "p" (tagOf "p"))
    , ("div", b1 "div" (tagOf "div"))
    , ("span", b1 "span" (tagOf "span"))
    , ("strong", b1 "strong" (tagOf "strong"))
    , ("em", b1 "em" (tagOf "em"))
    , ("time", b1 "time" (tagOf "time"))
    , ("ul", b1 "ul" (tagOf "ul"))
    , ("ol", b1 "ol" (tagOf "ol"))
    , ("li", b1 "li" (tagOf "li"))
    , ("a", b2 "a" aOf)
    , ("img", b2 "img" imgOf)
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
    _ -> typeErr "formatDate" ("date string and format string" <> ", got " <> showValue d <> " and " <> showValue f)

parseIsoDate :: Text -> Either Text Day
parseIsoDate t = case T.splitOn "-" t of
    [y, m, d] -> case (readMaybe (T.unpack y) :: Maybe Integer, readMaybe (T.unpack m) :: Maybe Int, readMaybe (T.unpack d) :: Maybe Int) of
        (Just yy, Just mm, Just dd) -> case fromGregorianValid yy mm dd of
            Just day -> Right day
            Nothing -> Left ("invalid date '" <> t <> "'")
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

-- | Escape `& < > " '` as HTML entities.
escOf :: Value -> Either LangError Value
escOf v = case v of
    VStr t -> Right (VStr (escapeHtml t))
    _ -> typeErr "esc" ("string" <> ", got " <> showValue v)

{- | Any tag: `el(name, attrs, content)`. Attributes: map keys and
values are escaped, `true` renders a bare attribute, `false`/`nil`
omits it, keys are in alphabetical order. Void elements (br, hr,
img, input, meta, link, source) render without a closing tag.
Content is inserted as-is.
-}
elOf :: Value -> Value -> Value -> Either LangError (Value, [Text])
elOf n a c = case (n, a, c) of
    (VStr name, VMap attrs, VStr content) -> do
        attrsHtml <- T.concat <$> mapM renderAttr (Map.toList attrs)
        ok (VStr ("<" <> name <> attrsHtml <> ">" <> if isVoid name then "" else content <> "</" <> name <> ">"))
    _ -> typeErr "el" ("tag name, attribute map and string content" <> ", got " <> showValue n <> ", " <> showValue a <> " and " <> showValue c)
  where
    renderAttr :: (Text, Value) -> Either LangError Text
    renderAttr (k, v) = case v of
        VBool True -> Right (" " <> k)
        VBool False -> Right ""
        VNil -> Right ""
        _ -> do
            t <- either (Left . msg) Right (strOf v)
            Right (" " <> k <> "=\"" <> escapeHtml t <> "\"")

    isVoid :: Text -> Bool
    isVoid name = name `elem` ["br", "hr", "img", "input", "meta", "link", "source"]

-- | A content-only tag: `name(content)`.
tagOf :: Text -> Value -> Either LangError Value
tagOf name c = case c of
    VStr content -> Right (VStr ("<" <> name <> ">" <> content <> "</" <> name <> ">"))
    _ -> typeErr name ("string content" <> ", got " <> showValue c)

-- | `a(content, href)`: an anchor with an escaped href.
aOf :: Value -> Value -> Either LangError (Value, [Text])
aOf c href = case (c, href) of
    (VStr content, VStr h) -> ok (VStr ("<a href=\"" <> escapeHtml h <> "\">" <> content <> "</a>"))
    _ -> typeErr "a" ("string content and string href" <> ", got " <> showValue c <> " and " <> showValue href)

-- | `img(src, alt)`: a void image tag; `nil` omits the alt attribute.
imgOf :: Value -> Value -> Either LangError (Value, [Text])
imgOf s alt = case s of
    VStr src -> do
        altHtml <- case alt of
            VNil -> Right ""
            VStr a -> Right (" alt=\"" <> escapeHtml a <> "\"")
            _ -> typeErr "img" ("string src and string or nil alt" <> ", got " <> showValue s <> " and " <> showValue alt)
        ok (VStr ("<img src=\"" <> escapeHtml src <> "\"" <> altHtml <> ">"))
    _ -> typeErr "img" ("string src and string or nil alt" <> ", got " <> showValue s <> " and " <> showValue alt)

escapeHtml :: Text -> Text
escapeHtml = T.concatMap escape
  where
    escape c = case c of
        '&' -> "&amp;"
        '<' -> "&lt;"
        '>' -> "&gt;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> T.singleton c

typeErr :: Text -> Text -> Either LangError a
typeErr name want = Left (msg ("cannot " <> name <> ": expected " <> want))

msg :: Text -> LangError
msg t = LangError t []
