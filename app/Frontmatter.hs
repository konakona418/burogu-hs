module Frontmatter (Kind (..), normalizeFrontmatter, splitFrontmatter) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.Scientific (FPFormat (Generic), Scientific, floatingOrInteger, formatScientific, toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import Data.Yaml (decodeEither', encode)
import System.FilePath (takeFileName)

data Kind = PostKind | PageKind

{- | Split a markdown file into (frontmatter block, body). The block is
Nothing when the file does not start with a `---` line (or the block
is not closed).
-}
splitFrontmatter :: Text -> (Maybe Text, Text)
splitFrontmatter content = case T.lines content of
    "---" : rest -> case break (== "---") rest of
        (blockLines, bodyLines)
            | not (null bodyLines) -> (Just (T.unlines blockLines), T.unlines (drop 1 bodyLines))
        _ -> (Nothing, content)
    _ -> (Nothing, content)

{- | Normalize a frontmatter block: fill in every missing field with
its default, order the keys canonically, and keep unknown keys (with a
warning). Returns the rendered block and the unknown keys. The
filename supplies the date (posts) and the slug (title default). An
empty block yields a fully-defaulted frontmatter.
-}
normalizeFrontmatter :: Kind -> FilePath -> Text -> Either Text (Text, [Text])
normalizeFrontmatter kind filename block = do
    object <- case decodeEither' (encodeUtf8 block) of
        Right (Object o) -> pure o
        Right _ -> Left "frontmatter is not a YAML mapping"
        Left _ -> Right mempty
    case kind of
        PostKind -> normalizePost object
        PageKind -> normalizePage object
  where
    name = T.pack (takeFileName filename)
    slug = fromMaybe name (T.stripSuffix ".md" name)

    normalizePost :: KM.KeyMap Value -> Either Text (Text, [Text])
    normalizePost object = do
        title <- fromMaybe slug <$> strOpt "title" object
        mDate <- strOpt "date" object
        tags <- tagsField object
        mDescription <- strOpt "description" object
        draft <- boolOpt "draft" object
        let date = case mDate of
                Just d -> Just d
                Nothing -> prefixDate name
        if date == Nothing && not draft
            then Left "no date (neither in frontmatter nor the filename prefix)"
            else
                let known =
                        ("title", title)
                            : maybe [] (\d -> [("date", d)]) date
                                <> [("tags", tags)]
                                <> maybe [] (\d -> [("description", d)]) mDescription
                                <> [("draft", showBool draft)]
                 in Right (render (known <> unknownPairs object ["title", "date", "tags", "description", "draft"]), unknownKeys object ["title", "date", "tags", "description", "draft"])

    normalizePage :: KM.KeyMap Value -> Either Text (Text, [Text])
    normalizePage object = do
        title <- fromMaybe slug <$> strOpt "title" object
        priority <- maybe 100 id <$> intOpt "priority" object
        hidden <- boolOpt "hiddenInNavbar" object
        mRedirect <- strOpt "redirectAs" object
        let known =
                [ ("title", title)
                , ("priority", T.pack (show priority))
                , ("hiddenInNavbar", showBool hidden)
                ]
                    <> maybe [] (\r -> [("redirectAs", r)]) mRedirect
        Right (render (known <> unknownPairs object ["title", "priority", "hiddenInNavbar", "redirectAs"]), unknownKeys object ["title", "priority", "hiddenInNavbar", "redirectAs"])

    strOpt :: Text -> KM.KeyMap Value -> Either Text (Maybe Text)
    strOpt key object = case lookupKey key object of
        Nothing -> Right Nothing
        Just (String t) -> Right (Just t)
        Just (Number n) -> Right (Just (numberText n))
        Just _ -> Left ("field '" <> key <> "' must be a string")

    numberText :: Scientific -> Text
    numberText n = case (floatingOrInteger n :: Either Double Integer) of
        Right i -> T.pack (show i)
        Left _ -> T.pack (formatScientific Generic Nothing n)

    intOpt :: Text -> KM.KeyMap Value -> Either Text (Maybe Int)
    intOpt key object = case lookupKey key object of
        Nothing -> Right Nothing
        Just (Number n) -> case toBoundedInteger n :: Maybe Int of
            Just i -> Right (Just i)
            Nothing -> Left ("field '" <> key <> "' must be an integer")
        Just _ -> Left ("field '" <> key <> "' must be an integer")

    boolOpt :: Text -> KM.KeyMap Value -> Either Text Bool
    boolOpt key object = case lookupKey key object of
        Nothing -> Right False
        Just (Bool b) -> Right b
        Just _ -> Left ("field '" <> key <> "' must be a boolean")

    lookupKey :: Text -> KM.KeyMap Value -> Maybe Value
    lookupKey key = KM.lookup (K.fromText key)

    tagsField :: KM.KeyMap Value -> Either Text Text
    tagsField object = case lookupKey "tags" object of
        Nothing -> Right "[]"
        Just (Array arr) -> Right (inlineList [t | String t <- V.toList arr])
        Just _ -> Left "field 'tags' must be a list of strings"

    unknownKeys :: KM.KeyMap Value -> [Text] -> [Text]
    unknownKeys object known =
        [k | k <- map K.toText (KM.keys object), k `notElem` known]

    unknownPairs :: KM.KeyMap Value -> [Text] -> [(Text, Text)]
    unknownPairs object known =
        [(k, emitValue v) | k <- unknownKeys object known, Just v <- [lookupKey k object]]

    emitValue :: Value -> Text
    emitValue v = case T.lines (T.strip (decodeUtf8 (encode v))) of
        [] -> ""
        [_] -> T.strip (decodeUtf8 (encode v))
        _ -> T.unlines (map ("  " <>) (T.lines (T.strip (decodeUtf8 (encode v)))))

    render :: [(Text, Text)] -> Text
    render = T.unlines . map renderOne
      where
        renderOne :: (Text, Text) -> Text
        renderOne (k, v)
            | "\n" `T.isInfixOf` v = k <> ":\n" <> T.dropWhileEnd (== '\n') v
            | otherwise = k <> ": " <> v

    showBool :: Bool -> Text
    showBool True = "true"
    showBool False = "false"

    inlineList :: [Text] -> Text
    inlineList [] = "[]"
    inlineList items = "[" <> T.intercalate ", " (map scalar items) <> "]"

    scalar :: Text -> Text
    scalar = T.strip . decodeUtf8 . encode . String

    prefixDate :: Text -> Maybe Text
    prefixDate n = case T.splitOn "-" n of
        (y : m : rest)
            | T.length y == 4 && T.length m == 2 ->
                let d = T.take 2 (T.intercalate "-" rest)
                 in if T.all isDigit d && T.length d == 2 then Just (y <> "-" <> m <> "-" <> d) else Nothing
        _ -> Nothing
