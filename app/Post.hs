module Post (Post (..), TocEntry (..), loadPosts, mathMethod, parsePost, warnCaseTags) where

import Data.Char (isDigit)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Pandoc (docHasMath, readerOpts, writerOpts)
import System.Directory (listDirectory)
import System.FilePath (takeFileName, (</>))
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Definition (Block (Header), MetaValue (..), Pandoc (..), lookupMeta)
import Text.Pandoc.Options (HTMLMathMethod (..), defaultKaTeXURL, defaultMathJaxURL)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (query)
import Text.Pandoc.Writers.HTML (writeHtml5String)

data Post = Post
    { postSlug :: Text
    , postTitle :: Text
    , postDate :: Text
    , postTags :: [Text]
    , postDescription :: Maybe Text
    , postDraft :: Bool
    , postBodyHtml :: Text
    , postText :: Text
    , postHasMath :: Bool
    , postShowToc :: Bool
    , postToc :: [TocEntry]
    }
    deriving (Eq, Show)

data TocEntry = TocEntry
    { tocLevel :: Int
    , tocTitle :: Text
    , tocId :: Text
    }
    deriving (Eq, Show)

data PostFields = PostFields
    { pfSlug :: Text
    , pfTitle :: Text
    , pfDate :: Text
    , pfTags :: [Text]
    , pfDescription :: Maybe Text
    , pfDraft :: Bool
    , pfHasMath :: Bool
    , pfShowToc :: Bool
    , pfToc :: [TocEntry]
    }

mathMethod :: Text -> Maybe Text -> HTMLMathMethod
mathMethod "none" _ = PlainMath
mathMethod "mathjax" url = MathJax (fromMaybe defaultMathJaxURL url)
mathMethod "katex" url = KaTeX (fromMaybe defaultKaTeXURL url)
mathMethod _ _ = PlainMath

loadPosts :: HTMLMathMethod -> FilePath -> IO (Either [Text] [Post])
loadPosts math dir = do
    names <- sortOn id . filter (T.isSuffixOf ".md" . T.pack) <$> listDirectory dir
    results <- mapM (loadOne math dir) names
    let errs = [name <> ": " <> reason | Left (name, reason) <- results]
    if null errs
        then pure (Right (sortPosts [post | Right post <- results, not (postDraft post)]))
        else pure (Left errs)

loadOne :: HTMLMathMethod -> FilePath -> FilePath -> IO (Either (Text, Text) Post)
loadOne math dir name = do
    content <- TIO.readFile (dir </> name)
    result <- parsePost math (dir </> name) content
    pure
        ( case result of
            Left reason -> Left (T.pack name, reason)
            Right post -> Right post
        )

parsePost :: HTMLMathMethod -> FilePath -> Text -> IO (Either Text Post)
parsePost math path content = do
    edoc <- runIO (readMarkdown readerOpts content)
    case edoc of
        Left err -> pure (Left (T.pack (show err)))
        Right doc ->
            case extractMeta name doc of
                Left err -> pure (Left err)
                Right fields -> do
                    ebody <- runIO (writeHtml5String (writerOpts math) doc)
                    pure (either (Left . T.pack . show) (Right . mkPost fields (bodyText doc)) ebody)
  where
    name = T.pack (takeFileName path)

extractMeta :: Text -> Pandoc -> Either Text PostFields
extractMeta name (Pandoc meta body) = do
    let base = fromMaybe name (T.stripSuffix ".md" name)
        (prefixDate, slug) = slugFromName base
        hasMath = docHasMath (Pandoc meta body)
    draft <- case lookupMeta "draft" meta of
        Nothing -> Right False
        Just (MetaBool b) -> Right b
        Just _ -> Left "field 'draft' must be a boolean (true or false)"
    title <- case lookupMeta "title" meta of
        Nothing -> Right slug
        Just value -> case metaText value of
            Just t -> Right t
            Nothing -> Left "field 'title' must be a string"
    date <- case lookupMeta "date" meta of
        Nothing -> case prefixDate of
            Just d -> Right d
            Nothing
                | draft -> Right ""
                | otherwise -> Left "missing date: set 'date: YYYY-MM-DD' in the frontmatter, or use a YYYY-MM-DD- filename prefix"
        Just value -> case metaText value of
            Just t
                | validIsoDate t -> Right t
                | otherwise -> Left ("invalid date '" <> t <> "': expected format YYYY-MM-DD")
            Nothing -> Left "field 'date' must be a string"
    tags <- case lookupMeta "tags" meta of
        Nothing -> Right []
        Just (MetaList values) -> mapM tagOf values
        Just _ -> Left "field 'tags' must be a list of strings"
    validTags <- traverse validateTag tags
    description <- case lookupMeta "description" meta of
        Nothing -> Right Nothing
        Just value -> case metaText value of
            Just t -> Right (Just t)
            Nothing -> Left "field 'description' must be a string"
    showToc <- case lookupMeta "toc" meta of
        Nothing -> Right False
        Just (MetaBool b) -> Right b
        Just _ -> Left "field 'toc' must be a boolean (true or false)"
    pure PostFields{pfSlug = slug, pfTitle = title, pfDate = date, pfTags = validTags, pfDescription = description, pfDraft = draft, pfHasMath = hasMath, pfShowToc = showToc, pfToc = tocOf body}

mkPost :: PostFields -> Text -> Text -> Post
mkPost fields text body =
    Post
        { postSlug = pfSlug fields
        , postTitle = pfTitle fields
        , postDate = pfDate fields
        , postTags = pfTags fields
        , postDescription = pfDescription fields
        , postDraft = pfDraft fields
        , postBodyHtml = body
        , postText = text
        , postHasMath = pfHasMath fields
        , postShowToc = pfShowToc fields
        , postToc = pfToc fields
        }

sortPosts :: [Post] -> [Post]
sortPosts = sortOn (\p -> (Down (postDate p), postSlug p))

slugFromName :: Text -> (Maybe Text, Text)
slugFromName name
    | T.length name > 10
        && validIsoDate (T.take 10 name)
        && T.index name 10 == '-' =
        (Just (T.take 10 name), T.drop 11 name)
    | otherwise = (Nothing, name)

validIsoDate :: Text -> Bool
validIsoDate t =
    T.length t == 10
        && T.index t 4 == '-'
        && T.index t 7 == '-'
        && T.all isDigit (T.take 4 t)
        && T.all isDigit (T.take 2 (T.drop 5 t))
        && T.all isDigit (T.take 2 (T.drop 8 t))

metaText :: MetaValue -> Maybe Text
metaText (MetaString t) = Just t
metaText (MetaInlines inlines) = Just (stringify inlines)
metaText _ = Nothing

{- | The heading structure of the body: (level, title, id) for every
Header, in document order. Ids come from the gfm_auto_identifiers
reader extension.
-}
tocOf :: [Block] -> [TocEntry]
tocOf body = query headerEntry body
  where
    headerEntry :: Block -> [TocEntry]
    headerEntry (Header level (ident, _, _) inlines) = [TocEntry level (stringify inlines) ident]
    headerEntry _ = []

-- | Plain text of the document body, excluding the frontmatter meta.
bodyText :: Pandoc -> Text
bodyText (Pandoc _ body) = stringify body

tagOf :: MetaValue -> Either Text Text
tagOf value = maybe (Left "field 'tags' must be a list of strings") Right (metaText value)

reservedTagChars :: String
reservedTagChars = "/?#% "

validateTag :: Text -> Either Text Text
validateTag tag
    | T.null (T.strip tag) =
        Left "tags must not contain empty or whitespace-only values"
    | T.any (`elem` reservedTagChars) tag =
        Left ("tag '" <> tag <> "' contains a reserved character (/, ?, #, %, space); percent-encode the tag or use a different one")
    | otherwise = Right tag

warnCaseTags :: [Post] -> [Text]
warnCaseTags posts =
    [ "tags " <> T.intercalate ", " (map quote (nub variants)) <> " differ only in case; consider unifying them"
    | variants <- Map.elems (Map.fromListWith (++) [(T.toLower tag, [tag]) | tag <- concatMap postTags posts])
    , nub variants `lengthGreaterThan` 1
    ]
  where
    quote tag = "'" <> tag <> "'"
    lengthGreaterThan xs n = length xs > n
