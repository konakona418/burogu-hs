module Post (Post (..), loadPosts, parsePost) where

import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory)
import System.FilePath (takeFileName, (</>))
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Definition (MetaValue (..), Pandoc (..), lookupMeta)
import Text.Pandoc.Extensions (Extension (..), pandocExtensions)
import Text.Pandoc.Options (HighlightMethod (..), ReaderOptions (..), WriterOptions (..), def, enableExtension)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Writers.HTML (writeHtml5String)

data Post = Post
    { postSlug :: Text
    , postTitle :: Text
    , postDate :: Text
    , postTags :: [Text]
    , postDescription :: Maybe Text
    , postDraft :: Bool
    , postBodyHtml :: Text
    }

data PostFields = PostFields
    { pfSlug :: Text
    , pfTitle :: Text
    , pfDate :: Text
    , pfTags :: [Text]
    , pfDescription :: Maybe Text
    , pfDraft :: Bool
    }

readerOpts :: ReaderOptions
readerOpts =
    def
        { readerExtensions =
            enableExtension Ext_autolink_bare_uris (enableExtension Ext_gfm_auto_identifiers pandocExtensions)
        }

writerOpts :: WriterOptions
writerOpts = def{writerHighlightMethod = NoHighlighting}

loadPosts :: FilePath -> IO (Either [Text] [Post])
loadPosts dir = do
    names <- sortOn id . filter (T.isSuffixOf ".md" . T.pack) <$> listDirectory dir
    results <- mapM (loadOne dir) names
    let errs = [name <> ": " <> reason | Left (name, reason) <- results]
    if null errs
        then pure (Right (sortPosts [post | Right post <- results, not (postDraft post)]))
        else pure (Left errs)

loadOne :: FilePath -> FilePath -> IO (Either (Text, Text) Post)
loadOne dir name = do
    content <- TIO.readFile (dir </> name)
    result <- parsePost (dir </> name) content
    pure
        ( case result of
            Left reason -> Left (T.pack name, reason)
            Right post -> Right post
        )

parsePost :: FilePath -> Text -> IO (Either Text Post)
parsePost path content = do
    edoc <- runIO (readMarkdown readerOpts content)
    case edoc of
        Left err -> pure (Left (T.pack (show err)))
        Right doc ->
            case extractMeta name doc of
                Left err -> pure (Left err)
                Right fields -> do
                    ebody <- runIO (writeHtml5String writerOpts doc)
                    pure (either (Left . T.pack . show) (Right . mkPost fields) ebody)
  where
    name = T.pack (takeFileName path)

extractMeta :: Text -> Pandoc -> Either Text PostFields
extractMeta name (Pandoc meta _) = do
    let base = fromMaybe name (T.stripSuffix ".md" name)
        (prefixDate, slug) = slugFromName base
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
    description <- case lookupMeta "description" meta of
        Nothing -> Right Nothing
        Just value -> case metaText value of
            Just t -> Right (Just t)
            Nothing -> Left "field 'description' must be a string"
    pure PostFields{pfSlug = slug, pfTitle = title, pfDate = date, pfTags = tags, pfDescription = description, pfDraft = draft}

mkPost :: PostFields -> Text -> Post
mkPost fields body =
    Post
        { postSlug = pfSlug fields
        , postTitle = pfTitle fields
        , postDate = pfDate fields
        , postTags = pfTags fields
        , postDescription = pfDescription fields
        , postDraft = pfDraft fields
        , postBodyHtml = body
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

tagOf :: MetaValue -> Either Text Text
tagOf value = maybe (Left "field 'tags' must be a list of strings") Right (metaText value)
