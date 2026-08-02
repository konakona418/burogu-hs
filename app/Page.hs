module Page (CustomPage (..), defaultPagePriority, loadPage, loadPages) where

import Control.Exception (IOException, catch)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Pandoc (docHasMath, readerOpts, writerOpts)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Definition (MetaValue (..), Pandoc (..), lookupMeta)
import Text.Pandoc.Options (HTMLMathMethod)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Read (readMaybe)

defaultPagePriority :: Int
defaultPagePriority = 100

data CustomPage = CustomPage
    { cpTitle :: Maybe Text
    , cpBodyHtml :: Text
    , cpHasMath :: Bool
    , cpPriority :: Int
    , cpRedirectAs :: Maybe Text
    , cpHiddenInNavbar :: Bool
    , cpScript :: Maybe Text
    , cpText :: Text
    }
    deriving (Eq, Show)

{- | Load a custom page (e.g. 404.md, about.md) from a markdown file.
Returns Right Nothing when the file does not exist; Left on parse or
read errors.
-}
loadPage :: HTMLMathMethod -> FilePath -> IO (Either Text (Maybe CustomPage))
loadPage math path = do
    exists <- doesFileExist path
    if not exists
        then pure (Right Nothing)
        else do
            econtent <- (Right <$> TIO.readFile path) `catch` \(e :: IOException) -> pure (Left (T.pack (show e)))
            case econtent of
                Left err -> pure (Left err)
                Right content -> do
                    edoc <- runIO (readMarkdown readerOpts content)
                    case edoc of
                        Left err -> pure (Left (T.pack (show err)))
                        Right doc -> do
                            ebody <- runIO (writeHtml5String (writerOpts math) doc)
                            case ebody of
                                Left err -> pure (Left (T.pack (show err)))
                                Right body -> case pagePriority doc of
                                    Left err -> pure (Left err)
                                    Right priority -> case pageHiddenInNavbar doc of
                                        Left err -> pure (Left err)
                                        Right hidden ->
                                            pure (Right (Just CustomPage{cpTitle = pageTitle doc, cpBodyHtml = body, cpHasMath = docHasMath doc, cpPriority = priority, cpRedirectAs = pageRedirectAs doc, cpHiddenInNavbar = hidden, cpScript = pageScript doc, cpText = bodyText doc}))

{- | Load all custom pages from a directory of markdown files. Each file
becomes a page keyed by its basename without the .md extension; slugs
are sorted alphabetically. A missing directory yields an empty list;
parse errors are aggregated.
-}
loadPages :: HTMLMathMethod -> FilePath -> IO (Either [Text] [(Text, CustomPage)])
loadPages math dir = do
    isDir <- doesDirectoryExist dir
    if not isDir
        then pure (Right [])
        else do
            names <- sortOn id . filter (T.isSuffixOf ".md" . T.pack) <$> listDirectory dir
            results <- mapM (loadOne math dir) names
            let errs = [name <> ": " <> err | Left (name, err) <- results]
            if null errs
                then pure (Right [(slug name, page) | Right (name, page) <- results])
                else pure (Left errs)
  where
    loadOne :: HTMLMathMethod -> FilePath -> FilePath -> IO (Either (Text, Text) (Text, CustomPage))
    loadOne method base filename = do
        result <- loadPage method (base </> filename)
        pure
            ( case result of
                Left err -> Left (T.pack filename, err)
                Right (Just page) -> Right (slug (T.pack filename), page)
                Right Nothing -> Left (T.pack filename, "unexpectedly missing")
            )
    slug :: Text -> Text
    slug name = fromMaybe name (T.stripSuffix ".md" name)

pageTitle :: Pandoc -> Maybe Text
pageTitle (Pandoc meta _) =
    case lookupMeta "title" meta of
        Just value -> metaText value
        Nothing -> Nothing

pageRedirectAs :: Pandoc -> Maybe Text
pageRedirectAs (Pandoc meta _) =
    case lookupMeta "redirectAs" meta of
        Just value -> metaText value
        Nothing -> Nothing

pageScript :: Pandoc -> Maybe Text
pageScript (Pandoc meta _) =
    case lookupMeta "script" meta of
        Just value -> metaText value
        Nothing -> Nothing

pagePriority :: Pandoc -> Either Text Int
pagePriority (Pandoc meta _) =
    case lookupMeta "priority" meta of
        Nothing -> Right defaultPagePriority
        Just value -> case metaInt value of
            Just n -> Right n
            Nothing -> Left "invalid priority in frontmatter: expected an integer"

pageHiddenInNavbar :: Pandoc -> Either Text Bool
pageHiddenInNavbar (Pandoc meta _) =
    case lookupMeta "hiddenInNavbar" meta of
        Nothing -> Right False
        Just (MetaBool b) -> Right b
        Just _ -> Left "invalid hiddenInNavbar in frontmatter: expected true or false"

metaInt :: MetaValue -> Maybe Int
metaInt (MetaString t) = readMaybe (T.unpack t)
metaInt (MetaInlines inlines) = readMaybe (T.unpack (stringify inlines))
metaInt _ = Nothing

metaText :: MetaValue -> Maybe Text
metaText (MetaString t) = Just t
metaText (MetaInlines inlines) = Just (stringify inlines)
metaText _ = Nothing

-- | Plain text of the document body, excluding the frontmatter meta.
bodyText :: Pandoc -> Text
bodyText (Pandoc _ body) = stringify body
