module Page (CustomPage (..), loadPage) where

import Control.Exception (IOException, catch)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Pandoc (docHasMath, readerOpts, writerOpts)
import System.Directory (doesFileExist)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Definition (MetaValue (..), Pandoc (..), lookupMeta)
import Text.Pandoc.Options (HTMLMathMethod)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Writers.HTML (writeHtml5String)

data CustomPage = CustomPage
    { cpTitle :: Maybe Text
    , cpBodyHtml :: Text
    , cpHasMath :: Bool
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
                                Right body ->
                                    pure (Right (Just CustomPage{cpTitle = pageTitle doc, cpBodyHtml = body, cpHasMath = docHasMath doc}))

pageTitle :: Pandoc -> Maybe Text
pageTitle (Pandoc meta _) =
    case lookupMeta "title" meta of
        Just value -> metaText value
        Nothing -> Nothing

metaText :: MetaValue -> Maybe Text
metaText (MetaString t) = Just t
metaText (MetaInlines inlines) = Just (stringify inlines)
metaText _ = Nothing
