module Posts (orDie, runDraft, runNew, runPublish, runRename) where

import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Yaml (decodeEither', encode)
import Frontmatter (Kind (..), normalizeFrontmatter, splitFrontmatter)
import System.Directory (doesFileExist, listDirectory, renameFile)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)

reservedChars :: String
reservedChars = "/?#% "

toIsoDate :: Day -> Text
toIsoDate day =
    let (y, m, d) = toGregorian day
     in T.pack (show y) <> "-" <> T.pack (pad m) <> "-" <> T.pack (pad d)
  where
    pad :: Int -> String
    pad n = if n < 10 then "0" <> show n else show n

orDie :: IO (Either Text a) -> IO ()
orDie act = act >>= either die (const (pure ()))

die :: Text -> IO a
die msg = do
    TIO.hPutStrLn stderr ("error: " <> msg)
    exitFailure

runNew :: FilePath -> Text -> IO (Either Text FilePath)
runNew dir slug = case validateSlug slug of
    Left err -> pure (Left err)
    Right () -> do
        today <- getCurrentTime >>= pure . toIsoDate . utctDay
        let filename = T.unpack today <> "-" <> T.unpack slug <> ".md"
        eFree <- ensureSlugFree dir slug
        case eFree of
            Left err -> pure (Left err)
            Right () -> writeSlug dir filename (regularFrontmatter slug today)

runDraft :: FilePath -> Text -> IO (Either Text FilePath)
runDraft dir slug = case validateSlug slug of
    Left err -> pure (Left err)
    Right () -> do
        today <- getCurrentTime >>= pure . toIsoDate . utctDay
        let filename = T.unpack today <> "-" <> T.unpack slug <> ".md"
        eFree <- ensureSlugFree dir slug
        case eFree of
            Left err -> pure (Left err)
            Right () -> writeSlug dir filename (draftFrontmatter slug)

runPublish :: FilePath -> Text -> IO (Either Text FilePath)
runPublish dir slug = case validateSlug slug of
    Left err -> pure (Left err)
    Right () -> do
        files <- matches dir slug
        case files of
            [] -> pure (Left ("no draft found for '" <> slug <> "' in " <> T.pack dir))
            [_] -> publishOne dir (head files)
            _ -> pure (Left ("multiple files match slug '" <> slug <> "': " <> T.intercalate ", " (map T.pack files)))

runRename :: FilePath -> Text -> Text -> IO (Either Text FilePath)
runRename dir oldSlug newSlug = case validateSlug newSlug of
    Left err -> pure (Left err)
    Right () -> do
        files <- matches dir oldSlug
        case files of
            [] -> pure (Left ("no post found for '" <> oldSlug <> "' in " <> T.pack dir))
            [_] -> do
                eFree <- ensureSlugFree dir newSlug
                case eFree of
                    Left err -> pure (Left err)
                    Right () -> renameOne dir (head files) newSlug
            _ -> pure (Left ("multiple files match slug '" <> oldSlug <> "': " <> T.intercalate ", " (map T.pack files)))

{- | Rename a post file only: keep the date prefix, change the slug,
leave the frontmatter untouched.
-}
renameOne :: FilePath -> FilePath -> Text -> IO (Either Text FilePath)
renameOne dir file newSlug = do
    let oldPath = dir </> file
        newFile = T.unpack (T.take 10 (T.pack file)) <> "-" <> T.unpack newSlug <> ".md"
        newPath = dir </> newFile
    renameFile oldPath newPath
    TIO.putStrLn ("renamed " <> T.pack oldPath <> " -> " <> T.pack newPath)
    pure (Right newPath)

publishOne :: FilePath -> FilePath -> IO (Either Text FilePath)
publishOne dir file = do
    let path = dir </> file
    content <- TIO.readFile path
    case splitFrontmatter content of
        (Nothing, _) -> pure (Left (T.pack file <> " is not a draft (no frontmatter)"))
        (Just block, body) -> do
            if not (isDraft block)
                then pure (Left (T.pack file <> " is not a draft (draft: true missing)"))
                else do
                    today <- getCurrentTime >>= pure . toIsoDate . utctDay
                    case promoteFrontmatter today block of
                        Left err -> pure (Left (T.pack file <> ": " <> err))
                        Right block' -> case normalizeFrontmatter PostKind path block' of
                            Left err -> pure (Left (T.pack file <> ": " <> err))
                            Right (canonical, _, _) -> do
                                let newContent = "---\n" <> canonical <> "---\n" <> body
                                unless (newContent == content) (TIO.writeFile path newContent)
                                TIO.putStrLn ("published " <> T.pack path)
                                pure (Right path)

{- | Replace the draft flag with a date: the existing frontmatter date
wins, otherwise today; the draft key is dropped.
-}
promoteFrontmatter :: Text -> Text -> Either Text Text
promoteFrontmatter today block = do
    object <- case decodeEither' (encodeUtf8 block) of
        Right (Object o) -> pure o
        Right _ -> Left "frontmatter is not a YAML mapping"
        Left err -> Left (T.pack (show err))
    let mDate = KM.lookup (K.fromText "date") object
        object' = KM.insert (K.fromText "date") (String (maybe today textValue mDate)) (KM.delete (K.fromText "draft") object)
    pure (T.strip (decodeUtf8 (encode (Object object'))))
  where
    textValue :: Value -> Text
    textValue (String t) = t
    textValue _ = today

isDraft :: Text -> Bool
isDraft block = case decodeEither' (encodeUtf8 block) of
    Right (Object o) -> KM.lookup (K.fromText "draft") o == Just (Bool True)
    _ -> False

validateSlug :: Text -> Either Text ()
validateSlug slug =
    if T.any (`elem` reservedChars) slug
        then Left "slug contains a reserved character (/, ?, #, %, space)"
        else Right ()

ensureSlugFree :: FilePath -> Text -> IO (Either Text ())
ensureSlugFree dir slug = do
    dupes <- matches dir slug
    case dupes of
        [] -> pure (Right ())
        (d : _) -> pure (Left ("a post with slug '" <> slug <> "' already exists (" <> T.pack d <> ")"))

{- | The files in the post directory whose name is SLUG.md or ends
with -SLUG.md.
-}
matches :: FilePath -> Text -> IO [FilePath]
matches dir slug = do
    names <- map T.pack <$> listDirectory dir
    pure [T.unpack n | n <- names, n == slugName || T.isSuffixOf ("-" <> slugName) n]
  where
    slugName = slug <> ".md"

writeSlug :: FilePath -> FilePath -> Text -> IO (Either Text FilePath)
writeSlug dir filename content = do
    let path = dir </> filename
    exists <- doesFileExist path
    if exists
        then pure (Left (T.pack path <> " already exists"))
        else do
            TIO.writeFile path content
            TIO.putStrLn ("created " <> T.pack path)
            pure (Right path)

draftFrontmatter :: Text -> Text
draftFrontmatter slug =
    T.unlines
        [ "---"
        , "title: " <> slug
        , "draft: true"
        , "# tags: []"
        , "# description: "
        , "---"
        , ""
        ]

regularFrontmatter :: Text -> Text -> Text
regularFrontmatter slug today =
    T.unlines
        [ "---"
        , "title: " <> slug
        , "date: " <> today
        , "# tags: []"
        , "# description: "
        , "---"
        , ""
        ]
