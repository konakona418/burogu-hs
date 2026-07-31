module NewPost (run) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Calendar (Day, toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)

reservedChars :: String
reservedChars = "/?#% "

run :: Text -> Bool -> IO ()
run slug draft = do
    if T.any (`elem` reservedChars) slug
        then do
            TIO.hPutStrLn stderr "error: slug contains a reserved character (/, ?, #, %, space)"
            exitFailure
        else pure ()
    postDir <- pure "src/_post"
    if draft
        then writeSlug postDir (T.unpack slug <> ".md") (draftFrontmatter slug)
        else do
            today <- getCurrentTime >>= pure . toIsoDate . utctDay
            writeSlug postDir (T.unpack today <> "-" <> T.unpack slug <> ".md") (regularFrontmatter slug today)

toIsoDate :: Day -> Text
toIsoDate day =
    let (y, m, d) = toGregorian day
     in T.pack (show y) <> "-" <> T.pack (pad m) <> "-" <> T.pack (pad d)
  where
    pad :: Int -> String
    pad n = if n < 10 then "0" <> show n else show n

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

writeSlug :: FilePath -> FilePath -> Text -> IO ()
writeSlug postDir filename content = do
    let path = postDir </> filename
    exists <- doesFileExist path
    if exists
        then do
            TIO.hPutStrLn stderr ("error: " <> T.pack path <> " already exists")
            exitFailure
        else do
            TIO.writeFile path content
            TIO.putStrLn ("created " <> T.pack path)
