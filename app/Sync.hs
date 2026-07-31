module Sync (run) where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Env (envValue)
import System.Directory (
    copyFile,
    createDirectory,
    doesDirectoryExist,
    getTemporaryDirectory,
    listDirectory,
    removePathForcibly,
 )
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
import System.Process (cwd, proc, readCreateProcessWithExitCode)

run :: Text -> Maybe Text -> IO ()
run action mRepo = do
    repo <- maybe (envValue "BUROGU_SRC_REPO") (pure . Just) mRepo
    case repo of
        Nothing -> do
            TIO.hPutStrLn stderr "error: no repo URL."
            TIO.hPutStrLn stderr "usage: cabal run burogu -- sync [push|pull] [repo-url]  or set BUROGU_SRC_REPO in .env"
            exitFailure
        Just r -> case action of
            "pull" -> pull r
            "push" -> push r
            _ -> do
                TIO.hPutStrLn stderr ("error: unknown action " <> action <> " (expected push or pull)")
                exitFailure

withClone :: Text -> (FilePath -> IO a) -> IO a
withClone repo k = do
    tmp <- getTemporaryDirectory
    let clone = tmp </> "burogu-sync"
    removePathForcibly clone
    runGit "" ["clone", "--quiet", T.unpack repo, clone]
    bracket (pure clone) removePathForcibly k

runGit :: FilePath -> [String] -> IO ()
runGit dir args = do
    (code, out, err) <- readCreateProcessWithExitCode (proc "git" args){cwd = if null dir then Nothing else Just dir} ""
    unless (code == ExitSuccess) $ do
        TIO.hPutStrLn stderr (T.pack out)
        TIO.hPutStrLn stderr (T.pack err)
        exitFailure

pull :: Text -> IO ()
pull repo = withClone repo $ \clone -> do
    removePathForcibly "src"
    createDirectory "src"
    copyTree clone "src" False
    TIO.putStrLn ("pulled src/ from " <> repo)

push :: Text -> IO ()
push repo = withClone repo $ \clone -> do
    runGit clone ["rm", "-rq", "--ignore-unmatch", "."]
    copyTree "src" clone False
    runGit clone ["add", "-A"]
    (code, _, _) <- readCreateProcessWithExitCode (proc "git" ["diff", "--cached", "--quiet"]){cwd = Just clone} ""
    if code == ExitSuccess
        then TIO.putStrLn "nothing to push"
        else do
            message <- syncMessage
            runGit clone ["commit", "-q", "-m", message]
            runGit clone ["push", "--quiet", "origin", "HEAD"]
            TIO.putStrLn ("pushed src/ to " <> repo)

syncMessage :: IO String
syncMessage = do
    now <- getCurrentTime
    zone <- getCurrentTimeZone
    pure (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (utcToLocalTime zone now))

{- | Recursively copy the contents of a directory. When @keepDotGit@ is
False the source's .git directory is skipped.
-}
copyTree :: FilePath -> FilePath -> Bool -> IO ()
copyTree srcBase dstBase keepDotGit = do
    entries <- listDirectory srcBase
    mapM_ copyOne (filter keep entries)
  where
    keep :: FilePath -> Bool
    keep name = keepDotGit || name /= ".git"
    copyOne :: FilePath -> IO ()
    copyOne name = do
        let source = srcBase </> name
            target = dstBase </> name
        isDir <- doesDirectoryExist source
        if isDir
            then do
                createDirectory target
                copyTree source target keepDotGit
            else copyFile source target
