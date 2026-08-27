module Sync (run) where

import Cli (Paths (..), defaultPaths)
import Config (SiteConfig (..), loadConfig)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Exit (ExitCode (..), exitFailure)
import System.IO (stderr)
import System.Process (proc, readCreateProcessWithExitCode)

{- | Sync the site repository (the directory containing config.yaml and
src/) with a remote git repo. Runs git in the current directory; the
remote URL comes from the argument, falling back to config's srcRepo.
-}
run :: Text -> Maybe Text -> Bool -> IO ()
run action mRepo verbose = do
    config <- loadConfig (pConfig defaultPaths)
    let repo = maybe (siteSrcRepo config) Just mRepo
    case repo of
        Nothing -> do
            TIO.hPutStrLn stderr "error: no repo URL."
            TIO.hPutStrLn stderr "usage: cabal run burogu -- sync [push|pull] [repo-url]  or set srcRepo in config.yaml"
            exitFailure
        Just r -> case action of
            "pull" -> pull verbose r
            "push" -> push verbose r
            _ -> do
                TIO.hPutStrLn stderr ("error: unknown action " <> action <> " (expected push or pull)")
                exitFailure

{- | Point the origin remote at the given URL: set-url when origin
already exists, add otherwise.
-}
ensureRemote :: Bool -> Text -> IO ()
ensureRemote verbose repo = do
    (code, _, _) <- readCreateProcessWithExitCode (proc "git" ["remote", "get-url", "origin"]) ""
    let url = T.unpack repo
    if code == ExitSuccess
        then runGit verbose ["remote", "set-url", "origin", url]
        else runGit verbose ["remote", "add", "origin", url]

push :: Bool -> Text -> IO ()
push verbose repo = do
    ensureRemote verbose repo
    runGit verbose ["add", "-A"]
    (code, _, _) <- readCreateProcessWithExitCode (proc "git" ["diff", "--cached", "--quiet"]) ""
    unless (code == ExitSuccess) $ do
        message <- syncMessage
        runGit verbose ["commit", "-q", "-m", message]
    synced <- sameAsRemote
    if code == ExitSuccess && synced
        then TIO.putStrLn "nothing to push"
        else do
            runGit verbose ["push", "--quiet", "origin", "HEAD"]
            TIO.putStrLn ("pushed site to " <> repo)
sameAsRemote :: IO Bool
sameAsRemote = do
    local <- revParse "HEAD"
    remote <- revParse =<< remoteRef
    pure (local == remote)

{- | The remote ref tracking HEAD: origin/<current-branch>, falling back
to origin/HEAD (e.g. detached HEAD).
-}
remoteRef :: IO String
remoteRef = do
    mBranch <- gitOut ["symbolic-ref", "--quiet", "--short", "HEAD"]
    pure (maybe "origin/HEAD" ("origin/" <>) mBranch)

revParse :: String -> IO (Maybe String)
revParse ref = gitOut ["rev-parse", "--quiet", "--verify", ref]

gitOut :: [String] -> IO (Maybe String)
gitOut args = do
    (code, out, _) <- readCreateProcessWithExitCode (proc "git" args) ""
    pure (if code == ExitSuccess then Just (head (lines out)) else Nothing)

{- | Remote wins: fetch and hard-reset the whole site (config + src) to
the remote's HEAD.
-}
pull :: Bool -> Text -> IO ()
pull verbose repo = do
    ensureRemote verbose repo
    runGit verbose ["fetch", "--quiet", "origin"]
    ref <- remoteRef
    runGit verbose ["reset", "--hard", ref]
    TIO.putStrLn ("pulled site from " <> repo)

syncMessage :: IO String
syncMessage = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime

{- | Run git. With verbose, drop the silent flags and forward the
tool's own output; otherwise print it only on failure.
-}
runGit :: Bool -> [String] -> IO ()
runGit verbose args = do
    let args' = if verbose then filter (`notElem` ["--quiet", "-q"]) args else args
    (code, out, err) <- readCreateProcessWithExitCode (proc "git" args') ""
    if verbose
        then do
            TIO.putStr (T.pack out)
            TIO.hPutStr stderr (T.pack err)
        else unless (code == ExitSuccess) $ do
            TIO.putStrLn (T.pack out)
            TIO.hPutStrLn stderr (T.pack err)
    unless (code == ExitSuccess) exitFailure
