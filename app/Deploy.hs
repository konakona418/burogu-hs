module Deploy (run) where

import Build (runBuild)
import Cli (Paths (..), defaultPaths)
import Config (DeployConfig (..), SiteConfig (..), loadConfig)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (
    XdgDirectory (XdgCache),
    copyFile,
    createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    getXdgDirectory,
    listDirectory,
    removePathForcibly,
 )
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
import System.Process (callProcess, cwd, proc, readCreateProcessWithExitCode)

run :: Bool -> IO ()
run clearCacheFlag
    | clearCacheFlag = clearCache
    | otherwise = deploySite

deploySite :: IO ()
deploySite = do
    config <- loadConfig (pConfig defaultPaths)
    let d = siteDeploy config
    case (deployTarget d, deployRepo d) of
        (Just _, Just _) -> die "both deploy.target and deploy.repo are set in config.yaml; pick one"
        (Just target, Nothing) -> rsyncDeploy target
        (Nothing, Just repo) -> case (deployBranch d, deployCommitName d, deployCommitEmail d) of
            (Nothing, _, _) -> die "deploy.repo is set but deploy.branch is missing in config.yaml; git deployment needs a branch (e.g. gh-pages)"
            (Just _, Nothing, _) -> die "deploy.repo is set but deploy.commitName is missing in config.yaml; git deployment needs a commit identity"
            (Just _, _, Nothing) -> die "deploy.repo is set but deploy.commitEmail is missing in config.yaml; git deployment needs a commit identity"
            (Just branch, Just name, Just email) -> gitDeploy repo branch name email
        (Nothing, Nothing) -> do
            TIO.hPutStrLn stderr "error: no deploy configuration."
            TIO.hPutStrLn stderr "usage: set deploy.target (rsync) or deploy.repo + deploy.branch (git) in config.yaml"
            exitFailure

clearCache :: IO ()
clearCache = do
    cache <- cacheDir
    removePathForcibly cache
    TIO.putStrLn ("deploy cache cleared: " <> T.pack cache)

{- | The persistent git cache for git deployment: holds the deployed
branch's history so each deploy fetches and pushes only the deltas
(no full re-clone, no other branches, no force).
-}
cacheDir :: IO FilePath
cacheDir = getXdgDirectory XdgCache "burogu-deploy"

rsyncDeploy :: Text -> IO ()
rsyncDeploy target = do
    runBuild defaultPaths
    callProcess "rsync" ["-avz", "--delete", "site/", T.unpack target <> "/"]

gitDeploy :: Text -> Text -> Text -> Text -> IO ()
gitDeploy repo branch name email = do
    runBuild defaultPaths
    cache <- cacheDir
    createDirectoryIfMissing True cache
    runGit cache ["init", "--quiet"]
    ensureRemote cache repo
    branchExists <- gitOk cache ["ls-remote", "--exit-code", "origin", "refs/heads/" <> T.unpack branch]
    if branchExists
        then do
            runGit cache ["fetch", "--quiet", "--depth", "1", "--no-tags", "origin", T.unpack branch]
            runGit cache ["checkout", "-q", "-B", T.unpack branch, "FETCH_HEAD"]
        else runGit cache ["checkout", "-q", "--orphan", T.unpack branch]
    runGit cache ["rm", "-rq", "--ignore-unmatch", "."]
    copyTree "site" cache
    runGit cache ["add", "-A"]
    (code, _, _) <- readCreateProcessWithExitCode (proc "git" ["diff", "--cached", "--quiet"]){cwd = Just cache} ""
    if code == ExitSuccess
        then TIO.putStrLn "nothing to deploy"
        else do
            message <- deployMessage
            runGit cache ["-c", "user.name=" <> T.unpack name, "-c", "user.email=" <> T.unpack email, "commit", "-q", "-m", message]
            runGit cache ["push", "--quiet", "origin", T.unpack branch]
            TIO.putStrLn ("deployed site to " <> repo <> " (" <> branch <> ")")

{- | Point the cache's origin remote at the given URL: set-url when
origin already exists, add otherwise.
-}
ensureRemote :: FilePath -> Text -> IO ()
ensureRemote dir repo = do
    (code, _, _) <- readCreateProcessWithExitCode (proc "git" ["remote", "get-url", "origin"]){cwd = Just dir} ""
    let url = T.unpack repo
    if code == ExitSuccess
        then runGit dir ["remote", "set-url", "origin", url]
        else runGit dir ["remote", "add", "origin", url]

gitOk :: FilePath -> [String] -> IO Bool
gitOk dir args = do
    (code, _, _) <- readCreateProcessWithExitCode (proc "git" args){cwd = Just dir} ""
    pure (code == ExitSuccess)

runGit :: FilePath -> [String] -> IO ()
runGit dir args = do
    (code, out, err) <- readCreateProcessWithExitCode (proc "git" args){cwd = if null dir then Nothing else Just dir} ""
    unless (code == ExitSuccess) $ do
        TIO.putStrLn (T.pack out)
        TIO.hPutStrLn stderr (T.pack err)
        exitFailure

deployMessage :: IO String
deployMessage = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" <$> getCurrentTime

-- | Recursively copy the contents of a directory.
copyTree :: FilePath -> FilePath -> IO ()
copyTree srcBase dstBase = do
    entries <- listDirectory srcBase
    mapM_ copyOne entries
  where
    copyOne :: FilePath -> IO ()
    copyOne name = do
        let source = srcBase </> name
            target = dstBase </> name
        isDir <- doesDirectoryExist source
        if isDir
            then do
                createDirectory target
                copyTree source target
            else copyFile source target

die :: Text -> IO a
die msg = do
    TIO.hPutStrLn stderr ("config.yaml: " <> msg)
    exitFailure
