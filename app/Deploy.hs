module Deploy (run) where

import Build (runBuild)
import Cli (Paths (..), defaultPaths)
import Config (DeployConfig (..), SiteConfig (..), loadConfig)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
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
import System.Process (callProcess, cwd, proc, readCreateProcessWithExitCode)

run :: IO ()
run = do
    config <- loadConfig (pConfig defaultPaths)
    let d = siteDeploy config
    case (deployTarget d, deployRepo d) of
        (Just _, Just _) -> die "both deploy.target and deploy.repo are set in config.yaml; pick one"
        (Just target, Nothing) -> rsyncDeploy target
        (Nothing, Just repo) -> case deployBranch d of
            Nothing -> die "deploy.repo is set but deploy.branch is missing in config.yaml; git deployment needs a branch (e.g. gh-pages)"
            Just branch -> gitDeploy repo branch
        (Nothing, Nothing) -> do
            TIO.hPutStrLn stderr "error: no deploy configuration."
            TIO.hPutStrLn stderr "usage: set deploy.target (rsync) or deploy.repo + deploy.branch (git) in config.yaml"

rsyncDeploy :: Text -> IO ()
rsyncDeploy target = do
    runBuild defaultPaths
    callProcess "rsync" ["-avz", "--delete", "site/", T.unpack target <> "/"]

gitDeploy :: Text -> Text -> IO ()
gitDeploy repo branch = do
    runBuild defaultPaths
    withClone repo $ \clone -> do
        branchExists <- gitOk clone ["rev-parse", "--quiet", "--verify", "origin/" <> T.unpack branch]
        if branchExists
            then runGit clone ["checkout", "-q", "-B", T.unpack branch, "origin/" <> T.unpack branch]
            else runGit clone ["checkout", "-q", "--orphan", T.unpack branch]
        runGit clone ["rm", "-rq", "--ignore-unmatch", "."]
        copyTree "site" clone
        runGit clone ["add", "-A"]
        (code, _, _) <- readCreateProcessWithExitCode (proc "git" ["diff", "--cached", "--quiet"]){cwd = Just clone} ""
        if code == ExitSuccess
            then TIO.putStrLn "nothing to deploy"
            else do
                message <- deployMessage
                runGit clone ["commit", "-q", "-m", message]
                runGit clone ["push", "--quiet", "origin", "HEAD:" <> T.unpack branch]
                TIO.putStrLn ("deployed site to " <> repo <> " (" <> branch <> ")")

withClone :: Text -> (FilePath -> IO a) -> IO a
withClone repo k = do
    tmp <- getTemporaryDirectory
    let clone = tmp </> "burogu-deploy"
    removePathForcibly clone
    runGit "" ["clone", "--quiet", T.unpack repo, clone]
    bracket (pure clone) removePathForcibly k

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
