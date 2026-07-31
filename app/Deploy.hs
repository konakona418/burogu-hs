module Deploy (run) where

import Build (runBuild)
import Cli (Paths (..), defaultPaths)
import Config (SiteConfig (..), loadConfig)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Exit (exitFailure)
import System.IO (stderr)
import System.Process (callProcess)

run :: Maybe Text -> IO ()
run mTarget = do
    config <- loadConfig (pConfig defaultPaths)
    let fromConfig = siteDeployTarget config
        target = maybe fromConfig Just mTarget
    case target of
        Nothing -> do
            TIO.hPutStrLn stderr "error: no deploy target."
            TIO.hPutStrLn stderr "usage: cabal run burogu -- deploy <user@host:/path>  or set deployTarget in config.yaml"
            exitFailure
        Just t -> do
            runBuild defaultPaths
            callProcess "rsync" ["-avz", "--delete", "site/", T.unpack t <> "/"]
