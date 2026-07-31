module Deploy (run) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Env (envValue)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.Process (callProcess)

run :: Maybe Text -> IO ()
run mTarget = do
    target <- maybe (envValue "BUROGU_DEPLOY_TARGET") (pure . Just) mTarget
    case target of
        Nothing -> do
            TIO.hPutStrLn stderr "error: no deploy target."
            TIO.hPutStrLn stderr "usage: cabal run burogu -- deploy <user@host:/path>  or set BUROGU_DEPLOY_TARGET in .env"
            exitFailure
        Just t -> do
            callProcess "cabal" ["run", "burogu", "--", "build"]
            callProcess "rsync" ["-avz", "--delete", "site/", T.unpack t <> "/"]
