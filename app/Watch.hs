module Watch (runPreview, runWatch) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Functor ((<&>))
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (
    doesDirectoryExist,
    getModificationTime,
    listDirectory,
 )
import System.FilePath ((</>))
import System.Process (callProcess)

pollInterval :: Int
pollInterval = 2000000

buildSite :: IO ()
buildSite = callProcess "cabal" ["run", "burogu", "--", "build"]

serveSite :: Int -> IO ()
serveSite port = do
    TIO.putStrLn ("Preview: http://127.0.0.1:" <> T.pack (show port) <> "/  (Ctrl-C to stop)")
    callProcess "python3" ["-m", "http.server", show port, "--bind", "127.0.0.1", "--directory", "site"]

runPreview :: Int -> IO ()
runPreview port = do
    buildSite
    serveSite port

runWatch :: Maybe Int -> IO ()
runWatch mPort = do
    case mPort of
        Just port -> serveSite port
        Nothing -> pure ()
    lastMtime <- latestMtime
    buildSite
    TIO.putStrLn "Watching src/ and config.yaml... (Ctrl-C to stop)"
    loop lastMtime
  where
    loop :: UTCTime -> IO ()
    loop lastMtime = do
        threadDelay pollInterval
        current <- latestMtime
        if current > lastMtime
            then do
                TIO.putStrLn "--- rebuilding ---"
                buildSite
                loop current
            else loop lastMtime

{- | The newest modification time among src/ (recursively) and config.yaml,
falling back to the current time when nothing is readable.
-}
latestMtime :: IO UTCTime
latestMtime = do
    now <- getCurrentTime
    files <- collectFiles "src" <&> (++ ["config.yaml"])
    times <- mapM (\f -> try (getModificationTime f) :: IO (Either SomeException UTCTime)) files
    let readable = [t | Right t <- times]
    pure (if null readable then now else foldl1 max readable)

collectFiles :: FilePath -> IO [FilePath]
collectFiles dir = do
    isDir <- doesDirectoryExist dir
    if not isDir
        then pure [dir]
        else do
            entries <- listDirectory dir
            fmap concat . mapM (\e -> collectFiles (dir </> e)) $ entries
