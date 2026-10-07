module Build (runBuild) where

import Cli (Paths (..))
import Config (SiteConfig (..), Theme (..), loadConfig)
import Control.Exception (IOException, SomeException, catch, fromException, throwIO)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Layout (LayoutError (..))
import Post (loadPosts, mathMethod, warnCaseTags)
import Script (resetGensym)
import Site (BuildReport (..), build)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
import System.IO.Error (isUserError)

runBuild :: Paths -> IO ()
runBuild paths = do
    resetGensym
    config <- loadConfig (pConfig paths)
    let math = mathMethod (themeMath (siteTheme config)) (themeMathUrl (siteTheme config))
    eposts <- loadPosts math (pSrc paths </> "_post") `catch` \(e :: IOException) -> pure (Left [T.pack (show e)])
    case eposts of
        Left errs -> do
            TIO.hPutStrLn stderr "Build failed. The following problems were found:"
            TIO.hPutStrLn stderr (T.unlines (("  - " <>) <$> errs))
            TIO.hPutStrLn stderr "Nothing was written: the existing output directory was left untouched."
            exitFailure
        Right posts -> do
            mapM_ (TIO.hPutStrLn stderr . ("warning: " <>)) (warnCaseTags posts)
            result <-
                (Right <$> build paths config posts)
                    `catch` \(e :: SomeException) ->
                        case fromException e :: Maybe LayoutError of
                            Just (LayoutError msgs) -> pure (Left ("layout script:\n" <> T.unlines (("  - " <>) <$> msgs), False))
                            Nothing -> case fromException e :: Maybe IOException of
                                Just ioe -> pure (Left (T.pack (show ioe), isUserError ioe))
                                Nothing -> throwIO e
            case result of
                Left (message, userError') -> do
                    TIO.hPutStrLn stderr ("Build failed: " <> message)
                    if userError'
                        then TIO.hPutStrLn stderr "Nothing was written: the existing output directory was left untouched."
                        else TIO.hPutStrLn stderr "The output directory was removed; nothing is left to deploy."
                    exitFailure
                Right report -> printSummary paths (length posts) report

printSummary :: Paths -> Int -> BuildReport -> IO ()
printSummary paths nPosts report = do
    TIO.putStrLn "Build complete."
    TIO.putStrLn ("  Posts generated : " <> T.pack (show nPosts))
    TIO.putStrLn ("  Tags generated  : " <> T.pack (show (brTagPages report)))
    TIO.putStrLn ("  Static files    : " <> T.pack (show (brStaticFiles report)))
    TIO.putStrLn ("  Script files    : " <> T.pack (show (brScriptFiles report)))
    TIO.putStrLn ("  Feed            : " <> if brFeed report then "feed.xml" else "skipped (set baseUrl in config.yaml)")
    TIO.putStrLn ("  Sitemap         : " <> if brSitemap report then "sitemap.xml" else "skipped (set baseUrl in config.yaml)")
    TIO.putStrLn ("  Output directory: " <> T.pack (pOut paths))
