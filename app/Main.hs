module Main where

import Cli (Paths (..), parsePaths)
import Config (SiteConfig (..), Theme (..), loadConfig)
import Control.Exception (IOException, catch)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Post (loadPosts, mathMethod, warnCaseTags)
import Site (BuildReport (..), build)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)

main :: IO ()
main = do
    paths <- parsePaths
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
            ereport <- (Right <$> build paths config posts) `catch` \(e :: IOException) -> pure (Left e)
            case ereport of
                Left e -> do
                    TIO.hPutStrLn stderr ("Build failed: " <> T.pack (show e))
                    TIO.hPutStrLn stderr "The output directory was removed; nothing is left to deploy."
                    exitFailure
                Right report -> printSummary paths (length posts) report

printSummary :: Paths -> Int -> BuildReport -> IO ()
printSummary paths nPosts report = do
    TIO.putStrLn "Build complete."
    TIO.putStrLn ("  Posts generated : " <> T.pack (show nPosts))
    TIO.putStrLn ("  Tags generated  : " <> T.pack (show (brTagPages report)))
    TIO.putStrLn ("  Static files    : " <> T.pack (show (brStaticFiles report)))
    TIO.putStrLn ("  Feed            : " <> if brFeed report then "feed.xml" else "skipped (set baseUrl in config.yaml)")
    TIO.putStrLn ("  Sitemap         : " <> if brSitemap report then "sitemap.xml" else "skipped (set baseUrl in config.yaml)")
    TIO.putStrLn ("  Output directory: " <> T.pack (pOut paths))
