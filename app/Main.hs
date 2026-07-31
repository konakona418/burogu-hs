module Main where

import Config (SiteConfig (..), Theme (..), loadConfig)
import Control.Exception (IOException, catch)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Post (loadPosts, mathMethod, warnCaseTags)
import Site (BuildReport (..), build, postsSrcDir)
import System.Exit (exitFailure)
import System.IO (stderr)

main :: IO ()
main = do
    config <- loadConfig "config.yaml"
    let theme = siteTheme config
        styleName = themeHighlightStyle theme
        math = mathMethod (themeMath theme) (themeMathUrl theme)
    eposts <- loadPosts styleName math postsSrcDir `catch` \(e :: IOException) -> pure (Left [T.pack (show e)])
    case eposts of
        Left errs -> do
            TIO.hPutStrLn stderr "Build failed. The following problems were found:"
            TIO.hPutStrLn stderr (T.unlines (("  - " <>) <$> errs))
            TIO.hPutStrLn stderr "Nothing was written: the existing site/ directory was left untouched."
            exitFailure
        Right posts -> do
            mapM_ (TIO.hPutStrLn stderr . ("warning: " <>)) (warnCaseTags posts)
            ereport <- (Right <$> build config posts) `catch` \(e :: IOException) -> pure (Left e)
            case ereport of
                Left e -> do
                    TIO.hPutStrLn stderr ("Build failed: " <> T.pack (show e))
                    TIO.hPutStrLn stderr "site/ was removed; nothing is left to deploy."
                    exitFailure
                Right report -> printSummary (length posts) report

printSummary :: Int -> BuildReport -> IO ()
printSummary nPosts report = do
    TIO.putStrLn "Build complete."
    TIO.putStrLn ("  Posts generated : " <> T.pack (show nPosts))
    TIO.putStrLn ("  Tags generated  : " <> T.pack (show (brTagPages report)))
    TIO.putStrLn ("  Static files    : " <> T.pack (show (brStaticFiles report)))
    TIO.putStrLn ("  Feed            : " <> if brFeed report then "site/feed.xml" else "skipped (set baseUrl in config.yaml)")
    TIO.putStrLn "  Output directory: site/"
