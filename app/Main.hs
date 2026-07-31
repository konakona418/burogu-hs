module Main where

import Config (SiteConfig (..), Theme (..), loadConfig)
import Control.Exception (IOException, catch)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Post (loadPosts, warnCaseTags)
import Site (build)
import System.Exit (exitFailure)
import System.IO (stderr)

main :: IO ()
main = do
    config <- loadConfig "config.yaml"
    let styleName = themeHighlightStyle (siteTheme config)
    eposts <- loadPosts styleName "src/_post" `catch` \(e :: IOException) -> pure (Left [T.pack (show e)])
    case eposts of
        Left errs -> do
            TIO.hPutStrLn stderr "Build failed. The following problems were found:"
            TIO.hPutStrLn stderr (T.unlines (("  - " <>) <$> errs))
            TIO.hPutStrLn stderr "Nothing was written: the existing site/ directory was left untouched."
            exitFailure
        Right posts -> do
            mapM_ (TIO.hPutStrLn stderr . ("warning: " <>)) (warnCaseTags posts)
            (nStatic, nTags) <- build config posts
            TIO.putStrLn "Build complete."
            TIO.putStrLn ("  Posts generated : " <> T.pack (show (length posts)))
            TIO.putStrLn ("  Tags generated  : " <> T.pack (show nTags))
            TIO.putStrLn ("  Static files    : " <> T.pack (show nStatic))
            TIO.putStrLn "  Output directory: site/"
