module Site (BuildReport (..), build) where

import Config (SiteConfig (..), Theme (..))
import Css (renderCss)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Feed (renderAtom)
import Html qualified as H
import Lucid qualified as L
import Post (Post (..))
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removePathForcibly,
 )
import System.FilePath ((</>))

srcDir, outDir, postsDir :: FilePath
srcDir = "src"
outDir = "site"
postsDir = "posts"

data BuildReport = BuildReport
    { brStaticFiles :: Int
    , brTagPages :: Int
    , brFeed :: Bool
    }

build :: SiteConfig -> [Post] -> IO BuildReport
build config posts = do
    removePathForcibly outDir
    createDirectoryIfMissing True outDir
    TIO.writeFile (outDir </> "index.html") (TL.toStrict (L.renderText (H.renderIndex config posts)))
    TIO.writeFile (outDir </> "style.css") (renderCss (themeHighlightStyle (siteTheme config)))
    mapM_ (writePost config) posts
    writeTagPages config posts
    brFeed <- writeFeed config posts
    nStatic <- copyStatic
    pure BuildReport{brStaticFiles = nStatic, brTagPages = length (H.groupByTag posts), brFeed = brFeed}

writePost :: SiteConfig -> Post -> IO ()
writePost config post = do
    let dir = outDir </> postsDir </> T.unpack (postSlug post)
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderPost config post)))

writeTagPages :: SiteConfig -> [Post] -> IO ()
writeTagPages config posts = do
    let groups = H.groupByTag posts
        tagsDir = outDir </> "tags"
    createDirectoryIfMissing True tagsDir
    TIO.writeFile (tagsDir </> "index.html") (TL.toStrict (L.renderText (H.renderTagIndex config groups)))
    mapM_ (writeTagPage config tagsDir) groups

writeTagPage :: SiteConfig -> FilePath -> (Text, [Post]) -> IO ()
writeTagPage config tagsDir (tag, posts) = do
    let dir = tagsDir </> T.unpack tag
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderTagArchive config tag posts)))

writeFeed :: SiteConfig -> [Post] -> IO Bool
writeFeed config posts =
    case siteBaseUrl config of
        Just baseUrl
            | not (null posts) -> do
                TIO.writeFile (outDir </> "feed.xml") (renderAtom config baseUrl posts)
                pure True
        _ -> pure False

copyStatic :: IO Int
copyStatic = do
    entries <- listDirectory srcDir
    let others = filter (/= "_post") entries
    mapM_ (copyTree srcDir outDir) others
    pure (length others)

copyTree :: FilePath -> FilePath -> FilePath -> IO ()
copyTree srcBase dstBase name = do
    let source = srcBase </> name
        target = dstBase </> name
    isDir <- doesDirectoryExist source
    if isDir
        then do
            createDirectoryIfMissing True target
            children <- listDirectory source
            mapM_ (copyTree source target) children
        else copyFile source target
