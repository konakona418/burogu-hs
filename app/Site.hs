module Site (BuildReport (..), build) where

import Cli (Paths (..))
import Config (SiteConfig (..), Theme (..))
import Control.Exception (IOException, catch, throwIO)
import Css (renderCss)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Feed (renderAtom)
import Html qualified as H
import Lucid qualified as L
import Post (Post (..))
import Sitemap (renderSitemap)
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
    removePathForcibly,
 )
import System.FilePath ((</>))

postsDirName :: FilePath
postsDirName = "posts"

data BuildReport = BuildReport
    { brStaticFiles :: Int
    , brTagPages :: Int
    , brFeed :: Bool
    , brSitemap :: Bool
    }

build :: Paths -> SiteConfig -> [Post] -> IO BuildReport
build paths config posts = do
    report <-
        buildWork paths config posts `catch` \(e :: IOException) -> do
            removePathForcibly (pOut paths)
            throwIO e
    pure report

buildWork :: Paths -> SiteConfig -> [Post] -> IO BuildReport
buildWork paths config posts = do
    removePathForcibly (pOut paths)
    createDirectoryIfMissing True (pOut paths)
    TIO.writeFile (pOut paths </> "index.html") (TL.toStrict (L.renderText (H.renderIndex config posts)))
    TIO.writeFile (pOut paths </> "404.html") (TL.toStrict (L.renderText (H.render404 config)))
    writeStyleSheet paths config
    mapM_ (writePost paths config) posts
    writeTagPages paths config posts
    writeRobots paths config
    brFeed <- writeFeed paths config posts
    brSitemap <- writeSitemap paths config posts
    nStatic <- copyStatic paths
    pure BuildReport{brStaticFiles = nStatic, brTagPages = length (H.groupByTag posts), brFeed = brFeed, brSitemap = brSitemap}

writeStyleSheet :: Paths -> SiteConfig -> IO ()
writeStyleSheet paths config = do
    extra <- mapM (readExtraCss . T.unpack) (themeExtraCss (siteTheme config))
    TIO.writeFile (pOut paths </> "style.css") (renderCss extra)
  where
    readExtraCss :: FilePath -> IO Text
    readExtraCss file = TIO.readFile (pSrc paths </> file)

writePost :: Paths -> SiteConfig -> Post -> IO ()
writePost paths config post = do
    let dir = pOut paths </> postsDirName </> T.unpack (postSlug post)
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderPost config post)))

writeTagPages :: Paths -> SiteConfig -> [Post] -> IO ()
writeTagPages paths config posts = do
    let groups = H.groupByTag posts
        tagsDir = pOut paths </> T.unpack (T.dropWhile (== '/') (T.dropWhileEnd (== '/') H.tagUrlPrefix))
    createDirectoryIfMissing True tagsDir
    TIO.writeFile (tagsDir </> "index.html") (TL.toStrict (L.renderText (H.renderTagIndex config groups)))
    mapM_ (writeTagPage config tagsDir) groups

writeTagPage :: SiteConfig -> FilePath -> (Text, [Post]) -> IO ()
writeTagPage config tagsDir (tag, posts) = do
    let dir = tagsDir </> T.unpack tag
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderTagArchive config tag posts)))

writeFeed :: Paths -> SiteConfig -> [Post] -> IO Bool
writeFeed paths config posts =
    case siteBaseUrl config of
        Just baseUrl
            | not (null posts) -> do
                TIO.writeFile (pOut paths </> "feed.xml") (renderAtom config baseUrl posts)
                pure True
        _ -> pure False

writeSitemap :: Paths -> SiteConfig -> [Post] -> IO Bool
writeSitemap paths config posts =
    case siteBaseUrl config of
        Just baseUrl
            | not (null posts) -> do
                TIO.writeFile (pOut paths </> "sitemap.xml") (renderSitemap baseUrl posts)
                pure True
        _ -> pure False

writeRobots :: Paths -> SiteConfig -> IO ()
writeRobots paths config =
    TIO.writeFile (pOut paths </> "robots.txt") $
        "User-agent: *\n"
            <> "Allow: /\n"
            <> maybe "" (\baseUrl -> "Sitemap: " <> baseUrl <> "/sitemap.xml\n") (siteBaseUrl config)

copyStatic :: Paths -> IO Int
copyStatic paths = do
    entries <- listDirectory (pSrc paths)
    let others = filter (/= "_post") entries
    mapM_ (copyTree (pSrc paths) (pOut paths)) others
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
