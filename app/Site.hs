module Site (BuildReport (..), build) where

import Cli (Paths (..))
import Config (SiteConfig (..), Theme (..))
import Control.Exception (IOException, catch, throwIO)
import Css (renderCss)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Feed (renderAtom)
import Html qualified as H
import Lucid qualified as L
import Page (CustomPage (..))
import Page qualified
import Post (Post (..), mathMethod)
import Sitemap (renderSitemap)
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removePathForcibly,
 )
import System.FilePath ((</>))
import Text.Pandoc.Options (HTMLMathMethod)

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
    validateExtraJs paths config
    let math = mathMethod (themeMath (siteTheme config)) (themeMathUrl (siteTheme config))
    e404page <- loadCustomPage math (pSrc paths </> "404.md")
    eabout <- loadCustomPage math (pSrc paths </> "about.md")
    let mAbout = cpTitle <$> eabout
        mAboutLabel = fromMaybe "about" <$> mAbout
    removePathForcibly (pOut paths)
    createDirectoryIfMissing True (pOut paths)
    TIO.writeFile (pOut paths </> "index.html") (TL.toStrict (L.renderText (H.renderIndex config mAboutLabel posts)))
    write404 paths config mAboutLabel e404page
    writeAbout paths config mAboutLabel eabout
    writeStyleSheet paths config
    mapM_ (writePost paths config mAboutLabel) posts
    writeTagPages paths config mAboutLabel posts
    writeRobots paths config
    brFeed <- writeFeed paths config posts
    brSitemap <- writeSitemap paths config posts
    nStatic <- copyStatic paths
    pure BuildReport{brStaticFiles = nStatic, brTagPages = length (H.groupByTag posts), brFeed = brFeed, brSitemap = brSitemap}

loadCustomPage :: HTMLMathMethod -> FilePath -> IO (Maybe CustomPage)
loadCustomPage math path = do
    result <- Page.loadPage math path
    case result of
        Left err -> ioError (userError (path <> ": " <> T.unpack err))
        Right mPage -> pure mPage

write404 :: Paths -> SiteConfig -> Maybe Text -> Maybe CustomPage -> IO ()
write404 paths config mAbout mPage =
    case mPage of
        Just page ->
            TIO.writeFile (pOut paths </> "404.html") (TL.toStrict (L.renderText (H.renderCustomPage config mAbout pageMeta page)))
        Nothing ->
            TIO.writeFile (pOut paths </> "404.html") (TL.toStrict (L.renderText (H.render404 config mAbout)))
  where
    pageMeta :: H.PageMeta
    pageMeta =
        H.PageMeta
            { H.pmTitle = fromMaybe "404" (cpTitle =<< mPage)
            , H.pmOgType = "website"
            , H.pmOgPath = "/404.html"
            , H.pmOgDescription = Nothing
            , H.pmHasMath = maybe False cpHasMath mPage
            }

writeAbout :: Paths -> SiteConfig -> Maybe Text -> Maybe CustomPage -> IO ()
writeAbout paths config mAbout mPage =
    case mPage of
        Nothing -> pure ()
        Just page -> do
            let dir = pOut paths </> "about"
            createDirectoryIfMissing True dir
            TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderCustomPage config mAbout (aboutMeta page) page)))
  where
    aboutMeta :: CustomPage -> H.PageMeta
    aboutMeta page =
        H.PageMeta
            { H.pmTitle = fromMaybe "about" (cpTitle page)
            , H.pmOgType = "website"
            , H.pmOgPath = "/about/"
            , H.pmOgDescription = Nothing
            , H.pmHasMath = cpHasMath page
            }

writeStyleSheet :: Paths -> SiteConfig -> IO ()
writeStyleSheet paths config = do
    extra <- mapM (readExtraCss . T.unpack) (themeExtraCss (siteTheme config))
    TIO.writeFile (pOut paths </> "style.css") (renderCss extra)
  where
    readExtraCss :: FilePath -> IO Text
    readExtraCss file = TIO.readFile (pSrc paths </> file)

validateExtraJs :: Paths -> SiteConfig -> IO ()
validateExtraJs paths config =
    mapM_ check (themeExtraJs (siteTheme config))
  where
    check :: Text -> IO ()
    check file = do
        exists <- doesFileExist (pSrc paths </> T.unpack file)
        if exists
            then pure ()
            else ioError (userError ("extra JS file not found in " <> pSrc paths <> ": " <> T.unpack file))

writePost :: Paths -> SiteConfig -> Maybe Text -> Post -> IO ()
writePost paths config mAbout post = do
    let dir = pOut paths </> postsDirName </> T.unpack (postSlug post)
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderPost config mAbout post)))

writeTagPages :: Paths -> SiteConfig -> Maybe Text -> [Post] -> IO ()
writeTagPages paths config mAbout posts = do
    let groups = H.groupByTag posts
        tagsDir = pOut paths </> T.unpack (T.dropWhile (== '/') (T.dropWhileEnd (== '/') H.tagUrlPrefix))
    createDirectoryIfMissing True tagsDir
    TIO.writeFile (tagsDir </> "index.html") (TL.toStrict (L.renderText (H.renderTagIndex config mAbout groups)))
    mapM_ (writeTagPage config mAbout tagsDir) groups

writeTagPage :: SiteConfig -> Maybe Text -> FilePath -> (Text, [Post]) -> IO ()
writeTagPage config mAbout tagsDir (tag, posts) = do
    let dir = tagsDir </> T.unpack tag
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderTagArchive config mAbout tag posts)))

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
    let others = filter (`notElem` ["_post", "404.md", "about.md"]) entries
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
