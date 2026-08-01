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
import Page (CustomPage (..), loadPages)
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
    let pagesDir = pSrc paths </> "_pages"
    epages <- loadPages math pagesDir
    case epages of
        Left errs -> ioError (userError (T.unpack (T.unlines (("_pages: " <>) <$> errs))))
        Right pages -> do
            let (m404, navPages) = partitionPages pages
                navItems = [(fromMaybe slug (cpTitle page), "/" <> slug <> "/") | (slug, page) <- navPages]
            removePathForcibly (pOut paths)
            createDirectoryIfMissing True (pOut paths)
            TIO.writeFile (pOut paths </> "index.html") (TL.toStrict (L.renderText (H.renderIndex config navItems posts)))
            write404 paths config navItems m404
            writePages paths config navItems navPages
            writeStyleSheet paths config
            mapM_ (writePost paths config navItems) posts
            writeTagPages paths config navItems posts
            writeRobots paths config
            brFeed <- writeFeed paths config posts
            brSitemap <- writeSitemap paths config posts navItems
            nStatic <- copyStatic paths
            pure BuildReport{brStaticFiles = nStatic, brTagPages = length (H.groupByTag posts), brFeed = brFeed, brSitemap = brSitemap}

-- | Split loaded pages into the special 404 page and the nav pages.
partitionPages :: [(Text, CustomPage)] -> (Maybe CustomPage, [(Text, CustomPage)])
partitionPages pages =
    ( lookup "404" pages
    , [(slug, page) | (slug, page) <- pages, slug /= "404"]
    )

write404 :: Paths -> SiteConfig -> [(Text, Text)] -> Maybe CustomPage -> IO ()
write404 paths config navItems mPage =
    case mPage of
        Just page ->
            TIO.writeFile (pOut paths </> "404.html") (TL.toStrict (L.renderText (H.renderCustomPage config navItems pageMeta page)))
        Nothing ->
            TIO.writeFile (pOut paths </> "404.html") (TL.toStrict (L.renderText (H.render404 config navItems)))
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

writePages :: Paths -> SiteConfig -> [(Text, Text)] -> [(Text, CustomPage)] -> IO ()
writePages paths config navItems pages =
    mapM_ writeOne pages
  where
    writeOne :: (Text, CustomPage) -> IO ()
    writeOne (slug, page) = do
        let dir = pOut paths </> T.unpack slug
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderCustomPage config navItems pageMeta page)))
      where
        pageMeta :: H.PageMeta
        pageMeta =
            H.PageMeta
                { H.pmTitle = fromMaybe slug (cpTitle page)
                , H.pmOgType = "website"
                , H.pmOgPath = "/" <> slug <> "/"
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

writePost :: Paths -> SiteConfig -> [(Text, Text)] -> Post -> IO ()
writePost paths config navItems post = do
    let dir = pOut paths </> postsDirName </> T.unpack (postSlug post)
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderPost config navItems post)))

writeTagPages :: Paths -> SiteConfig -> [(Text, Text)] -> [Post] -> IO ()
writeTagPages paths config navItems posts = do
    let groups = H.groupByTag posts
        tagsDir = pOut paths </> T.unpack (T.dropWhile (== '/') (T.dropWhileEnd (== '/') H.tagUrlPrefix))
    createDirectoryIfMissing True tagsDir
    TIO.writeFile (tagsDir </> "index.html") (TL.toStrict (L.renderText (H.renderTagIndex config navItems groups)))
    mapM_ (writeTagPage config navItems tagsDir) groups

writeTagPage :: SiteConfig -> [(Text, Text)] -> FilePath -> (Text, [Post]) -> IO ()
writeTagPage config navItems tagsDir (tag, posts) = do
    let dir = tagsDir </> T.unpack tag
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderTagArchive config navItems tag posts)))

writeFeed :: Paths -> SiteConfig -> [Post] -> IO Bool
writeFeed paths config posts =
    case siteBaseUrl config of
        Just baseUrl
            | not (null posts) -> do
                TIO.writeFile (pOut paths </> "feed.xml") (renderAtom config baseUrl posts)
                pure True
        _ -> pure False

writeSitemap :: Paths -> SiteConfig -> [Post] -> [(Text, Text)] -> IO Bool
writeSitemap paths config posts navItems =
    case siteBaseUrl config of
        Just baseUrl
            | not (null posts) -> do
                TIO.writeFile (pOut paths </> "sitemap.xml") (renderSitemap baseUrl posts navItems)
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
    let others = filter (`notElem` ["_post", "_pages"]) entries
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
