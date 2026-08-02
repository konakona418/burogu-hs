module Site (BuildReport (..), build) where

import Cli (Paths (..))
import Config (SiteConfig (..), Theme (..))
import Control.Exception (IOException, catch, throwIO)
import Css (FontFile (..), Fonts (..), renderCss)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Feed (renderAtom)
import Html qualified as H
import Lucid qualified as L
import Page (CustomPage (..), loadPages)
import Post (Post (..), mathMethod)
import Registry (SitePages (..), classifyPages, defaultArchiveTitle, defaultSearchTitle, defaultTagsLabel, navItems, specialPages)
import Scripts (filterScriptPages, loadData, runPageScripts)
import Search (renderSearch, renderSearchIndex)
import Shaft (presetByName)
import Sitemap (renderSitemap)
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
    removePathForcibly,
 )
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (isUserError)

postsDirName :: FilePath
postsDirName = "posts"

data BuildReport = BuildReport
    { brStaticFiles :: Int
    , brTagPages :: Int
    , brFeed :: Bool
    , brSitemap :: Bool
    , brScriptFiles :: Int
    }

build :: Paths -> SiteConfig -> [Post] -> IO BuildReport
build paths config posts = do
    report <-
        buildWork paths config posts `catch` \(e :: IOException) ->
            if isUserError e
                then throwIO e
                else do
                    removePathForcibly (pOut paths)
                    throwIO e
    pure report

buildWork :: Paths -> SiteConfig -> [Post] -> IO BuildReport
buildWork paths config posts = do
    validateExtraJs paths config
    validateFontFiles paths config
    let math = mathMethod (themeMath (siteTheme config)) (themeMathUrl (siteTheme config))
    let pagesDir = pSrc paths </> "_pages"
    epages <- loadPages math pagesDir
    case epages of
        Left errs -> ioError (userError (T.unpack (T.unlines (("_pages: " <>) <$> errs))))
        Right pages -> do
            (pages', outputSpecs) <- case filterScriptPages pages of
                Left errs -> ioError (userError (T.unpack (T.unlines (("_pages: " <>) <$> errs))))
                Right r -> pure r
            case classifyPages pages' of
                Left errs -> ioError (userError (T.unpack (T.unlines (("_pages: " <>) <$> errs))))
                Right spNav -> do
                    let nav = navItems spNav
                    edata <- loadData paths
                    dataEnv <- case edata of
                        Left errs -> ioError (userError (T.unpack (T.unlines (("_data: " <>) <$> errs))))
                        Right env -> pure env
                    scriptResult <- runPageScripts paths config nav posts pages' outputSpecs dataEnv
                    (pages'', scriptFiles) <- case scriptResult of
                        Left errs -> ioError (userError (T.unpack (T.unlines (("_pages: " <>) <$> errs))))
                        Right r -> pure r
                    case classifyPages pages'' of
                        Left errs -> ioError (userError (T.unpack (T.unlines (("_pages: " <>) <$> errs))))
                        Right sp -> do
                            removePathForcibly (pOut paths)
                            createDirectoryIfMissing True (pOut paths)
                            TIO.writeFile (pOut paths </> "index.html") (TL.toStrict (L.renderText (H.renderIndex config nav (snd <$> spIndex sp) posts)))
                            write404 paths config nav (sp404 sp)
                            writePages paths config nav (spNormal sp)
                            writeRedirects paths config (specialPages sp <> spRedirects sp)
                            writeStyleSheet paths config
                            mapM_ (writePost paths config nav posts) posts
                            writeTagPages paths config nav (spTags sp) posts
                            writeArchive paths config nav (spArchive sp) posts
                            writeSearch paths config nav sp posts
                            writeRobots paths config
                            brFeed <- writeFeed paths config posts
                            brSitemap <- writeSitemap paths config posts nav (isJust (spTags sp))
                            nStatic <- copyStatic paths
                            writeScriptOutputs paths scriptFiles
                            pure BuildReport{brStaticFiles = nStatic, brTagPages = length (H.groupByTag posts), brFeed = brFeed, brSitemap = brSitemap, brScriptFiles = length scriptFiles}

write404 :: Paths -> SiteConfig -> [(Text, Text)] -> Maybe (Text, CustomPage) -> IO ()
write404 paths config nav mEntry =
    case mEntry of
        Just (_, page) ->
            TIO.writeFile (pOut paths </> "404.html") (TL.toStrict (L.renderText (H.renderCustomPage config nav pageMeta page)))
        Nothing ->
            TIO.writeFile (pOut paths </> "404.html") (TL.toStrict (L.renderText (H.render404 config nav)))
  where
    pageMeta :: H.PageMeta
    pageMeta =
        H.PageMeta
            { H.pmTitle = fromMaybe "404" (cpTitle =<< (snd <$> mEntry))
            , H.pmOgType = "website"
            , H.pmOgPath = "/404.html"
            , H.pmOgDescription = Nothing
            , H.pmHasMath = maybe False cpHasMath (snd <$> mEntry)
            }

writePages :: Paths -> SiteConfig -> [(Text, Text)] -> [(Text, CustomPage)] -> IO ()
writePages paths config nav pages =
    mapM_ writeOne pages
  where
    writeOne :: (Text, CustomPage) -> IO ()
    writeOne (slug, page) = do
        let dir = pOut paths </> T.unpack slug
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderCustomPage config nav pageMeta page)))
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

{- | Write redirect pages for special pages whose file slug differs
from their canonical URL.
-}
writeRedirects :: Paths -> SiteConfig -> [(Text, CustomPage)] -> IO ()
writeRedirects paths config specials =
    mapM_ writeOne [(slug, page) | (slug, page) <- specials, "/" <> slug <> "/" /= canonical page]
  where
    canonical :: CustomPage -> Text
    canonical page = fromMaybe "" (cpRedirectAs page)
    writeOne :: (Text, CustomPage) -> IO ()
    writeOne (slug, page) = do
        let dir = pOut paths </> T.unpack slug
        createDirectoryIfMissing True dir
        TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderRedirect config (canonical page))))

writeStyleSheet :: Paths -> SiteConfig -> IO ()
writeStyleSheet paths config = do
    let theme = siteTheme config
        preset = case presetByName (themePreset theme) of
            Just p -> p
            Nothing -> error ("unknown theme preset: " <> T.unpack (themePreset theme))
    extra <- mapM (readExtraCss . T.unpack) (themeExtraCss theme)
    copyFonts paths (fromMaybe [] (fontsFiles (themeFonts theme)))
    TIO.writeFile (pOut paths </> "style.css") (renderCss preset (themeFonts theme) extra)
  where
    readExtraCss :: FilePath -> IO Text
    readExtraCss file = TIO.readFile (pSrc paths </> file)

{- | Copy embedded font files into site/fonts/. The @font-face rules
reference them as /fonts/<basename> (site-root absolute).
-}
copyFonts :: Paths -> [FontFile] -> IO ()
copyFonts paths = mapM_ copyOne
  where
    copyOne :: FontFile -> IO ()
    copyOne ff = do
        let source = pSrc paths </> T.unpack (ffSrc ff)
            target = pOut paths </> "fonts" </> T.unpack (basename (ffSrc ff))
        createDirectoryIfMissing True (pOut paths </> "fonts")
        copyFile source target
    basename :: Text -> Text
    basename = T.reverse . T.takeWhile (/= '/') . T.reverse

validateFontFiles :: Paths -> SiteConfig -> IO ()
validateFontFiles paths config =
    mapM_ check (fromMaybe [] (fontsFiles (themeFonts (siteTheme config))))
  where
    check :: FontFile -> IO ()
    check ff = do
        exists <- doesFileExist (pSrc paths </> T.unpack (ffSrc ff))
        if exists
            then pure ()
            else ioError (userError ("font file not found in " <> pSrc paths <> ": " <> T.unpack (ffSrc ff)))

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

writePost :: Paths -> SiteConfig -> [(Text, Text)] -> [Post] -> Post -> IO ()
writePost paths config nav allPosts post = do
    let dir = pOut paths </> postsDirName </> T.unpack (postSlug post)
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderPost config nav (neighborsOf post allPosts) post)))

neighborsOf :: Post -> [Post] -> (Maybe Post, Maybe Post)
neighborsOf post posts = case break ((== postSlug post) . postSlug) posts of
    (before, _ : after) -> (listToMaybe (reverse before), listToMaybe after)
    _ -> (Nothing, Nothing)

writeTagPages :: Paths -> SiteConfig -> [(Text, Text)] -> Maybe (Text, CustomPage) -> [Post] -> IO ()
writeTagPages paths config nav mTagsPage posts =
    case mTagsPage of
        Nothing -> pure ()
        Just (_, page) -> do
            let label = fromMaybe defaultTagsLabel (cpTitle page)
                groups = H.groupByTag posts
                tagsDir = pOut paths </> T.unpack (T.dropWhile (== '/') (T.dropWhileEnd (== '/') H.tagUrlPrefix))
            createDirectoryIfMissing True tagsDir
            TIO.writeFile (tagsDir </> "index.html") (TL.toStrict (L.renderText (H.renderTagIndex config nav label groups)))
            mapM_ (writeTagPage config nav tagsDir) groups

writeTagPage :: SiteConfig -> [(Text, Text)] -> FilePath -> (Text, [Post]) -> IO ()
writeTagPage config nav tagsDir (tag, posts) = do
    let dir = tagsDir </> T.unpack tag
    createDirectoryIfMissing True dir
    TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderTagArchive config nav tag posts)))

writeArchive :: Paths -> SiteConfig -> [(Text, Text)] -> Maybe (Text, CustomPage) -> [Post] -> IO ()
writeArchive paths config nav mArchivePage posts =
    case mArchivePage of
        Nothing -> pure ()
        Just (_, page) -> do
            let dir = pOut paths </> "archive"
            createDirectoryIfMissing True dir
            TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (H.renderArchive config nav (fromMaybe defaultArchiveTitle (cpTitle page)) posts)))

writeSearch :: Paths -> SiteConfig -> [(Text, Text)] -> SitePages -> [Post] -> IO ()
writeSearch paths config nav sp posts =
    case spSearch sp of
        Nothing -> pure ()
        Just (_, page) -> do
            let dir = pOut paths </> "search"
            createDirectoryIfMissing True dir
            TIO.writeFile (dir </> "index.html") (TL.toStrict (L.renderText (renderSearch config nav (fromMaybe defaultSearchTitle (cpTitle page)))))
            TIO.writeFile (pOut paths </> "search.json") (renderSearchIndex posts (spNormal sp))

writeFeed :: Paths -> SiteConfig -> [Post] -> IO Bool
writeFeed paths config posts =
    case siteBaseUrl config of
        Just baseUrl
            | not (null posts) -> do
                TIO.writeFile (pOut paths </> "feed.xml") (renderAtom config baseUrl posts)
                pure True
        _ -> pure False

writeSitemap :: Paths -> SiteConfig -> [Post] -> [(Text, Text)] -> Bool -> IO Bool
writeSitemap paths config posts nav includeTags =
    case siteBaseUrl config of
        Just baseUrl
            | not (null posts) -> do
                TIO.writeFile (pOut paths </> "sitemap.xml") (renderSitemap baseUrl posts nav includeTags)
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
    let others = filter (`notElem` ["_post", "_pages", "_scripts", "_data"]) entries
    mapM_ (copyTree (pSrc paths) (pOut paths)) others
    pure (length others)

{- | Write script-generated output files (declared with `output:` in a
page's frontmatter). Written last so they can override built-ins and
static files.
-}
writeScriptOutputs :: Paths -> [(FilePath, Text)] -> IO ()
writeScriptOutputs paths files =
    mapM_ writeOne files
  where
    writeOne :: (FilePath, Text) -> IO ()
    writeOne (rel, content) = do
        let target = pOut paths </> rel
        createDirectoryIfMissing True (takeDirectory target)
        TIO.writeFile target content

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
