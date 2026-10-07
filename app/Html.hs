{-# LANGUAGE TemplateHaskell #-}

module Html (PageMeta (..), groupByTag, layout, postUrl, render404, renderArchive, renderCustomPage, renderIndex, renderPost, renderRedirect, renderTagArchive, renderTagIndex, tagUrl, tagUrlPrefix) where

import Config (Layout (..), SiteConfig (..))
import Control.Exception (throw)
import Control.Monad (when)
import Data.Char (isSpace)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import I18n (fromSiteLang, t, tWith)
import Layout (LayoutError (..), configMap, layoutHelpers, navMap, resolveProgram, siteMap)
import Lucid qualified as L
import Lucid.Base qualified as LB
import Page (CustomPage (..))
import Post (Post (..), TocEntry (..))
import Script (Value (..), formatError, initialEnv, renderResult, runProgram)
import Script.Value (Html (..))

data PageMeta = PageMeta
    { pmTitle :: Text
    , pmOgType :: Text
    , pmOgPath :: Text
    , pmOgDescription :: Maybe Text
    , pmHasMath :: Bool
    }

{- | Render a page by running the site's layout script: the finished
page body (raw HTML) is handed to the script, which returns the whole
document. A runtime error aborts the build.
-}
layout :: SiteConfig -> [(Text, Text)] -> [(Text, Text)] -> Text -> PageMeta -> L.Html () -> L.Html ()
layout cfg navPages footerLinks cssRef meta body =
    case applyLayout cfg navPages footerLinks cssRef meta (TL.toStrict (L.renderText body)) of
        Left errs -> throw (LayoutError errs)
        Right doc -> L.toHtmlRaw doc

applyLayout :: SiteConfig -> [(Text, Text)] -> [(Text, Text)] -> Text -> PageMeta -> Text -> Either [Text] Text
applyLayout cfg navPages footerLinks cssRef meta bodyHtml = do
    (value, _) <- either (Left . pure . formatError) Right (runProgram env program)
    either (Left . pure) Right (renderResult value)
  where
    program = resolveProgram cfg (layProgram (siteLayout cfg))
    env =
        Map.unions
            [ Map.fromList
                [ ("site", siteMap cfg)
                , ("nav-links", navMap navPages)
                , ("footer-links", navMap footerLinks)
                , ("page", pageMetaMap meta)
                , ("content", VHtml (HRaw bodyHtml))
                , ("cssRef", VStr cssRef)
                , ("config", configMap cfg)
                ]
            , layGlobals (siteLayout cfg)
            , Map.fromList (layoutHelpers cfg)
            , initialEnv
            ]

pageMetaMap :: PageMeta -> Value
pageMetaMap meta =
    VMap $
        [ ("title", VStr (pmTitle meta))
        , ("ogType", VStr (pmOgType meta))
        , ("ogPath", VStr (pmOgPath meta))
        , ("hasMath", VBool (pmHasMath meta))
        ]
            <> maybe [] (\d -> [("ogDescription", VStr d)]) (pmOgDescription meta)

renderCustomPage :: SiteConfig -> [(Text, Text)] -> [(Text, Text)] -> Text -> PageMeta -> CustomPage -> L.Html ()
renderCustomPage cfg navPages footerLinks cssRef meta page = layout cfg navPages footerLinks cssRef meta $ L.div_ [L.class_ "post-body"] (L.toHtmlRaw (cpBodyHtml page))

render404 :: SiteConfig -> [(Text, Text)] -> [(Text, Text)] -> Text -> L.Html ()
render404 cfg navPages footerLinks cssRef = layout cfg navPages footerLinks cssRef pageMeta $ L.div_ [L.class_ "not-found"] $ do
    L.h1_ (L.toHtml ("404" :: Text))
    L.p_ (L.toHtml (t lang "notFoundTitle"))
    L.p_ (L.toHtml (t lang "notFoundBody"))
    L.p_ $ L.a_ [L.href_ "/"] (L.toHtml (t lang "backHome"))
  where
    lang = fromSiteLang (siteLang cfg)
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = "404", pmOgType = "website", pmOgPath = "/404.html", pmOgDescription = Nothing, pmHasMath = False}

{- | The archive page: all posts grouped into year sections, newest
first. Posts must already be sorted by date descending.
-}
renderArchive :: SiteConfig -> [(Text, Text)] -> [(Text, Text)] -> Text -> Text -> [Post] -> L.Html ()
renderArchive cfg navPages footerLinks cssRef title posts = layout cfg navPages footerLinks cssRef pageMeta (mapM_ yearSection (groupByYear posts))
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = title, pmOgType = "website", pmOgPath = "/archive/", pmOgDescription = Nothing, pmHasMath = False}
    yearSection :: (Text, [Post]) -> L.Html ()
    yearSection (year, yearPosts) = do
        L.h2_ [L.class_ "archive-year"] (L.toHtml year)
        L.ul_ [L.class_ "post-list"] (mapM_ item yearPosts)
    item :: Post -> L.Html ()
    item post =
        L.li_ [L.class_ "post-item"] $ do
            L.time_ [L.class_ "post-date"] (L.toHtml (postDate post))
            L.a_ [L.href_ (postUrl post)] (L.toHtml (postTitle post))
            renderTags (postTags post)

{- | A standalone redirect page (meta refresh + canonical link) for a
page whose canonical URL differs from its own slug URL.
-}
renderRedirect :: SiteConfig -> Text -> Text -> L.Html ()
renderRedirect cfg cssRef target =
    L.doctype_
        *> L.html_
            [L.lang_ "en"]
            ( do
                L.head_ $ do
                    L.meta_ [L.charset_ "utf-8"]
                    L.meta_ [LB.makeAttribute "http-equiv" "refresh", L.content_ ("0; url=" <> target)]
                    L.link_ [L.rel_ "canonical", L.href_ target]
                    L.link_ [L.rel_ "stylesheet", L.href_ cssRef]
                    L.title_ (L.toHtml (tWith lang "redirectingTo" [target]))
                L.body_ $ do
                    L.p_ $ L.toHtml (tWith lang "redirectingTo" [target])
                    L.a_ [L.href_ target] (L.toHtml target)
            )
  where
    lang = fromSiteLang (siteLang cfg)

renderIndex :: SiteConfig -> [(Text, Text)] -> [(Text, Text)] -> Text -> Maybe CustomPage -> [Post] -> L.Html ()
renderIndex cfg navPages footerLinks cssRef mIndexPage posts = layout cfg navPages footerLinks cssRef pageMeta $ do
    case mIndexPage of
        Nothing -> L.ul_ [L.class_ "post-list"] (mapM_ item posts)
        Just page -> do
            L.div_ [L.class_ "post-body index-body"] (L.toHtmlRaw (cpBodyHtml page))
            L.h2_ [L.class_ "index-title"] (L.toHtml (t lang "sectionTitle"))
            L.ul_ [L.class_ "post-list"] (mapM_ item posts)
  where
    lang = fromSiteLang (siteLang cfg)
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = siteName cfg, pmOgType = "website", pmOgPath = "/", pmOgDescription = Nothing, pmHasMath = False}
    item :: Post -> L.Html ()
    item post =
        L.li_ [L.class_ "post-item"] $ do
            L.time_ [L.class_ "post-date"] (L.toHtml (postDate post))
            L.a_ [L.href_ (postUrl post)] (L.toHtml (postTitle post))
            maybe (pure ()) renderDescription (postDescription post)
            renderTags (postTags post)

renderPost :: SiteConfig -> [(Text, Text)] -> [(Text, Text)] -> Text -> (Maybe Post, Maybe Post) -> Post -> L.Html ()
renderPost cfg navPages footerLinks cssRef neighbors post = layout cfg navPages footerLinks cssRef pageMeta $ L.article_ $ do
    L.h1_ (L.toHtml (postTitle post))
    L.p_ [L.class_ "post-meta"] $ do
        L.time_ (L.toHtml (postDate post))
        renderTags (postTags post)
        L.span_ [L.class_ "post-meta-time"] (L.toHtml (readingLabel cfg post))
    when (postShowToc post && not (null (postToc post))) (renderToc (postToc post))
    L.div_ [L.class_ "post-body"] (L.toHtmlRaw (postBodyHtml post))
    renderPostNav cfg neighbors
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = postTitle post, pmOgType = "article", pmOgPath = postUrl post, pmOgDescription = postDescription post, pmHasMath = postHasMath post}

renderToc :: [TocEntry] -> L.Html ()
renderToc entries = L.nav_ [L.class_ "toc"] $ L.ul_ $ mapM_ item entries
  where
    item :: TocEntry -> L.Html ()
    item entry = L.li_ [L.class_ ("toc-level-" <> T.pack (show (tocLevel entry)))] $ L.a_ [L.href_ ("#" <> tocId entry)] (L.toHtml (tocTitle entry))

renderPostNav :: SiteConfig -> (Maybe Post, Maybe Post) -> L.Html ()
renderPostNav cfg (prev, next) = case (prev, next) of
    (Nothing, Nothing) -> pure ()
    _ -> L.nav_ [L.class_ "post-nav"] $ do
        maybe (pure ()) renderPrev prev
        maybe (pure ()) renderNext next
  where
    lang = fromSiteLang (siteLang cfg)
    renderPrev :: Post -> L.Html ()
    renderPrev p = L.div_ [L.class_ "post-nav-prev"] $ do
        L.span_ [L.class_ "post-nav-label"] (L.toHtml (t lang "prevPost"))
        L.a_ [L.href_ (postUrl p)] (L.toHtml (postTitle p))
    renderNext :: Post -> L.Html ()
    renderNext p = L.div_ [L.class_ "post-nav-next"] $ do
        L.span_ [L.class_ "post-nav-label"] (L.toHtml (t lang "nextPost"))
        L.a_ [L.href_ (postUrl p)] (L.toHtml (postTitle p))

readingLabel :: SiteConfig -> Post -> Text
readingLabel cfg post =
    tWith (fromSiteLang (siteLang cfg)) "readingTime" [T.pack (show (readingMinutes post))]

readingMinutes :: Post -> Int
readingMinutes post =
    let text = postText post
        chars = T.length (T.filter (not . isSpace) text)
        wordCount = length (T.words text)
     in max 1 (max (chars `div` 400) (wordCount `div` 200))

renderTagIndex :: SiteConfig -> [(Text, Text)] -> [(Text, Text)] -> Text -> Text -> [(Text, [Post])] -> L.Html ()
renderTagIndex cfg navPages footerLinks cssRef label groups = layout cfg navPages footerLinks cssRef pageMeta $ L.ul_ [L.class_ "tag-list"] (mapM_ item groups)
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = label, pmOgType = "website", pmOgPath = tagUrlPrefix, pmOgDescription = Nothing, pmHasMath = False}
    item :: (Text, [Post]) -> L.Html ()
    item (tag, posts) =
        L.li_ [L.class_ "tag-item", LB.makeAttribute "style" ("--tag-count: " <> T.pack (show (length posts)))] $ do
            L.a_ [L.class_ "tag-name", L.href_ (tagUrl tag)] (L.toHtml tag)
            L.span_ [L.class_ "tag-count"] (L.toHtml ("(" <> T.pack (show (length posts)) <> ")"))

renderTagArchive :: SiteConfig -> [(Text, Text)] -> [(Text, Text)] -> Text -> Text -> [Post] -> L.Html ()
renderTagArchive cfg navPages footerLinks cssRef tag posts = layout cfg navPages footerLinks cssRef pageMeta $ L.article_ $ do
    L.h1_ (L.toHtml tag)
    L.ul_ [L.class_ "post-list"] (mapM_ item posts)
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = tag, pmOgType = "website", pmOgPath = tagUrl tag, pmOgDescription = Nothing, pmHasMath = False}
    item :: Post -> L.Html ()
    item post =
        L.li_ [L.class_ "post-item"] $ do
            L.time_ [L.class_ "post-date"] (L.toHtml (postDate post))
            L.a_ [L.href_ (postUrl post)] (L.toHtml (postTitle post))
            maybe (pure ()) renderDescription (postDescription post)

groupByTag :: [Post] -> [(Text, [Post])]
groupByTag posts =
    Map.toAscList (Map.fromListWith (flip (++)) (concatMap pair posts))
  where
    pair :: Post -> [(Text, [Post])]
    pair post = [(tag, [post]) | tag <- postTags post]

{- | Group posts into year sections, preserving the input order (posts
are date-descending, so years come out newest-first).
-}
groupByYear :: [Post] -> [(Text, [Post])]
groupByYear = reverse . foldl step []
  where
    step :: [(Text, [Post])] -> Post -> [(Text, [Post])]
    step [] post = [(yearOf post, [post])]
    step (group@(year, yearPosts) : rest) post
        | yearOf post == year = (year, yearPosts <> [post]) : rest
        | otherwise = (yearOf post, [post]) : group : rest
    yearOf :: Post -> Text
    yearOf post = T.take 4 (postDate post)

tagUrlPrefix :: Text
tagUrlPrefix = "/tags/"

tagUrl :: Text -> Text
tagUrl tag = tagUrlPrefix <> tag <> "/"

renderTags :: [Text] -> L.Html ()
renderTags [] = pure ()
renderTags tags = L.span_ [L.class_ "post-tags"] (go tags)
  where
    go :: [Text] -> L.Html ()
    go [single] = renderTag single
    go (tag : rest) = renderTag tag >> L.span_ [L.class_ "post-tag-sep"] (L.toHtml (" · " :: Text)) >> go rest
    go [] = pure ()
    renderTag :: Text -> L.Html ()
    renderTag tag = L.a_ [L.class_ "post-tag", L.href_ (tagUrl tag)] (L.toHtml tag)

renderDescription :: Text -> L.Html ()
renderDescription description = L.p_ [L.class_ "post-desc"] (L.toHtml description)

postUrl :: Post -> Text
postUrl post = "/posts/" <> postSlug post <> "/"
