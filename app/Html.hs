module Html (PageMeta (..), groupByTag, layout, postUrl, render404, renderArchive, renderCustomPage, renderIndex, renderPost, renderRedirect, renderTagArchive, renderTagIndex, tagUrl, tagUrlPrefix) where

import Config (SiteConfig (..), Theme (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lucid qualified as L
import Lucid.Base qualified as LB
import Page (CustomPage (..))
import Post (Post (..))
import Text.Pandoc.Options (defaultKaTeXURL, defaultMathJaxURL)

data PageMeta = PageMeta
    { pmTitle :: Text
    , pmOgType :: Text
    , pmOgPath :: Text
    , pmOgDescription :: Maybe Text
    , pmHasMath :: Bool
    }

layout :: SiteConfig -> [(Text, Text)] -> PageMeta -> L.Html () -> L.Html ()
layout cfg navPages meta body =
    L.doctype_
        *> L.html_
            [L.lang_ (siteLang cfg)]
            ( do
                L.head_ $ do
                    L.meta_ [L.charset_ "utf-8"]
                    L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1, viewport-fit=cover"]
                    L.meta_ [L.name_ "description", L.content_ (siteDescription cfg)]
                    L.title_ (L.toHtml (pmTitle meta))
                    renderOg cfg meta
                    renderMath cfg meta
                    renderExtraJs cfg
                    L.link_ [L.rel_ "stylesheet", L.href_ "/style.css"]
                L.body_ $ do
                    L.header_ [L.class_ "site-header"] $ L.nav_ $ do
                        L.a_ [L.class_ "site-name", L.href_ "/"] (L.toHtml (siteName cfg))
                        mapM_ renderNavPage navPages
                    L.main_ body
                    L.footer_ [L.class_ "site-footer"] $ L.p_ $ do
                        L.toHtml (siteCopyright cfg)
                        maybe (pure ()) renderCredit (siteGeneratedBy cfg)
            )
  where
    renderCredit :: Text -> L.Html ()
    renderCredit credit = L.toHtml (" · " :: Text) >> L.toHtml credit

renderOg :: SiteConfig -> PageMeta -> L.Html ()
renderOg cfg meta = do
    L.meta_ [LB.makeAttribute "property" "og:title", L.content_ (pmTitle meta)]
    L.meta_ [LB.makeAttribute "property" "og:type", L.content_ (pmOgType meta)]
    L.meta_ [LB.makeAttribute "property" "og:site_name", L.content_ (siteName cfg)]
    L.meta_ [LB.makeAttribute "property" "og:description", L.content_ (fromMaybe (siteDescription cfg) (pmOgDescription meta))]
    maybe (pure ()) renderOgUrl (siteBaseUrl cfg)
  where
    renderOgUrl :: Text -> L.Html ()
    renderOgUrl baseUrl = L.meta_ [LB.makeAttribute "property" "og:url", L.content_ (baseUrl <> pmOgPath meta)]

renderMath :: SiteConfig -> PageMeta -> L.Html ()
renderMath cfg meta =
    case (themeMath theme, pmHasMath meta) of
        ("mathjax", True) ->
            L.script_ [L.defer_ "", L.src_ (fromMaybe defaultMathJaxURL (themeMathUrl theme)), L.type_ "text/javascript"] (pure () :: L.Html ())
        ("katex", True) -> do
            L.link_ [L.rel_ "stylesheet", L.href_ (katexBase <> "katex.min.css")]
            L.script_ [L.defer_ "", L.src_ (katexBase <> "katex.min.js")] (pure () :: L.Html ())
            L.script_ (katexScript :: Text)
        _ -> pure ()
  where
    theme = siteTheme cfg
    katexBase = fromMaybe defaultKaTeXURL (themeMathUrl theme)

renderExtraJs :: SiteConfig -> L.Html ()
renderExtraJs cfg = mapM_ scriptTag (themeExtraJs (siteTheme cfg))
  where
    scriptTag :: Text -> L.Html ()
    scriptTag file = L.script_ [L.defer_ "", L.src_ ("/" <> file)] (pure () :: L.Html ())

katexScript :: Text
katexScript =
    T.unlines
        [ "document.addEventListener(\"DOMContentLoaded\", function () {"
        , " var mathElements = document.getElementsByClassName(\"math\");"
        , " var macros = [];"
        , " for (var i = 0; i < mathElements.length; i++) {"
        , "  var texText = mathElements[i].firstChild;"
        , "  if (mathElements[i].tagName == \"SPAN\") {"
        , "   katex.render(texText.data, mathElements[i], {"
        , "    displayMode: mathElements[i].classList.contains('display'),"
        , "    throwOnError: false,"
        , "    macros: macros,"
        , "    fleqn: false"
        , "   });"
        , "}}});"
        ]

renderNavPage :: (Text, Text) -> L.Html ()
renderNavPage (label, href) = L.a_ [L.href_ href] (L.toHtml label)

renderCustomPage :: SiteConfig -> [(Text, Text)] -> PageMeta -> CustomPage -> L.Html ()
renderCustomPage cfg navPages meta page = layout cfg navPages meta $ L.div_ [L.class_ "post-body"] (L.toHtmlRaw (cpBodyHtml page))

render404 :: SiteConfig -> [(Text, Text)] -> L.Html ()
render404 cfg navPages = layout cfg navPages pageMeta $ L.div_ [L.class_ "not-found"] $ do
    L.h1_ (L.toHtml ("404" :: Text))
    L.p_ (L.toHtml ("Page not found." :: Text))
    L.p_ $ L.a_ [L.href_ "/"] (L.toHtml ("Back to the home page" :: Text))
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = "404", pmOgType = "website", pmOgPath = "/404.html", pmOgDescription = Nothing, pmHasMath = False}

{- | The archive page: all posts grouped into year sections, newest
first. Posts must already be sorted by date descending.
-}
renderArchive :: SiteConfig -> [(Text, Text)] -> Text -> [Post] -> L.Html ()
renderArchive cfg navPages title posts = layout cfg navPages pageMeta (mapM_ yearSection (groupByYear posts))
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
renderRedirect :: Text -> L.Html ()
renderRedirect target =
    L.doctype_
        *> L.html_
            [L.lang_ "en"]
            ( do
                L.head_ $ do
                    L.meta_ [L.charset_ "utf-8"]
                    L.meta_ [LB.makeAttribute "http-equiv" "refresh", L.content_ ("0; url=" <> target)]
                    L.link_ [L.rel_ "canonical", L.href_ target]
                    L.title_ (L.toHtml ("Redirecting to " <> target))
                L.body_ $ do
                    L.p_ $ L.toHtml ("Redirecting to " :: Text)
                    L.a_ [L.href_ target] (L.toHtml target)
            )

renderIndex :: SiteConfig -> [(Text, Text)] -> [Post] -> L.Html ()
renderIndex cfg navPages posts = layout cfg navPages pageMeta $ L.ul_ [L.class_ "post-list"] (mapM_ item posts)
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = siteName cfg, pmOgType = "website", pmOgPath = "/", pmOgDescription = Nothing, pmHasMath = False}
    item :: Post -> L.Html ()
    item post =
        L.li_ [L.class_ "post-item"] $ do
            L.time_ [L.class_ "post-date"] (L.toHtml (postDate post))
            L.a_ [L.href_ (postUrl post)] (L.toHtml (postTitle post))
            renderTags (postTags post)
            maybe (pure ()) renderDescription (postDescription post)

renderPost :: SiteConfig -> [(Text, Text)] -> Post -> L.Html ()
renderPost cfg navPages post = layout cfg navPages pageMeta $ L.article_ $ do
    L.h1_ (L.toHtml (postTitle post))
    L.p_ [L.class_ "post-meta"] $ do
        L.time_ (L.toHtml (postDate post))
        renderTags (postTags post)
    L.div_ [L.class_ "post-body"] (L.toHtmlRaw (postBodyHtml post))
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = postTitle post, pmOgType = "article", pmOgPath = postUrl post, pmOgDescription = postDescription post, pmHasMath = postHasMath post}

renderTagIndex :: SiteConfig -> [(Text, Text)] -> Text -> [(Text, [Post])] -> L.Html ()
renderTagIndex cfg navPages label groups = layout cfg navPages pageMeta $ L.ul_ [L.class_ "tag-list"] (mapM_ item groups)
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = label, pmOgType = "website", pmOgPath = tagUrlPrefix, pmOgDescription = Nothing, pmHasMath = False}
    item :: (Text, [Post]) -> L.Html ()
    item (tag, posts) =
        L.li_ [L.class_ "tag-item", LB.makeAttribute "style" ("--tag-count: " <> T.pack (show (length posts)))] $ do
            L.a_ [L.class_ "tag-name", L.href_ (tagUrl tag)] (L.toHtml tag)
            L.span_ [L.class_ "tag-count"] (L.toHtml ("(" <> T.pack (show (length posts)) <> ")"))

renderTagArchive :: SiteConfig -> [(Text, Text)] -> Text -> [Post] -> L.Html ()
renderTagArchive cfg navPages tag posts = layout cfg navPages pageMeta $ L.article_ $ do
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
    go (t : rest) = renderTag t >> L.span_ [L.class_ "post-tag-sep"] (L.toHtml (" · " :: Text)) >> go rest
    go [] = pure ()
    renderTag :: Text -> L.Html ()
    renderTag tag = L.a_ [L.class_ "post-tag", L.href_ (tagUrl tag)] (L.toHtml tag)

renderDescription :: Text -> L.Html ()
renderDescription description = L.p_ [L.class_ "post-desc"] (L.toHtml description)

postUrl :: Post -> Text
postUrl post = "/posts/" <> postSlug post <> "/"
