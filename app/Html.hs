module Html (PageMeta (..), groupByTag, postUrl, renderIndex, renderPost, renderTagArchive, renderTagIndex, tagUrl, tagUrlPrefix) where

import Config (SiteConfig (..), Theme (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Lucid qualified as L
import Lucid.Base qualified as LB
import Post (Post (..))
import Text.Pandoc.Options (defaultKaTeXURL, defaultMathJaxURL)

data PageMeta = PageMeta
    { pmTitle :: Text
    , pmOgType :: Text
    , pmOgPath :: Text
    , pmOgDescription :: Maybe Text
    , pmHasMath :: Bool
    }

layout :: SiteConfig -> PageMeta -> L.Html () -> L.Html ()
layout cfg meta body =
    L.doctype_
        *> L.html_
            [L.lang_ (siteLang cfg)]
            ( do
                L.head_ $ do
                    L.meta_ [L.charset_ "utf-8"]
                    L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
                    L.meta_ [L.name_ "description", L.content_ (siteDescription cfg)]
                    L.title_ (L.toHtml (pmTitle meta))
                    renderOg cfg meta
                    renderMath cfg meta
                    L.link_ [L.rel_ "stylesheet", L.href_ "/style.css"]
                L.body_ $ do
                    L.header_ [L.class_ "site-header"] $ L.nav_ $ do
                        L.a_ [L.href_ "/"] (L.toHtml (siteName cfg))
                        L.a_ [L.href_ "/tags/"] (L.toHtml (siteTagsLabel cfg))
                    L.main_ body
                    L.footer_ [L.class_ "site-footer"] $ L.p_ (L.toHtml ("© " <> siteAuthor cfg))
            )

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

renderIndex :: SiteConfig -> [Post] -> L.Html ()
renderIndex cfg posts = layout cfg pageMeta $ L.ul_ [L.class_ "post-list"] (mapM_ item posts)
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

renderPost :: SiteConfig -> Post -> L.Html ()
renderPost cfg post = layout cfg pageMeta $ L.article_ $ do
    L.h1_ (L.toHtml (postTitle post))
    L.p_ [L.class_ "post-meta"] $ do
        L.time_ (L.toHtml (postDate post))
        renderTags (postTags post)
    L.div_ [L.class_ "post-body"] (L.toHtmlRaw (postBodyHtml post))
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = postTitle post, pmOgType = "article", pmOgPath = postUrl post, pmOgDescription = postDescription post, pmHasMath = postHasMath post}

renderTagIndex :: SiteConfig -> [(Text, [Post])] -> L.Html ()
renderTagIndex cfg groups = layout cfg pageMeta $ L.ul_ [L.class_ "tag-list"] (mapM_ item groups)
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = siteTagsLabel cfg, pmOgType = "website", pmOgPath = tagUrlPrefix, pmOgDescription = Nothing, pmHasMath = False}
    item :: (Text, [Post]) -> L.Html ()
    item (tag, posts) =
        L.li_ [L.class_ "tag-item"] $ do
            L.a_ [L.class_ "tag-name", L.href_ (tagUrl tag)] (L.toHtml tag)
            L.span_ [L.class_ "tag-count"] (L.toHtml ("(" <> T.pack (show (length posts)) <> ")"))

renderTagArchive :: SiteConfig -> Text -> [Post] -> L.Html ()
renderTagArchive cfg tag posts = layout cfg pageMeta $ L.article_ $ do
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
    go (t : rest) = renderTag t >> L.toHtml (" · " :: Text) >> go rest
    go [] = pure ()
    renderTag :: Text -> L.Html ()
    renderTag tag = L.a_ [L.class_ "post-tag", L.href_ (tagUrl tag)] (L.toHtml tag)

renderDescription :: Text -> L.Html ()
renderDescription description = L.p_ [L.class_ "post-desc"] (L.toHtml description)

postUrl :: Post -> Text
postUrl post = "/posts/" <> postSlug post <> "/"
