module Html (groupByTag, renderIndex, renderPost, renderTagArchive, renderTagIndex, tagUrl) where

import Config (SiteConfig (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Lucid qualified as L
import Post (Post (..))

layout :: SiteConfig -> Text -> L.Html () -> L.Html ()
layout cfg pageTitle body =
    L.doctype_
        *> L.html_
            [L.lang_ (siteLang cfg)]
            ( do
                L.head_ $ do
                    L.meta_ [L.charset_ "utf-8"]
                    L.meta_ [L.name_ "viewport", L.content_ "width=device-width, initial-scale=1"]
                    L.meta_ [L.name_ "description", L.content_ (siteDescription cfg)]
                    L.title_ (L.toHtml pageTitle)
                    L.link_ [L.rel_ "stylesheet", L.href_ "/style.css"]
                L.body_ $ do
                    L.header_ [L.class_ "site-header"] $ L.nav_ $ do
                        L.a_ [L.href_ "/"] (L.toHtml (siteName cfg))
                        L.a_ [L.href_ "/tags/"] (L.toHtml ("Tags" :: Text))
                    L.main_ body
                    L.footer_ [L.class_ "site-footer"] $ L.p_ (L.toHtml ("© " <> siteAuthor cfg))
            )

renderIndex :: SiteConfig -> [Post] -> L.Html ()
renderIndex cfg posts = layout cfg (siteName cfg) $ L.ul_ [L.class_ "post-list"] (mapM_ item posts)
  where
    item :: Post -> L.Html ()
    item post =
        L.li_ [L.class_ "post-item"] $ do
            L.time_ [L.class_ "post-date"] (L.toHtml (postDate post))
            L.a_ [L.href_ (postUrl post)] (L.toHtml (postTitle post))
            renderTags (postTags post)
            maybe (pure ()) renderDescription (postDescription post)

renderPost :: SiteConfig -> Post -> L.Html ()
renderPost cfg post = layout cfg (postTitle post) $ L.article_ $ do
    L.h1_ (L.toHtml (postTitle post))
    L.p_ [L.class_ "post-meta"] $ do
        L.time_ (L.toHtml (postDate post))
        renderTags (postTags post)
    L.div_ [L.class_ "post-body"] (L.toHtmlRaw (postBodyHtml post))

renderTagIndex :: SiteConfig -> [(Text, [Post])] -> L.Html ()
renderTagIndex cfg groups = layout cfg ("Tags" :: Text) $ L.ul_ [L.class_ "tag-list"] (mapM_ item groups)
  where
    item :: (Text, [Post]) -> L.Html ()
    item (tag, posts) =
        L.li_ [L.class_ "tag-item"] $ do
            L.a_ [L.class_ "tag-name", L.href_ (tagUrl tag)] (L.toHtml tag)
            L.span_ [L.class_ "tag-count"] (L.toHtml ("(" <> T.pack (show (length posts)) <> ")"))

renderTagArchive :: SiteConfig -> Text -> [Post] -> L.Html ()
renderTagArchive cfg tag posts = layout cfg ("Tag: " <> tag) $ L.article_ $ do
    L.h1_ (L.toHtml tag)
    L.ul_ [L.class_ "post-list"] (mapM_ item posts)
  where
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

tagUrl :: Text -> Text
tagUrl tag = "/tags/" <> tag <> "/"

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
