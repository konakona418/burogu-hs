module Feed (feedUrl, renderAtom) where

import Config (SiteConfig (..))
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Html (postUrl)
import Lucid qualified as L
import Lucid.Base qualified as LB
import Post (Post (..))

atomNamespace :: Text
atomNamespace = "http://www.w3.org/2005/Atom"

xmlDeclaration :: Text
xmlDeclaration = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"

feedUrl :: Text -> Text
feedUrl baseUrl = baseUrl <> "/feed.xml"

renderAtom :: SiteConfig -> Text -> [Post] -> Text
renderAtom config baseUrl posts =
    TL.toStrict (L.renderText (L.toHtmlRaw xmlDeclaration *> feedXml))
  where
    siteUrl = baseUrl <> "/"
    feedXml = feed [LB.makeAttribute "xmlns" atomNamespace] $ do
        title [] (L.toHtml (siteName config))
        link [L.rel_ "alternate", L.href_ siteUrl] (pure ())
        id_ [] (L.toHtml siteUrl)
        updated [] (L.toHtml (postDate (head posts)))
        author [] $ name [] (L.toHtml (siteAuthor config))
        mapM_ renderEntry posts
    renderEntry post = entry [] $ do
        title [] (L.toHtml (postTitle post))
        link [L.rel_ "alternate", L.href_ (baseUrl <> postUrl post)] (pure ())
        id_ [] (L.toHtml (baseUrl <> postUrl post))
        updated [] (L.toHtml (postDate post))
        mapM_ renderCategory (postTags post)
        maybe (pure ()) renderSummary (postDescription post)
        content [L.type_ "html"] (L.toHtml (postBodyHtml post))
    renderCategory tag = category [LB.makeAttribute "term" tag] (pure ())
    renderSummary description = summary [] (L.toHtml description)

feed :: [L.Attribute] -> L.Html () -> L.Html ()
feed = L.term "feed"

entry :: [L.Attribute] -> L.Html () -> L.Html ()
entry = L.term "entry"

title :: [L.Attribute] -> L.Html () -> L.Html ()
title = L.term "title"

link :: [L.Attribute] -> L.Html () -> L.Html ()
link = L.term "link"

id_ :: [L.Attribute] -> L.Html () -> L.Html ()
id_ = L.term "id"

updated :: [L.Attribute] -> L.Html () -> L.Html ()
updated = L.term "updated"

author :: [L.Attribute] -> L.Html () -> L.Html ()
author = L.term "author"

name :: [L.Attribute] -> L.Html () -> L.Html ()
name = L.term "name"

category :: [L.Attribute] -> L.Html () -> L.Html ()
category = L.term "category"

summary :: [L.Attribute] -> L.Html () -> L.Html ()
summary = L.term "summary"

content :: [L.Attribute] -> L.Html () -> L.Html ()
content = L.term "content"
