module Sitemap (renderSitemap) where

import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Html (groupByTag, postUrl, tagUrl)
import Lucid qualified as L
import Lucid.Base qualified as LB
import Post (Post (..))

sitemapNamespace :: Text
sitemapNamespace = "http://www.sitemaps.org/schemas/sitemap/0.9"

xmlDeclaration :: Text
xmlDeclaration = "<?xml version=\"1.0\" encoding=\"utf-8\"?>"

renderSitemap :: Text -> [Post] -> Text
renderSitemap baseUrl posts =
    TL.toStrict (L.renderText (L.toHtmlRaw xmlDeclaration *> sitemapXml))
  where
    sitemapXml = urlset [LB.makeAttribute "xmlns" sitemapNamespace] $ do
        urlEntry siteUrl Nothing
        mapM_ (\p -> urlEntry (baseUrl <> postUrl p) (Just (postDate p))) posts
        urlEntry (baseUrl <> "/tags/") Nothing
        mapM_ (\t -> urlEntry (baseUrl <> tagUrl t) Nothing) (map fst (groupByTag posts))
        urlEntry (baseUrl <> "/feed.xml") Nothing
    siteUrl = baseUrl <> "/"
    urlEntry :: Text -> Maybe Text -> L.Html ()
    urlEntry loc mDate = url [] $ do
        locEl [] (L.toHtml loc)
        maybe (pure ()) (\d -> lastmodEl [] (L.toHtml d)) mDate

urlset :: [L.Attribute] -> L.Html () -> L.Html ()
urlset = L.term "urlset"

url :: [L.Attribute] -> L.Html () -> L.Html ()
url = L.term "url"

locEl :: [L.Attribute] -> L.Html () -> L.Html ()
locEl = L.term "loc"

lastmodEl :: [L.Attribute] -> L.Html () -> L.Html ()
lastmodEl = L.term "lastmod"
