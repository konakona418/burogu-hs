{-# LANGUAGE TemplateHaskell #-}

module Search (renderSearch, renderSearchIndex) where

import Config (SiteConfig (..))
import Data.Aeson (Value (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Html (PageMeta (..))
import Html qualified as H
import I18n (fromSiteLang, t)
import Lucid qualified as L
import Lucid.Base qualified as LB
import Page (CustomPage (..))
import Post (Post (..))

{- | Build the search index as JSON: one entry per post (with date and
tags) and per normal page. Special pages are excluded; callers decide
which pages are normal.
-}
renderSearchIndex :: [Post] -> [(Text, CustomPage)] -> Text
renderSearchIndex posts pages =
    decodeUtf8 (BL.toStrict (encode (map postEntry posts <> map pageEntry pages)))
  where
    postEntry :: Post -> Value
    postEntry post =
        object
            [ "title" .= postTitle post
            , "url" .= ("/posts/" <> postSlug post <> "/")
            , "date" .= postDate post
            , "tags" .= postTags post
            , "text" .= postText post
            ]
    pageEntry :: (Text, CustomPage) -> Value
    pageEntry (slug, page) =
        object
            [ "title" .= maybe slug id (cpTitle page)
            , "url" .= ("/" <> slug <> "/")
            , "text" .= cpText page
            ]

{- | The search page: an input and a results container, wired up by an
inline script that searches /search.json client-side. The script is a
no-op when a user script (e.g. theme.extraJs) defines window.buroguSearch.
-}
renderSearch :: SiteConfig -> [(Text, Text)] -> Text -> L.Html ()
renderSearch cfg navPages title = H.layout cfg navPages pageMeta $ do
    L.input_ [L.class_ "search-input", L.type_ "search", LB.makeAttribute "placeholder" (t (fromSiteLang (siteLang cfg)) "searchPlaceholder"), LB.makeAttribute "autofocus" ""]
    L.p_ [L.class_ "search-no-results", LB.makeAttribute "hidden" ""] (L.toHtml (t (fromSiteLang (siteLang cfg)) "noResults"))
    L.ul_ [L.class_ "post-list", L.id_ "search-results"] (pure ())
    L.script_ (searchScript :: Text)
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = title, pmOgType = "website", pmOgPath = "/search/", pmOgDescription = Nothing, pmHasMath = False}

searchScript :: Text
searchScript = decodeUtf8 $(embedFile "js/search.js")
