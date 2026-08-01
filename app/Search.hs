module Search (renderSearch, renderSearchIndex) where

import Config (SiteConfig (..))
import Data.Aeson (Value (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Html (PageMeta (..))
import Html qualified as H
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
    L.input_ [L.class_ "search-input", L.type_ "search", LB.makeAttribute "placeholder" "Search…", LB.makeAttribute "autofocus" ""]
    L.ul_ [L.class_ "post-list", L.id_ "search-results"] (pure ())
    L.script_ (searchScript :: Text)
  where
    pageMeta :: PageMeta
    pageMeta = PageMeta{pmTitle = title, pmOgType = "website", pmOgPath = "/search/", pmOgDescription = Nothing, pmHasMath = False}

searchScript :: Text
searchScript =
    T.unlines
        [ "document.addEventListener(\"DOMContentLoaded\", function () {"
        , " if (window.buroguSearch) {"
        , "  window.buroguSearch({ url: \"/search.json\" });"
        , "  return;"
        , " }"
        , " var input = document.querySelector(\".search-input\");"
        , " var list = document.getElementById(\"search-results\");"
        , " function highlight(node, text, q) {"
        , "  var lower = text.toLowerCase();"
        , "  var i = lower.indexOf(q);"
        , "  if (i === -1) { node.textContent = text; return; }"
        , "  if (i > 0) { node.appendChild(document.createTextNode(text.slice(0, i))); }"
        , "  var mark = document.createElement(\"mark\");"
        , "  mark.textContent = text.slice(i, i + q.length);"
        , "  node.appendChild(mark);"
        , "  var after = text.slice(i + q.length);"
        , "  if (after) { node.appendChild(document.createTextNode(after)); }"
        , " }"
        , " function snippetAround(text, q) {"
        , "  var lower = text.toLowerCase();"
        , "  var i = lower.indexOf(q);"
        , "  if (i === -1) { return text.slice(0, 80); }"
        , "  var start = Math.max(0, i - 20);"
        , "  var end = Math.min(text.length, i + q.length + 60);"
        , "  return (start > 0 ? \"…\" : \"\") + text.slice(start, end);"
        , " }"
        , " fetch(\"/search.json\").then(function (r) { return r.json(); }).then(function (index) {"
        , "  input.addEventListener(\"input\", function () {"
        , "   var q = input.value.trim().toLowerCase();"
        , "   while (list.firstChild) { list.removeChild(list.firstChild); }"
        , "   if (!q) { return; }"
        , "   index.forEach(function (entry) {"
        , "    var hay = (entry.title + \" \" + entry.text).toLowerCase();"
        , "    if (hay.indexOf(q) === -1) { return; }"
        , "    var li = document.createElement(\"li\");"
        , "    li.className = \"post-item\";"
        , "    var a = document.createElement(\"a\");"
        , "    a.href = entry.url;"
        , "    highlight(a, entry.title, q);"
        , "    li.appendChild(a);"
        , "    var p = document.createElement(\"p\");"
        , "    p.className = \"post-desc\";"
        , "    if (entry.date) { p.appendChild(document.createTextNode(entry.date + \" — \")); }"
        , "    highlight(p, snippetAround(entry.text, q), q);"
        , "    li.appendChild(p);"
        , "    list.appendChild(li);"
        , "   });"
        , "  });"
        , " });"
        , "});"
        ]
