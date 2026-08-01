module Search (renderSearchIndex) where

import Data.Aeson (Value (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
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
