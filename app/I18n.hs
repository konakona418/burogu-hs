{-# LANGUAGE TemplateHaskell #-}

module I18n (UILang (..), fromSiteLang, messages, t, tWith) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.FileEmbed (embedFile)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Yaml (ParseException, decodeEither')

data UILang = En | Zh | ZhHant | Ja
    deriving (Eq, Show)

{- | The UI language for a site, derived from siteLang. The mapping
mirrors the manual's locale rules: zh_TW/zh_HK/zh_MO/zh-Hant select
Traditional Chinese, other zh* locales Simplified Chinese, ja*
Japanese, everything else English.
-}
fromSiteLang :: Text -> UILang
fromSiteLang lang =
    let lower = T.toLower lang
     in if "ja" `T.isPrefixOf` lower
            then Ja
            else
                if "zh" `T.isPrefixOf` lower
                    then
                        if any (`T.isInfixOf` lower) ["hant", "tw", "hk", "mo"]
                            then ZhHant
                            else Zh
                    else En

{- | The localized string for a key, falling back to English when the
language has no entry (key-set consistency across languages is caught
by the completeness test).
-}
t :: UILang -> Text -> Text
t lang key = fromMaybe "" (lookup key (messages lang))

-- | Like t, with {0}/{1}... placeholders replaced by the arguments.
tWith :: UILang -> Text -> [Text] -> Text
tWith lang key args = foldr replaceArg (t lang key) (zip [0 :: Int ..] args)
  where
    replaceArg :: (Int, Text) -> Text -> Text
    replaceArg (i, arg) acc = T.replace ("{" <> T.pack (show i) <> "}") arg acc

messages :: UILang -> [(Text, Text)]
messages lang = parseTable (content lang)
  where
    content :: UILang -> Text
    content En = enContent
    content Zh = zhContent
    content ZhHant = zhHantContent
    content Ja = jaContent

parseTable :: Text -> [(Text, Text)]
parseTable content = case decodeEither' (encodeUtf8 content) :: Either ParseException Value of
    Right (Object o) -> [(K.toText k, v) | (k, String v) <- KM.toList o]
    _ -> []

enContent :: Text
enContent = decodeUtf8 $(embedFile "i18n/en.yaml")

zhContent :: Text
zhContent = decodeUtf8 $(embedFile "i18n/zh.yaml")

zhHantContent :: Text
zhHantContent = decodeUtf8 $(embedFile "i18n/zh-Hant.yaml")

jaContent :: Text
jaContent = decodeUtf8 $(embedFile "i18n/ja.yaml")
