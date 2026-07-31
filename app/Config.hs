module Config (SiteConfig (..), Theme (..), loadConfig) where

import Control.Exception (IOException, catch)
import Data.Aeson (FromJSON (..), withObject, (.:?))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Yaml (decodeEither', prettyPrintParseException)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.IO.Error (isDoesNotExistError)
import Text.Pandoc.Highlighting (highlightingStyles)

data SiteConfig = SiteConfig
    { siteName :: Text
    , siteAuthor :: Text
    , siteDescription :: Text
    , siteLang :: Text
    , siteBaseUrl :: Maybe Text
    , siteTheme :: Theme
    }

data Theme = Theme
    { themeHighlightStyle :: Text
    }

data RawConfig = RawConfig
    { rawName :: Maybe Text
    , rawAuthor :: Maybe Text
    , rawDescription :: Maybe Text
    , rawLang :: Maybe Text
    , rawBaseUrl :: Maybe Text
    , rawTheme :: Maybe RawTheme
    }

data RawTheme = RawTheme
    { rawHighlightStyle :: Maybe Text
    }

instance FromJSON RawConfig where
    parseJSON = withObject "config" $ \object ->
        RawConfig
            <$> object .:? "siteName"
            <*> object .:? "siteAuthor"
            <*> object .:? "siteDescription"
            <*> object .:? "siteLang"
            <*> object .:? "baseUrl"
            <*> object .:? "theme"

instance FromJSON RawTheme where
    parseJSON = withObject "theme" $ \object ->
        RawTheme
            <$> object .:? "highlightStyle"

loadConfig :: FilePath -> IO SiteConfig
loadConfig path = do
    econtent <- (Right <$> TIO.readFile path) `catch` \(e :: IOException) -> pure (Left e)
    case econtent of
        Left e
            | isDoesNotExistError e -> do
                warn "config.yaml not found; using default configuration:"
                warn ("  siteName        = " <> siteName defaults)
                warn ("  siteAuthor      = " <> siteAuthor defaults)
                warn ("  siteDescription = " <> siteDescription defaults)
                warn ("  siteLang        = " <> siteLang defaults)
                warn ("  theme           = " <> themeHighlightStyle (siteTheme defaults))
                pure defaults
            | otherwise -> die ("cannot read config.yaml: " <> T.pack (show e))
        Right content ->
            case decodeEither' (encodeUtf8 content) of
                Left err -> die ("failed to parse YAML: " <> T.pack (prettyPrintParseException err))
                Right raw -> apply raw
  where
    apply raw = do
        name <- field "siteName" (rawName raw) (siteName defaults)
        author <- field "siteAuthor" (rawAuthor raw) (siteAuthor defaults)
        description <- field "siteDescription" (rawDescription raw) (siteDescription defaults)
        lang <- field "siteLang" (rawLang raw) (siteLang defaults)
        baseUrl <- resolveBaseUrl (rawBaseUrl raw)
        theme <- resolveTheme (rawTheme raw)
        pure SiteConfig{siteName = name, siteAuthor = author, siteDescription = description, siteLang = lang, siteBaseUrl = baseUrl, siteTheme = theme}

field :: Text -> Maybe Text -> Text -> IO Text
field key Nothing fallback = do
    warn (key <> " is not set in config.yaml; using default: " <> fallback)
    pure fallback
field _ (Just value) _ = pure value

resolveBaseUrl :: Maybe Text -> IO (Maybe Text)
resolveBaseUrl Nothing = do
    warn "baseUrl is not set in config.yaml; feed.xml will not be generated"
    pure Nothing
resolveBaseUrl (Just "") = do
    warn "baseUrl is empty in config.yaml; feed.xml will not be generated"
    pure Nothing
resolveBaseUrl (Just raw) =
    case T.dropWhileEnd (== '/') raw of
        clean
            | "http://" `T.isPrefixOf` clean || "https://" `T.isPrefixOf` clean -> pure (Just clean)
            | otherwise -> die ("invalid baseUrl '" <> raw <> "': must start with http:// or https://")

resolveTheme :: Maybe RawTheme -> IO Theme
resolveTheme Nothing = do
    warn "theme is not set in config.yaml; using default:"
    warn ("  highlightStyle = " <> defaultHighlightStyle)
    pure defaultTheme
resolveTheme (Just raw) = do
    style <- case rawHighlightStyle raw of
        Nothing -> do
            warn "theme.highlightStyle is not set in config.yaml; using default: tango"
            pure defaultHighlightStyle
        Just s -> pure s
    case lookup style highlightingStyles of
        Just _ -> pure Theme{themeHighlightStyle = style}
        Nothing -> die (availableStyles style)

availableStyles :: Text -> Text
availableStyles style =
    "unknown highlight style '" <> style <> "'. Available styles:\n  " <> T.intercalate ", " (map fst highlightingStyles)

defaults :: SiteConfig
defaults =
    SiteConfig
        { siteName = "burogu"
        , siteAuthor = ""
        , siteDescription = ""
        , siteLang = "zh-CN"
        , siteBaseUrl = Nothing
        , siteTheme = defaultTheme
        }

defaultTheme :: Theme
defaultTheme = Theme{themeHighlightStyle = defaultHighlightStyle}

defaultHighlightStyle :: Text
defaultHighlightStyle = "tango"

warn :: Text -> IO ()
warn msg = TIO.hPutStrLn stderr ("warning: " <> msg)

die :: Text -> IO a
die msg = do
    TIO.hPutStrLn stderr ("config.yaml: " <> msg)
    exitFailure
