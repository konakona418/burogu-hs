module Config (SiteConfig (..), Theme (..), loadConfig) where

import Control.Exception (IOException, catch)
import Data.Aeson (FromJSON (..), withObject, (.:?))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Yaml (decodeEither', prettyPrintParseException)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.IO.Error (isDoesNotExistError)

data SiteConfig = SiteConfig
    { siteName :: Text
    , siteAuthor :: Text
    , siteDescription :: Text
    , siteLang :: Text
    , siteBaseUrl :: Maybe Text
    , siteTagsLabel :: Text
    , siteCopyright :: Text
    , siteTheme :: Theme
    }

data Theme = Theme
    { themeMath :: Text
    , themeMathUrl :: Maybe Text
    , themeExtraCss :: [Text]
    , themeExtraJs :: [Text]
    }

data RawConfig = RawConfig
    { rawName :: Maybe Text
    , rawAuthor :: Maybe Text
    , rawDescription :: Maybe Text
    , rawLang :: Maybe Text
    , rawBaseUrl :: Maybe Text
    , rawTagsLabel :: Maybe Text
    , rawCopyright :: Maybe Text
    , rawTheme :: Maybe RawTheme
    }

data RawTheme = RawTheme
    { rawMath :: Maybe Text
    , rawMathUrl :: Maybe Text
    , rawExtraCss :: Maybe [Text]
    , rawExtraJs :: Maybe [Text]
    }

instance FromJSON RawConfig where
    parseJSON = withObject "config" $ \object ->
        RawConfig
            <$> object .:? "siteName"
            <*> object .:? "siteAuthor"
            <*> object .:? "siteDescription"
            <*> object .:? "siteLang"
            <*> object .:? "baseUrl"
            <*> object .:? "tagsLabel"
            <*> object .:? "siteCopyright"
            <*> object .:? "theme"

instance FromJSON RawTheme where
    parseJSON = withObject "theme" $ \object ->
        RawTheme
            <$> object .:? "math"
            <*> object .:? "mathUrl"
            <*> object .:? "extraCss"
            <*> object .:? "extraJs"

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
                warn "  baseUrl         = (not set)"
                warn ("  tagsLabel       = " <> siteTagsLabel defaults)
                warn ("    math          = " <> themeMath (siteTheme defaults))
                warn "    mathUrl       = (not set)"
                warn "    extraCss      = (none)"
                warn "    extraJs       = (none)"
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
        tagsLabel <- field "tagsLabel" (rawTagsLabel raw) (siteTagsLabel defaults)
        copyright <- field "siteCopyright" (rawCopyright raw) ("© " <> author)
        theme <- resolveTheme (rawTheme raw)
        pure SiteConfig{siteName = name, siteAuthor = author, siteDescription = description, siteLang = lang, siteBaseUrl = baseUrl, siteTagsLabel = tagsLabel, siteCopyright = copyright, siteTheme = theme}

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
    warn ("  math      = " <> defaultMathMethod)
    warn "  mathUrl   = (not set)"
    warn "  extraCss  = (none)"
    warn "  extraJs   = (none)"
    pure defaultTheme
resolveTheme (Just raw) = do
    math <- case rawMath raw of
        Nothing -> do
            warn "theme.math is not set in config.yaml; using default: mathjax"
            pure defaultMathMethod
        Just m -> pure m
    if math `elem` validMathMethods
        then pure ()
        else die ("unknown math method '" <> math <> "'. Available methods: " <> T.intercalate ", " validMathMethods)
    mathUrl <- resolveMathUrl math (rawMathUrl raw)
    let extraCss = fromMaybe [] (rawExtraCss raw)
        extraJs = fromMaybe [] (rawExtraJs raw)
    pure Theme{themeMath = math, themeMathUrl = mathUrl, themeExtraCss = extraCss, themeExtraJs = extraJs}

resolveMathUrl :: Text -> Maybe Text -> IO (Maybe Text)
resolveMathUrl "none" (Just _) = do
    warn "theme.mathUrl is set but theme.math is none; ignoring it"
    pure Nothing
resolveMathUrl _ Nothing = do
    warn "theme.mathUrl is not set in config.yaml; using the default CDN URL"
    pure Nothing
resolveMathUrl _ (Just "") = do
    warn "theme.mathUrl is empty in config.yaml; using the default CDN URL"
    pure Nothing
resolveMathUrl _ (Just raw)
    | "http://" `T.isPrefixOf` raw || "https://" `T.isPrefixOf` raw = pure (Just raw)
    | otherwise = die ("invalid mathUrl '" <> raw <> "': must start with http:// or https://")

validMathMethods :: [Text]
validMathMethods = ["none", "mathjax", "katex"]

defaults :: SiteConfig
defaults =
    SiteConfig
        { siteName = "burogu"
        , siteAuthor = ""
        , siteDescription = ""
        , siteLang = "zh-CN"
        , siteBaseUrl = Nothing
        , siteTagsLabel = "Tags"
        , siteCopyright = "© "
        , siteTheme = defaultTheme
        }

defaultTheme :: Theme
defaultTheme = Theme{themeMath = defaultMathMethod, themeMathUrl = Nothing, themeExtraCss = [], themeExtraJs = []}

defaultMathMethod :: Text
defaultMathMethod = "mathjax"

warn :: Text -> IO ()
warn msg = TIO.hPutStrLn stderr ("warning: " <> msg)

die :: Text -> IO a
die msg = do
    TIO.hPutStrLn stderr ("config.yaml: " <> msg)
    exitFailure
