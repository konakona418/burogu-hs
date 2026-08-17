module Config (DeployConfig (..), RawConfig (..), RawDeploy (..), RawTheme (..), SiteConfig (..), Theme (..), loadConfig) where

import Control.Exception (IOException, catch)
import Css (Fonts (..), emptyFonts)
import Data.Aeson (FromJSON (..), Value (..), withObject, (.:?))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Yaml (ParseException, decodeEither', prettyPrintParseException)
import Shaft (presetNames)
import System.Exit (exitFailure)
import System.IO (stderr)
import System.IO.Error (isDoesNotExistError)

data SiteConfig = SiteConfig
    { siteName :: Text
    , siteAuthor :: Text
    , siteDescription :: Text
    , siteLang :: Text
    , siteBaseUrl :: Maybe Text
    , siteCopyright :: Text
    , siteGeneratedBy :: Maybe Text
    , siteFooterSeparator :: Text
    , siteDeploy :: DeployConfig
    , siteSrcRepo :: Maybe Text
    , siteTheme :: Theme
    }

data DeployConfig = DeployConfig
    { deployTarget :: Maybe Text
    , deployRepo :: Maybe Text
    , deployBranch :: Maybe Text
    , deployCommitName :: Maybe Text
    , deployCommitEmail :: Maybe Text
    }

data Theme = Theme
    { themeMath :: Text
    , themeMathUrl :: Maybe Text
    , themeExtraCss :: [Text]
    , themeExtraJs :: [Text]
    , themePreset :: Text
    , themeFonts :: Fonts
    }

{- | User font overrides for the theme preset. Every field is optional;
absent fields fall back to the preset's defaults.
-}
data RawConfig = RawConfig
    { rawName :: Maybe Text
    , rawAuthor :: Maybe Text
    , rawDescription :: Maybe Text
    , rawLang :: Maybe Text
    , rawBaseUrl :: Maybe Text
    , rawTagsLabel :: Maybe Text
    , rawCopyright :: Maybe Text
    , rawGeneratedBy :: Maybe Text
    , rawFooterSeparator :: Maybe Text
    , rawDeploy :: Maybe RawDeploy
    , rawSrcRepo :: Maybe Text
    , rawTheme :: Maybe RawTheme
    }

data RawDeploy = RawDeploy
    { rawDeployTarget :: Maybe Text
    , rawDeployRepo :: Maybe Text
    , rawDeployBranch :: Maybe Text
    , rawCommitName :: Maybe Text
    , rawCommitEmail :: Maybe Text
    }

data RawTheme = RawTheme
    { rawMath :: Maybe Text
    , rawMathUrl :: Maybe Text
    , rawExtraCss :: Maybe [Text]
    , rawExtraJs :: Maybe [Text]
    , rawPreset :: Maybe Text
    , rawFonts :: Maybe Fonts
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
            <*> object .:? "siteGeneratedBy"
            <*> object .:? "footerSeparator"
            <*> object .:? "deploy"
            <*> object .:? "srcRepo"
            <*> object .:? "theme"

instance FromJSON RawDeploy where
    parseJSON = withObject "deploy" $ \object ->
        RawDeploy
            <$> object .:? "target"
            <*> object .:? "repo"
            <*> object .:? "branch"
            <*> object .:? "commitName"
            <*> object .:? "commitEmail"

instance FromJSON RawTheme where
    parseJSON = withObject "theme" $ \object ->
        RawTheme
            <$> object .:? "math"
            <*> object .:? "mathUrl"
            <*> object .:? "extraCss"
            <*> object .:? "extraJs"
            <*> object .:? "preset"
            <*> object .:? "fonts"

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
                warn ("    math          = " <> themeMath (siteTheme defaults))
                warn "    mathUrl       = (not set)"
                warn "    extraCss      = (none)"
                warn "    extraJs       = (none)"
                pure defaults
            | otherwise -> die ("cannot read config.yaml: " <> T.pack (show e))
        Right content ->
            case decodeEither' (encodeUtf8 content) of
                Left err -> die ("failed to parse YAML: " <> T.pack (prettyPrintParseException err))
                Right raw -> do
                    warnUnknownKeys content
                    apply raw
  where
    apply raw = do
        name <- field "siteName" (rawName raw) (siteName defaults)
        author <- field "siteAuthor" (rawAuthor raw) (siteAuthor defaults)
        description <- field "siteDescription" (rawDescription raw) (siteDescription defaults)
        lang <- field "siteLang" (rawLang raw) (siteLang defaults)
        baseUrl <- resolveBaseUrl (rawBaseUrl raw)
        rejectTagsLabel (rawTagsLabel raw)
        copyright <- field "siteCopyright" (rawCopyright raw) ("© " <> author)
        separator <- field "footerSeparator" (rawFooterSeparator raw) " · "
        theme <- resolveTheme (rawTheme raw)
        pure SiteConfig{siteName = name, siteAuthor = author, siteDescription = description, siteLang = lang, siteBaseUrl = baseUrl, siteCopyright = copyright, siteGeneratedBy = rawGeneratedBy raw, siteFooterSeparator = separator, siteDeploy = resolveDeploy (rawDeploy raw), siteSrcRepo = rawSrcRepo raw, siteTheme = theme}

rejectTagsLabel :: Maybe Text -> IO ()
rejectTagsLabel Nothing = pure ()
rejectTagsLabel (Just _) =
    die "tagsLabel is no longer supported; declare the tags page in _pages/ (e.g. _pages/tags.md with `title: Tags` and `redirectAs: /tags/`), then remove tagsLabel from config.yaml"

resolveDeploy :: Maybe RawDeploy -> DeployConfig
resolveDeploy Nothing = DeployConfig{deployTarget = Nothing, deployRepo = Nothing, deployBranch = Nothing, deployCommitName = Nothing, deployCommitEmail = Nothing}
resolveDeploy (Just raw) = DeployConfig{deployTarget = rawDeployTarget raw, deployRepo = rawDeployRepo raw, deployBranch = rawDeployBranch raw, deployCommitName = rawCommitName raw, deployCommitEmail = rawCommitEmail raw}

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
    warn ("  preset    = " <> defaultPreset)
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
    preset <- case rawPreset raw of
        Nothing -> do
            warn "theme.preset is not set in config.yaml; using default: aria"
            pure defaultPreset
        Just p
            | p `elem` presetNames -> pure p
            | otherwise -> die ("unknown theme preset '" <> p <> "'. Available presets: " <> T.intercalate ", " presetNames)
    let extraCss = fromMaybe [] (rawExtraCss raw)
        extraJs = fromMaybe [] (rawExtraJs raw)
        fonts = fromMaybe emptyFonts (rawFonts raw)
    pure Theme{themeMath = math, themeMathUrl = mathUrl, themeExtraCss = extraCss, themeExtraJs = extraJs, themePreset = preset, themeFonts = fonts}

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

{- | Warn about top-level config keys the generator does not know
(they are ignored). tagsLabel is known and rejected elsewhere.
-}
warnUnknownKeys :: Text -> IO ()
warnUnknownKeys content = case decodeEither' (encodeUtf8 content) :: Either ParseException Value of
    Right (Object o) ->
        mapM_
            (\k -> warn ("config.yaml: unknown key '" <> k <> "' ignored"))
            [k | k <- map K.toText (KM.keys o), k `notElem` knownKeys]
    _ -> pure ()

knownKeys :: [Text]
knownKeys =
    [ "siteName"
    , "siteAuthor"
    , "siteDescription"
    , "siteLang"
    , "baseUrl"
    , "tagsLabel"
    , "siteCopyright"
    , "siteGeneratedBy"
    , "deploy"
    , "srcRepo"
    , "theme"
    ]

defaults :: SiteConfig
defaults =
    SiteConfig
        { siteName = "burogu"
        , siteAuthor = ""
        , siteDescription = ""
        , siteLang = "zh-CN"
        , siteBaseUrl = Nothing
        , siteCopyright = "© "
        , siteGeneratedBy = Nothing
        , siteDeploy = DeployConfig{deployTarget = Nothing, deployRepo = Nothing, deployBranch = Nothing, deployCommitName = Nothing, deployCommitEmail = Nothing}
        , siteSrcRepo = Nothing
        , siteTheme = defaultTheme
        }

defaultTheme :: Theme
defaultTheme = Theme{themeMath = defaultMathMethod, themeMathUrl = Nothing, themeExtraCss = [], themeExtraJs = [], themePreset = defaultPreset, themeFonts = emptyFonts}

defaultMathMethod :: Text
defaultMathMethod = "mathjax"

defaultPreset :: Text
defaultPreset = "aria"

warn :: Text -> IO ()
warn msg = TIO.hPutStrLn stderr ("warning: " <> msg)

die :: Text -> IO a
die msg = do
    TIO.hPutStrLn stderr ("config.yaml: " <> msg)
    exitFailure
