module Format (formatOne, run) where

import Cli (Paths (..))
import Config (RawConfig (..), RawDeploy (..), RawTheme (..))
import Control.Exception (IOException, catch)
import Control.Monad (foldM, unless)
import Css (FontFile (..), Fonts (..), emptyFonts)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.List (sortOn)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Yaml (ParseException, decodeEither', encode)
import Frontmatter (Kind (..), normalizeFrontmatter, splitFrontmatter)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)
import Template (ConfigValues (..), defaultConfigTemplate, renderConfig)

run :: Paths -> Bool -> IO ()
run paths dryRun = do
    configErr <- formatConfig paths dryRun
    postErr <- formatDir paths dryRun "_post" PostKind
    pageErr <- formatDir paths dryRun "_pages" PageKind
    unless (not configErr && not postErr && not pageErr) exitFailure

formatConfig :: Paths -> Bool -> IO Bool
formatConfig paths dryRun = do
    let configPath = pConfig paths
    exists <- doesFileExist configPath
    if not exists
        then pure False
        else do
            econtent <- (Right <$> TIO.readFile configPath) `catch` \(e :: IOException) -> pure (Left e)
            case econtent of
                Left e -> do
                    reportError (T.pack configPath <> ": cannot read: " <> T.pack (show e))
                    pure True
                Right content -> case decodeEither' (encodeUtf8 content) :: Either Data.Yaml.ParseException RawConfig of
                    Left err -> do
                        reportError (T.pack configPath <> ": failed to parse YAML: " <> T.pack (show err))
                        pure True
                    Right raw -> do
                        let values = configValues raw
                            rendered = renderConfig defaultConfigTemplate values
                        warnUnknownConfigKeys (configPath <> ":") content
                        if rendered == content
                            then pure False
                            else do
                                if dryRun
                                    then TIO.putStrLn ("would rewrite " <> T.pack configPath <> ":")
                                    else TIO.putStrLn ("rewrote " <> T.pack configPath <> ":")
                                unless dryRun (TIO.writeFile configPath rendered)
                                TIO.putStr rendered
                                pure False

formatDir :: Paths -> Bool -> FilePath -> Kind -> IO Bool
formatDir paths dryRun dirName kind = do
    let dir = pSrc paths </> dirName
    isDir <- doesDirectoryExist dir
    if not isDir
        then pure False
        else do
            names <- sortOn id . filter (T.isSuffixOf ".md" . T.pack) <$> listDirectory dir
            foldM (\acc name -> (|| acc) <$> formatOne dryRun dir kind name) False names

formatOne :: Bool -> FilePath -> Kind -> String -> IO Bool
formatOne dryRun dir kind filename = do
    let path = dir </> filename
    content <- TIO.readFile path
    let (mBlock, body) = splitFrontmatter content
        block = fromMaybe "" mBlock
    case normalizeFrontmatter kind path block of
        Left err -> do
            reportError (T.pack path <> ": " <> err)
            pure True
        Right (normalized, unknownKeys) -> do
            mapM_ (\k -> reportWarning (path <> ":") k "kept as-is") unknownKeys
            let newContent = "---\n" <> normalized <> "---\n" <> body
            if newContent == content
                then pure False
                else do
                    if dryRun
                        then do
                            TIO.putStrLn ("would format " <> T.pack path <> ":")
                            TIO.putStr newContent
                        else TIO.putStrLn ("formatted " <> T.pack path <> ":")
                    unless dryRun (TIO.writeFile path newContent)
                    pure False

configValues :: RawConfig -> ConfigValues
configValues raw =
    ConfigValues
        { cvTop =
            [ ("siteName", rawName raw ?: "burogu")
            , ("baseUrl", rawBaseUrl raw ?: "https://example.com")
            , ("siteAuthor", rawAuthor raw ?: "Your Name")
            , ("siteDescription", rawDescription raw ?: "A blog generated with burogu")
            , ("siteLang", rawLang raw ?: "zh-CN")
            , ("siteCopyright", rawCopyright raw ?: "© Your Name")
            , ("siteGeneratedBy", rawGeneratedBy raw ?: "Generated with Burogu")
            ]
        , cvDeploy = case rawDeploy raw of
            Nothing -> Nothing
            Just d ->
                let vals =
                        [ ("target", rawDeployTarget d)
                        , ("repo", rawDeployRepo d)
                        , ("branch", rawDeployBranch d)
                        , ("commitName", rawCommitName d)
                        , ("commitEmail", rawCommitEmail d)
                        ]
                 in Just [(k, v) | (k, mv) <- vals, Just v <- [mv], v /= ""]
        , cvSrcRepo = rawSrcRepo raw
        , cvTheme =
            [ ("preset", rawPreset t ?: "aria")
            , ("math", rawMath t ?: "mathjax")
            , ("mathUrl", rawMathUrl t ?: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js")
            , ("extraCss", fmap (inlineList . map scalar) (rawExtraCss t) ?: "[theme.css]")
            ]
        , cvExtraJs = fmap (inlineList . map scalar) (rawExtraJs t)
        , cvFonts = fmap fontsValues (rawFonts t)
        , cvFontsFiles = fmap renderFontFiles (fontsFiles (fromMaybe emptyFonts (rawFonts t)))
        }
  where
    t = fromMaybe emptyRawTheme (rawTheme raw)
    (?:) :: Maybe Text -> Text -> Text
    m ?: d = fromMaybe d m

    scalar :: Text -> Text
    scalar = T.strip . decodeUtf8 . encode . String

    inlineList :: [Text] -> Text
    inlineList [] = "[]"
    inlineList items = "[" <> T.intercalate ", " items <> "]"

    fontsValues :: Fonts -> [(Text, Text)]
    fontsValues f =
        [ ("body", inlineList (map scalar xs)) | xs <- maybeToList (fontsBody f), not (null xs)
        ]
            <> [ ("display", inlineList (map scalar xs)) | xs <- maybeToList (fontsDisplay f), not (null xs)
               ]
            <> [ ("code", inlineList (map scalar xs)) | xs <- maybeToList (fontsCode f), not (null xs)
               ]
            <> [ ("size", v) | v <- maybeToList (fontsSize f), v /= ""
               ]
            <> [ ("lineHeight", v) | v <- maybeToList (fontsLineHeight f), v /= ""
               ]

    renderFontFiles :: [FontFile] -> Text
    renderFontFiles files =
        T.unlines
            ( "files:"
                : concatMap one files
            )
      where
        one :: FontFile -> [Text]
        one ff =
            [ "  - src: " <> scalar (ffSrc ff)
            , "    family: " <> scalar (ffFamily ff)
            , "    weight: " <> ffWeight ff
            , "    style: " <> ffStyle ff
            ]

emptyRawTheme :: RawTheme
emptyRawTheme = RawTheme{rawMath = Nothing, rawMathUrl = Nothing, rawExtraCss = Nothing, rawExtraJs = Nothing, rawPreset = Nothing, rawFonts = Nothing}

{- | Config keys the template knows about. Unknown top-level keys are
warned about and dropped (the template round-trip does not preserve
them).
-}
knownConfigKeys :: [Text]
knownConfigKeys =
    [ "siteName"
    , "baseUrl"
    , "siteAuthor"
    , "siteDescription"
    , "siteLang"
    , "siteCopyright"
    , "siteGeneratedBy"
    , "tagsLabel"
    , "deploy"
    , "srcRepo"
    , "theme"
    ]

warnUnknownConfigKeys :: String -> Text -> IO ()
warnUnknownConfigKeys prefix content =
    case decodeEither' (encodeUtf8 content) :: Either ParseException Value of
        Right (Object o) ->
            mapM_ (\k -> reportWarning prefix k "will be dropped") [k | k <- map K.toText (KM.keys o), k `notElem` knownConfigKeys]
        _ -> pure ()

reportError :: Text -> IO ()
reportError = TIO.hPutStrLn stderr . ("error: " <>)

reportWarning :: String -> Text -> String -> IO ()
reportWarning prefix key disposition =
    TIO.hPutStrLn stderr (T.pack prefix <> " warning: unknown key '" <> key <> "' " <> T.pack disposition)
