module Template (ConfigTemplate (..), ConfigValues (..), TemplateLine (..), defaultConfigTemplate, emptyConfigValues, renderConfig) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

{- | One configurable line in the config template: a key, the value
shown when nothing is configured (the documented default), and an
optional trailing comment.
-}
data TemplateLine = TemplateLine
    { tlKey :: Text
    , tlDefault :: Text
    , tlComment :: Maybe Text
    }

{- | The config.yaml template. Optional sections (deploy, srcRepo,
extraJs, fonts) carry a commented example for the "absent" case; when
the user actually configured the section, format renders it with real
values.
-}
data ConfigTemplate = ConfigTemplate
    { ctTop :: [TemplateLine]
    , ctDeployComment :: Text
    , ctDeploy :: [TemplateLine]
    , ctSrcRepoComment :: Text
    , ctSrcRepo :: TemplateLine
    , ctTheme :: [TemplateLine]
    , ctExtraJsComment :: Text
    , ctExtraJs :: TemplateLine
    , ctFontsComment :: Text
    , ctFonts :: [TemplateLine]
    }

{- | The values `renderConfig` substitutes into the template. Absent
top/theme entries fall back to the template default; a `Nothing`
optional section keeps the commented example, a `Just` renders the
real block (only the keys that are set).
-}
data ConfigValues = ConfigValues
    { cvTop :: [(Text, Text)]
    , cvDeploy :: Maybe [(Text, Text)]
    , cvSrcRepo :: Maybe Text
    , cvTheme :: [(Text, Text)]
    , cvExtraJs :: Maybe Text
    , cvFonts :: Maybe [(Text, Text)]
    , cvFontsFiles :: Maybe Text
    }

emptyConfigValues :: ConfigValues
emptyConfigValues = ConfigValues{cvTop = [], cvDeploy = Nothing, cvSrcRepo = Nothing, cvTheme = [], cvExtraJs = Nothing, cvFonts = Nothing, cvFontsFiles = Nothing}

defaultConfigTemplate :: ConfigTemplate
defaultConfigTemplate =
    ConfigTemplate
        { ctTop =
            [ TemplateLine "siteName" "burogu" (Just "# site title (header, HTML title, og:site_name)")
            , TemplateLine "baseUrl" "https://example.com" (Just "# site URL (http(s)://); enables the feed and og:url")
            , TemplateLine "siteAuthor" "Your Name" (Just "# author name (default copyright holder)")
            , TemplateLine "siteDescription" "A blog generated with burogu" (Just "# site description (HTML meta tag)")
            , TemplateLine "siteLang" "zh-CN" (Just "# page language code, e.g. en or zh-CN")
            , TemplateLine "siteCopyright" "© Your Name" (Just "# footer copyright")
            , TemplateLine "siteGeneratedBy" "Generated with Burogu" (Just "# footer credit; set to null to hide")
            , TemplateLine "footerSeparator" " · " (Just "# separator between footer links; set to empty to disable")
            ]
        , ctDeployComment =
            T.unlines
                [ "# deploy:"
                , "#   target: user@host:/var/www/lizi.moe      # rsync deployment (VPS); either target or repo"
                , "#   repo: git@github.com:user/site.git       # git deployment (GitHub Pages etc.)"
                , "#   branch: gh-pages                         # git deployment only; required when repo is set"
                , "#   commitName: Your Name                    # git deployment only; required (commit identity)"
                , "#   commitEmail: you@example.com             # git deployment only; required (commit identity)"
                ]
        , ctDeploy =
            [ TemplateLine "target" "" (Just "# rsync deployment (VPS); either target or repo")
            , TemplateLine "repo" "" (Just "# git deployment (GitHub Pages etc.)")
            , TemplateLine "branch" "" (Just "# git deployment only; required when repo is set")
            , TemplateLine "commitName" "" (Just "# git deployment only; required (commit identity)")
            , TemplateLine "commitEmail" "" (Just "# git deployment only; required (commit identity)")
            ]
        , ctSrcRepoComment = T.unlines ["# srcRepo: git@github.com:user/burogu-src.git # optional git repo for `sync`"]
        , ctSrcRepo = TemplateLine "srcRepo" "" (Just "# optional git repo for `sync`")
        , ctTheme =
            [ TemplateLine "preset" "aria" (Just "# aria | shaft (built-in theme presets)")
            , TemplateLine "math" "mathjax" (Just "# none | mathjax | katex")
            , TemplateLine "mathUrl" "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js" (Just "# optional: override the CDN URL")
            , TemplateLine "extraCss" "[theme.css]" (Just "")
            ]
        , ctExtraJsComment = T.unlines ["  # extraJs: [theme.js]  # optional: JS files under src/ loaded on every page"]
        , ctExtraJs = TemplateLine "extraJs" "" (Just "# optional: JS files under src/ loaded on every page")
        , ctFontsComment =
            T.unlines
                [ "  # fonts:               # optional: override the preset's font styles"
                , "  #   body: [\"Helvetica Neue\", \"PingFang SC\", sans-serif]"
                , "  #   display: [Georgia, \"Noto Serif CJK SC\", \"Songti SC\", SimSun, serif]"
                , "  #   code: [ui-monospace, Menlo, monospace]"
                , "  #   ja: [\"Hiragino Sans\", \"Noto Sans JP\", sans-serif]"
                , "  #   hant: [\"PingFang TC\", \"Noto Sans TC\", sans-serif]"
                , "  #   size: 17px"
                , "  #   lineHeight: 28px"
                , "  #   files:             # optional: embed font files (copied to site/fonts/, @font-face generated)"
                , "  #     - src: fonts/my-serif.woff2"
                , "  #       family: My Serif"
                , "  #       weight: 400"
                , "  #       style: normal"
                ]
        , ctFonts =
            [ TemplateLine "body" "" (Just "")
            , TemplateLine "display" "" (Just "")
            , TemplateLine "code" "" (Just "")
            , TemplateLine "size" "" (Just "")
            , TemplateLine "lineHeight" "" (Just "")
            , TemplateLine "files" "" (Just "")
            ]
        }

renderConfig :: ConfigTemplate -> ConfigValues -> Text
renderConfig tpl values = T.unlines (concat blocks)
  where
    blocks =
        [ map (renderLine "" (cvTop values)) (ctTop tpl)
        , deployBlock
        , srcRepoBlock
        , "theme:" : map (renderLine "  " (cvTheme values)) (ctTheme tpl)
        , extraJsBlock
        , fontsBlock
        ]

    renderLine :: Text -> [(Text, Text)] -> TemplateLine -> Text
    renderLine indent overrides entry =
        indent <> tlKey entry <> ": " <> fromMaybe (tlDefault entry) (lookup (tlKey entry) overrides) <> commentSuffix (tlComment entry)

    commentSuffix :: Maybe Text -> Text
    commentSuffix Nothing = ""
    commentSuffix (Just "") = ""
    commentSuffix (Just c) = "  " <> c

    deployBlock :: [Text]
    deployBlock = case cvDeploy values of
        Nothing -> T.lines (ctDeployComment tpl)
        Just vals -> "deploy:" : map (renderLine "  " vals) (setEntries (ctDeploy tpl) vals)

    srcRepoBlock :: [Text]
    srcRepoBlock = case cvSrcRepo values of
        Nothing -> T.lines (ctSrcRepoComment tpl)
        Just v -> [tlKey (ctSrcRepo tpl) <> ": " <> v <> commentSuffix (tlComment (ctSrcRepo tpl))]

    extraJsBlock :: [Text]
    extraJsBlock = case cvExtraJs values of
        Nothing -> T.lines (ctExtraJsComment tpl)
        Just v -> [renderLine "  " [(tlKey (ctExtraJs tpl), v)] (ctExtraJs tpl)]

    fontsBlock :: [Text]
    fontsBlock = case cvFonts values of
        Nothing -> T.lines (ctFontsComment tpl)
        Just vals ->
            "  fonts:"
                : map (renderLine "    " vals) (setEntries (ctFonts tpl) vals)
                    <> maybe [] (map ("    " <>) . T.lines) (cvFontsFiles values)

    setEntries :: [TemplateLine] -> [(Text, Text)] -> [TemplateLine]
    setEntries entries vals = [e | e <- entries, tlKey e `elem` map fst vals]
