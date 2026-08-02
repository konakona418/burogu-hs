module Css (FontFile (..), Fonts (..), Preset (..), TokenColor (..), ariaPreset, emptyFonts, renderCss, tokenColors) where

import Clay qualified as C
import Clay.Flexbox qualified as CF
import Clay.Media qualified as CM
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), Value (..), withObject, (.!=), (.:), (.:?))
import Data.Maybe (fromMaybe)
import Data.Scientific (FPFormat (Generic), floatingOrInteger, formatScientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL

data TokenColor = TokenColor
    { tcClass :: Text
    , tcLight :: Text
    , tcDark :: Text
    }

tokenColors :: [TokenColor]
tokenColors =
    [ TokenColor "al" "#ef2929" "#95da4c"
    , TokenColor "an" "#8f5902" "#3f8058"
    , TokenColor "at" "#204a87" "#2980b9"
    , TokenColor "bn" "#0000cf" "#f67400"
    , TokenColor "bu" "#204a87" "#7f8c8d"
    , TokenColor "cf" "#204a87" "#fdbc4b"
    , TokenColor "ch" "#4e9a06" "#3daee9"
    , TokenColor "cn" "#8f5902" "#27aeae"
    , TokenColor "co" "#8f5902" "#7a7c7d"
    , TokenColor "cv" "#8f5902" "#7f8c8d"
    , TokenColor "do" "#8f5902" "#a43340"
    , TokenColor "dt" "#204a87" "#2980b9"
    , TokenColor "dv" "#0000cf" "#f67400"
    , TokenColor "er" "#a40000" "#da4453"
    , TokenColor "ex" "#a40000" "#0099ff"
    , TokenColor "fl" "#0000cf" "#f67400"
    , TokenColor "fu" "#204a87" "#8e44ad"
    , TokenColor "im" "#204a87" "#27ae60"
    , TokenColor "in" "#8f5902" "#c45b00"
    , TokenColor "kw" "#204a87" "#cfcfc2"
    , TokenColor "op" "#ce5c00" "#cfcfc2"
    , TokenColor "ot" "#8f5902" "#27ae60"
    , TokenColor "pp" "#8f5902" "#27ae60"
    , TokenColor "re" "#4e9a06" "#2980b9"
    , TokenColor "sc" "#ce5c00" "#3daee9"
    , TokenColor "ss" "#4e9a06" "#da4453"
    , TokenColor "st" "#4e9a06" "#f44f4f"
    , TokenColor "va" "#000000" "#27aeae"
    , TokenColor "vs" "#4e9a06" "#da4453"
    , TokenColor "wa" "#8f5902" "#da4453"
    ]

{- | User font overrides for the theme preset. Every field is optional;
absent fields fall back to the preset's defaults.
-}
data Fonts = Fonts
    { fontsBody :: Maybe [Text]
    , fontsDisplay :: Maybe [Text]
    , fontsCode :: Maybe [Text]
    , fontsSize :: Maybe Text
    , fontsLineHeight :: Maybe Text
    , fontsFiles :: Maybe [FontFile]
    }

data FontFile = FontFile
    { ffSrc :: Text
    , ffFamily :: Text
    , ffWeight :: Text
    , ffStyle :: Text
    }

emptyFonts :: Fonts
emptyFonts = Fonts{fontsBody = Nothing, fontsDisplay = Nothing, fontsCode = Nothing, fontsSize = Nothing, fontsLineHeight = Nothing, fontsFiles = Nothing}

instance FromJSON Fonts where
    parseJSON = withObject "fonts" $ \object ->
        Fonts
            <$> object .:? "body"
            <*> object .:? "display"
            <*> object .:? "code"
            <*> object .:? "size"
            <*> object .:? "lineHeight"
            <*> object .:? "files"

instance FromJSON FontFile where
    parseJSON = withObject "fontFile" $ \object -> do
        src <- object .: "src"
        family <- object .: "family"
        weight <- object .:? "weight" .!= String "400"
        style <- object .:? "style" .!= String "normal"
        pure (FontFile src family (valueToText weight) (valueToText style))
      where
        valueToText :: Value -> Text
        valueToText (String s) = s
        valueToText (Number n) = case (floatingOrInteger n :: Either Double Integer) of
            Right i -> T.pack (show i)
            Left _ -> T.pack (formatScientific Generic Nothing n)
        valueToText (Bool b) = T.pack (show b)
        valueToText v = T.pack (show v)

{- | A theme preset: the full token sets (light and dark, including the
syntax-highlight token colors) plus its own rules. Font-family tokens are
declared by the preset, but user fonts (config `theme.fonts`) override
them via later emission in the same :root block.
-}
data Preset = Preset
    { presetName :: Text
    , presetTokens :: [(Text, Text)]
    , presetDarkTokens :: [(Text, Text)]
    , presetRules :: C.Css
    , presetDisplayFont :: Maybe [Text]
    , presetCodeFont :: Maybe [Text]
    }

ariaPreset :: Preset
ariaPreset =
    Preset
        { presetName = "aria"
        , presetTokens = baseTokens <> [("token-" <> tcClass tc, tcLight tc) | tc <- tokenColors]
        , presetDarkTokens = darkTokenValues <> [("token-" <> tcClass tc, tcDark tc) | tc <- tokenColors]
        , presetRules = tagGradient
        , presetDisplayFont = Nothing
        , presetCodeFont = Nothing
        }

renderCss :: Preset -> Fonts -> [Text] -> Text
renderCss preset fonts extraCss =
    fontFaceCss (fromMaybe [] (fontsFiles fonts))
        <> TL.toStrict (C.render (stylesheet preset fontTokens))
        <> mconcat extraCss
  where
    fontTokens = fontOverrideTokens preset fonts

stylesheet :: Preset -> [(Text, Text)] -> C.Css
stylesheet preset fontTokens =
    mconcat
        [ rootTokens (presetTokens preset <> fontTokens)
        , darkTokens (presetTokens preset) (presetDarkTokens preset)
        , baseRules
        , overflowRules
        , listSpacing
        , mobileRules
        , tokenRules
        , presetRules preset
        , printRules
        ]

{- | Tokens emitted after the preset tokens for the user font overrides;
later keys win in the same :root block. Display/code fall back to the
preset's defaults when the user did not override them.
-}
fontOverrideTokens :: Preset -> Fonts -> [(Text, Text)]
fontOverrideTokens preset fonts =
    maybe [] (\stack -> [("font-family", fontStack stack)]) (fontsBody fonts)
        <> maybe [] (\v -> [("font-size", v)]) (fontsSize fonts)
        <> maybe [] (\v -> [("line-height", v)]) (fontsLineHeight fonts)
        <> maybe [] (\stack -> [("font-display", fontStack stack)]) (fontsDisplay fonts <|> presetDisplayFont preset)
        <> maybe [] (\stack -> [("font-code", fontStack stack)]) (fontsCode fonts <|> presetCodeFont preset)

{- | Render a font stack: names with spaces are quoted, generic keywords
(serif, sans-serif, ...) are not.
-}
fontStack :: [Text] -> Text
fontStack = T.intercalate ", " . map renderName
  where
    renderName n
        | n `elem` genericKeywords = n
        | T.any (== ' ') n = "\"" <> n <> "\""
        | otherwise = n

genericKeywords :: [Text]
genericKeywords = ["serif", "sans-serif", "monospace", "cursive", "fantasy", "system-ui", "ui-serif", "ui-sans-serif", "ui-monospace"]

fontFaceCss :: [FontFile] -> Text
fontFaceCss = mconcat . map one
  where
    one ff =
        T.unlines
            [ "@font-face"
            , "{"
            , "  font-family : " <> quote (ffFamily ff) <> ";"
            , "  font-weight : " <> ffWeight ff <> ";"
            , "  font-style  : " <> ffStyle ff <> ";"
            , "  src         : url(/fonts/" <> basename (ffSrc ff) <> ");"
            , "}"
            ]
    quote name = "\"" <> name <> "\""
    basename :: Text -> Text
    basename = T.reverse . T.takeWhile (/= '/') . T.reverse

overflowRules :: C.Css
overflowRules = do
    C.img C.? do
        "max-width" C.-: "100%"
        "height" C.-: "auto"
    C.table C.? do
        C.display C.block
        "overflow-x" C.-: "auto"
        "max-width" C.-: "100%"
    C.figure C.? ("max-width" C.-: "100%")

mobileRules :: C.Css
mobileRules = C.query CM.screen [CM.maxWidth (C.px 600)] $ do
    (":root" :: C.Selector) C.? do
        "--font-size" C.-: "16px"
        "--line-height" C.-: "24px"
        "--space-page-top" C.-: "16px"
        "--space-page-side" C.-: "12px"
    C.pre C.? ("font-size" C.-: "14px")
    C.nav C.? C.a C.? ("padding" C.-: "8px 4px")
    (".site-name" :: C.Selector) C.? ("flex-basis" C.-: "100%")

listSpacing :: C.Css
listSpacing = do
    (".post-item" :: C.Selector) C.? do
        C.display C.flex
        C.flexWrap CF.wrap
        "gap" C.-: "0 var(--space-list-gap)"
        "align-items" C.-: "baseline"
    (".post-item .post-desc" :: C.Selector) C.? do
        "flex-basis" C.-: "100%"
        "margin" C.-: "6px 0 0"
    (".post-meta" :: C.Selector) C.? do
        C.display C.flex
        "gap" C.-: "0 var(--space-list-gap)"
        "align-items" C.-: "baseline"
    (".tag-item" :: C.Selector) C.? do
        C.display C.flex
        "gap" C.-: "0 6px"
        "align-items" C.-: "baseline"

rootTokens :: [(Text, Text)] -> C.Css
rootTokens tokens = (":root" :: C.Selector) C.? mapM_ emit tokens

{- | Dark tokens are emitted three times: under the system
prefers-color-scheme query, and under explicit html[data-theme] values
(the theme.js toggle). The attribute selectors outrank :root, so an
explicit light/dark choice wins over the system preference.
-}
darkTokens :: [(Text, Text)] -> [(Text, Text)] -> C.Css
darkTokens light dark =
    mconcat
        [ C.query CM.screen [CM.prefersColorScheme CM.dark] $ (":root" :: C.Selector) C.? mapM_ emit dark
        , C.element "html[data-theme=\"dark\"]" C.? mapM_ emit dark
        , C.element "html[data-theme=\"light\"]" C.? mapM_ emit light
        ]

emit :: (Text, Text) -> C.Css
emit (key, value) = C.Key (C.Plain ("--" <> key)) C.-: value

baseTokens :: [(Text, Text)]
baseTokens =
    [ ("color-bg", "#ffffff")
    , ("color-text", "#333333")
    , ("color-muted", "#666666")
    , ("color-link", "#0066cc")
    , ("color-link-hover", "#0055aa")
    , ("color-code-bg", "#f5f5f5")
    , ("color-mark", "#ffe58f")
    , ("font-family", "\"Helvetica Neue\", \"PingFang SC\", \"Hiragino Sans GB\", \"Microsoft YaHei\", sans-serif")
    , ("font-size", "17px")
    , ("line-height", "28px")
    , ("content-width", "800px")
    , ("space-page-top", "24px")
    , ("space-page-side", "16px")
    , ("space-list-gap", "8px")
    , ("space-nav-gap", "24px")
    , ("space-nav-link", "12px")
    ]

darkTokenValues :: [(Text, Text)]
darkTokenValues =
    [ ("color-bg", "#1a1a1a")
    , ("color-text", "#e6e6e6")
    , ("color-muted", "#999999")
    , ("color-link", "#6ab0f3")
    , ("color-link-hover", "#8cc2f5")
    , ("color-code-bg", "#2d2d2d")
    , ("color-mark", "#8a7000")
    ]

baseRules :: C.Css
baseRules = do
    C.html C.? do
        "scrollbar-gutter" C.-: "stable"
        "-webkit-text-size-adjust" C.-: "100%"
        "-webkit-tap-highlight-color" C.-: "transparent"
        "background-color" C.-: "var(--color-bg)"
    C.body C.? do
        "max-width" C.-: "var(--content-width)"
        C.marginLeft C.auto
        C.marginRight C.auto
        "padding-top" C.-: "var(--space-page-top)"
        "padding-left" C.-: "calc(var(--space-page-side) + env(safe-area-inset-left, 0px))"
        "padding-right" C.-: "calc(var(--space-page-side) + env(safe-area-inset-right, 0px))"
        "font-family" C.-: "var(--font-family)"
        "font-size" C.-: "var(--font-size)"
        "line-height" C.-: "var(--line-height)"
        "color" C.-: "var(--color-text)"
        "background-color" C.-: "var(--color-bg)"
    C.a C.? do
        "color" C.-: "var(--color-link)"
        C.textDecoration C.underline
    C.a C.# C.hover C.? ("color" C.-: "var(--color-link-hover)")
    C.h1 C.? do
        "font-size" C.-: "calc(var(--font-size) * 1.65)"
        "line-height" C.-: "calc(var(--line-height) * 1.45)"
    C.time C.? ("color" C.-: "var(--color-muted)")
    (".tag-count" :: C.Selector) C.? ("color" C.-: "var(--color-muted)")
    (".site-footer" :: C.Selector) C.? do
        "color" C.-: "var(--color-muted)"
        C.display C.flex
        "justify-content" C.-: "space-between"
        "align-items" C.-: "baseline"
    (".theme-toggle" :: C.Selector) C.? do
        "border" C.-: "none"
        "background" C.-: "none"
        "color" C.-: "inherit"
        "font-size" C.-: "1em"
        "padding" C.-: "0"
        "cursor" C.-: "pointer"
        "font-family" C.-: "var(--font-family)"
        "line-height" C.-: "var(--line-height)"
    C.code C.? do
        "background-color" C.-: "var(--color-code-bg)"
        C.paddingLeft (C.px 4)
        C.paddingRight (C.px 4)
        C.borderRadius (C.px 3) (C.px 3) (C.px 3) (C.px 3)
    C.pre C.? do
        "background-color" C.-: "var(--color-code-bg)"
        C.padding (C.px 12) (C.px 16) (C.px 12) (C.px 16)
        C.borderRadius (C.px 4) (C.px 4) (C.px 4) (C.px 4)
        C.overflowX C.scroll
    C.pre C.? C.code C.? ("background-color" C.-: "transparent")
    C.ul C.? C.listStyleType C.none
    C.li C.? ("margin-bottom" C.-: "var(--space-list-gap)")
    C.nav C.? do
        C.display C.flex
        C.flexWrap CF.wrap
        "align-items" C.-: "baseline"
        "gap" C.-: "var(--space-list-gap) var(--space-nav-link)"
        "margin-bottom" C.-: "var(--space-nav-gap)"
    (".post-desc" :: C.Selector) C.? ("color" C.-: "var(--color-muted)")
    C.a C.# ("aria-hidden" C.@= "true") C.? C.display C.none
    C.mark C.? do
        "background-color" C.-: "var(--color-mark)"
        "color" C.-: "var(--color-text)"
    (".search-input" :: C.Selector) C.? do
        C.display C.block
        "width" C.-: "100%"
        "box-sizing" C.-: "border-box"
        "padding" C.-: "8px 12px"
        "font-size" C.-: "var(--font-size)"
        "background-color" C.-: "var(--color-code-bg)"
        "color" C.-: "var(--color-text)"
        "border" C.-: "1px solid var(--color-muted)"
        "border-radius" C.-: "4px"
        "margin-bottom" C.-: "var(--space-nav-gap)"
    (".search-input" :: C.Selector) C.# C.focus C.? do
        "outline" C.-: "none"
        "border-color" C.-: "var(--color-link)"
    C.element "input[type=search]::-webkit-search-cancel-button" C.? do
        "-webkit-appearance" C.-: "none"
        "appearance" C.-: "none"

printRules :: C.Css
printRules =
    C.query CM.print [] $ do
        C.html C.? ("background-color" C.-: "#ffffff")
        C.body C.? do
            "background-color" C.-: "#ffffff"
            "color" C.-: "#000000"
        C.a C.? ("color" C.-: "#000000")
        C.nav C.? C.display C.none
        C.mark C.? ("background-color" C.-: "transparent")

tokenRules :: C.Css
tokenRules = mapM_ tokenRule tokenColors
  where
    tokenRule :: TokenColor -> C.Css
    tokenRule tc = C.element ("code span." <> tcClass tc) C.? ("color" C.-: ("var(--token-" <> tcClass tc <> ")"))

tagGradient :: C.Css
tagGradient = do
    (".tag-item" :: C.Selector) C.? ("--tag-hue" C.-: "calc(190deg + var(--tag-count) * 18deg)")
    (".tag-name" :: C.Selector) C.? ("color" C.-: "hsl(var(--tag-hue), 70%, 45%)")
