module Shaft (allPresets, presetByName, presetNames, shaftPreset) where

import Clay qualified as C
import Css (Preset (..), TokenColor (..), ariaPreset, tokenColors)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

allPresets :: [Preset]
allPresets = [ariaPreset, shaftPreset]

presetNames :: [Text]
presetNames = map presetName allPresets

presetByName :: Text -> Maybe Preset
presetByName name = listToMaybe [preset | preset <- allPresets, presetName preset == name]

{- | The shaft preset: an editorial print look. Warm paper-and-ink
colors, a single high-saturation red accent, serif display type, and a
few geometric moves (slanted archive years, outlined tags, oversized 404)
built purely from static CSS. No transitions or animations.
-}
shaftPreset :: Preset
shaftPreset =
    Preset
        { presetName = "shaft"
        , presetTokens = shaftBaseTokens <> [("token-" <> tcClass tc, tcLight tc) | tc <- tokenColors]
        , presetDarkTokens = shaftDarkBaseTokens <> [("token-" <> tcClass tc, tcDark tc) | tc <- tokenColors]
        , presetRules = shaftRules
        , presetDisplayFont = Just shaftDisplayStack
        , presetCodeFont = Nothing
        }

shaftDisplayStack :: [Text]
shaftDisplayStack = ["Georgia", "Noto Serif CJK SC", "Source Han Serif SC", "Songti SC", "SimSun", "serif"]

shaftBaseTokens :: [(Text, Text)]
shaftBaseTokens =
    [ ("color-bg", "#faf9f7")
    , ("color-text", "#26221f")
    , ("color-muted", "#6f6a64")
    , ("color-link", "#c4000e")
    , ("color-link-hover", "#8a0008")
    , ("color-code-bg", "#f0ede8")
    , ("color-mark", "#ffd97a")
    , ("color-accent", "#c4000e")
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

shaftDarkBaseTokens :: [(Text, Text)]
shaftDarkBaseTokens =
    [ ("color-bg", "#14120f")
    , ("color-text", "#e8e4de")
    , ("color-muted", "#98918a")
    , ("color-link", "#ff5347")
    , ("color-link-hover", "#ff7a70")
    , ("color-code-bg", "#211e1a")
    , ("color-mark", "#9c7a00")
    , ("color-accent", "#ff5347")
    ]

shaftRules :: C.Css
shaftRules = do
    displayHeadings
    siteName
    postDate
    postTags
    tagIndex
    archiveYear
    notFound

displayHeadings :: C.Css
displayHeadings =
    C.h1 C.? do
        "font-family" C.-: "var(--font-display)"
        "font-size" C.-: "calc(var(--font-size) * 2.2)"
        "line-height" C.-: "calc(var(--line-height) * 1.3)"

siteName :: C.Css
siteName =
    (".site-name" :: C.Selector) C.? do
        "font-family" C.-: "var(--font-display)"
        "font-weight" C.-: "600"
        "letter-spacing" C.-: "0.02em"

postDate :: C.Css
postDate =
    (".post-date" :: C.Selector) C.? do
        "font-family" C.-: "var(--font-display)"
        "color" C.-: "var(--color-accent)"
        "font-size" C.-: "0.92em"
        "letter-spacing" C.-: "0.03em"
        "min-width" C.-: "6.2em"

postTags :: C.Css
postTags = do
    (".post-tag" :: C.Selector) C.? do
        C.display C.inlineBlock
        "font-size" C.-: "0.85em"
        "border" C.-: "1px solid var(--color-accent)"
        "padding" C.-: "1px 6px"
        "color" C.-: "var(--color-accent)"
        "margin-right" C.-: "6px"
        C.textDecoration C.none
    (".post-tag-sep" :: C.Selector) C.? C.display C.none
    (".post-tag:last-child" :: C.Selector) C.? ("margin-right" C.-: "0")

tagIndex :: C.Css
tagIndex = do
    (".tag-item .tag-name" :: C.Selector) C.? do
        "border" C.-: "1px solid var(--color-accent)"
        "padding" C.-: "2px 10px"
        "font-family" C.-: "var(--font-display)"
        "color" C.-: "var(--color-accent)"
        C.textDecoration C.none
    (".tag-item .tag-count" :: C.Selector) C.? do
        "color" C.-: "var(--color-accent)"
        "font-family" C.-: "var(--font-display)"
        "margin-left" C.-: "8px"

archiveYear :: C.Css
archiveYear = do
    (".archive-year" :: C.Selector) C.? do
        C.display C.inlineBlock
        "font-family" C.-: "var(--font-display)"
        "font-size" C.-: "calc(var(--font-size) * 2.4)"
        "line-height" C.-: "1.15"
        "background-color" C.-: "var(--color-accent)"
        "color" C.-: "var(--color-bg)"
        "padding" C.-: "4px 14px"
        "margin" C.-: "0 0 12px"
        "clip-path" C.-: "polygon(0 0, 100% 0, calc(100% - 12px) 100%, 0 100%)"
    (".archive-year + .post-list" :: C.Selector) C.? ("margin-top" C.-: "0")

notFound :: C.Css
notFound = do
    (".not-found" :: C.Selector) C.? ("padding-top" C.-: "2rem")
    (".not-found h1" :: C.Selector) C.? do
        "font-family" C.-: "var(--font-display)"
        "font-size" C.-: "calc(var(--font-size) * 4.5)"
        "line-height" C.-: "1"
        "color" C.-: "var(--color-accent)"
        "margin" C.-: "0"
    (".not-found h1" :: C.Selector) C.# C.after C.? do
        "content" C.-: "\"\""
        C.display C.block
        "width" C.-: "3rem"
        "height" C.-: "4px"
        "margin-top" C.-: "0.75rem"
        "background-color" C.-: "var(--color-accent)"
        "clip-path" C.-: "polygon(0 0, 100% 0, calc(100% - 8px) 100%, 0 100%)"
