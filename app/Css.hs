module Css (renderCss) where

import Clay qualified as C
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Text.Pandoc.Highlighting (defaultStyle, highlightingStyles, styleToCss)

renderCss :: Text -> Text
renderCss styleName =
    TL.toStrict (C.render stylesheet) <> "\n" <> highlightCss styleName

highlightCss :: Text -> Text
highlightCss styleName = T.pack (styleToCss (fromMaybe defaultStyle (lookup styleName highlightingStyles)))

stylesheet :: C.Css
stylesheet = do
    C.body C.? do
        C.maxWidth (C.px 800)
        C.marginLeft C.auto
        C.marginRight C.auto
        C.paddingTop (C.px 24)
        C.paddingLeft (C.px 16)
        C.paddingRight (C.px 16)
        C.fontFamily ["Helvetica Neue", "PingFang SC", "Hiragino Sans GB", "Microsoft YaHei"] [C.sansSerif]
        C.fontSize (C.px 17)
        C.lineHeight (C.px 28)
        C.color (C.rgb 51 51 51)
    C.a C.? do
        C.color (C.rgb 0 102 204)
        C.textDecoration C.underline
    C.a C.# C.hover C.? C.color (C.rgb 0 85 170)
    C.h1 C.? do
        C.fontSize (C.px 28)
        C.lineHeight (C.px 40)
    C.code C.? do
        C.backgroundColor (C.rgb 245 245 245)
        C.paddingLeft (C.px 4)
        C.paddingRight (C.px 4)
        C.borderRadius (C.px 3) (C.px 3) (C.px 3) (C.px 3)
    C.pre C.? do
        C.backgroundColor (C.rgb 245 245 245)
        C.padding (C.px 12) (C.px 16) (C.px 12) (C.px 16)
        C.borderRadius (C.px 4) (C.px 4) (C.px 4) (C.px 4)
        C.overflowX C.scroll
    C.pre C.? C.code C.? C.backgroundColor C.transparent
    C.ul C.? C.listStyleType C.none
    C.li C.? C.marginBottom (C.px 8)
    C.nav C.? C.marginBottom (C.px 24)
    C.nav C.? C.a C.? C.marginRight (C.px 12)
    C.a C.# ("aria-hidden" C.@= "true") C.? C.display C.none
