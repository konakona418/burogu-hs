module Pandoc (docHasMath, readerOpts, wrapTables, writerOpts) where

import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..))
import Text.Pandoc.Extensions (Extension (..), pandocExtensions)
import Text.Pandoc.Highlighting (defaultStyle)
import Text.Pandoc.Options (HTMLMathMethod, HighlightMethod (..), ReaderOptions (..), WriterOptions (..), def, enableExtension)
import Text.Pandoc.Walk (query, walk)

readerOpts :: ReaderOptions
readerOpts =
    def
        { readerExtensions =
            enableExtension Ext_autolink_bare_uris (enableExtension Ext_gfm_auto_identifiers pandocExtensions)
        }

writerOpts :: HTMLMathMethod -> WriterOptions
writerOpts math =
    def
        { writerHighlightMethod = Skylighting defaultStyle
        , writerHTMLMathMethod = math
        }

docHasMath :: Pandoc -> Bool
docHasMath (Pandoc _ body) = not (null (query isMath body))
  where
    isMath :: Inline -> [()]
    isMath Math{} = [()]
    isMath _ = []

{- | Wrap every table in a div.table-scroll so it can scroll
horizontally on narrow screens without squashing its columns.
-}
wrapTables :: Pandoc -> Pandoc
wrapTables = walk wrapBlock
  where
    wrapBlock :: Block -> Block
    wrapBlock t@(Table _ _ _ _ _ _) = Div ("", ["table-scroll"], []) [t]
    wrapBlock b = b
