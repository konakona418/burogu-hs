module Pandoc (docHasMath, readerOpts, writerOpts) where

import Text.Pandoc.Definition (Inline (..), Pandoc (..))
import Text.Pandoc.Extensions (Extension (..), pandocExtensions)
import Text.Pandoc.Highlighting (defaultStyle)
import Text.Pandoc.Options (HTMLMathMethod, HighlightMethod (..), ReaderOptions (..), WriterOptions (..), def, enableExtension)
import Text.Pandoc.Walk (query)

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
