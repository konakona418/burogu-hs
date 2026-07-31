module Init (run) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word8)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
 )
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (stderr)

samplePost :: Text
samplePost =
    T.unlines
        [ "---"
        , "title: Hello, World"
        , "date: 2026-07-31"
        , "tags: [getting-started, demo]"
        , "description: A sample post showcasing the features of burogu"
        , "---"
        , ""
        , "Welcome! This sample post demonstrates what burogu can do."
        , ""
        , "## Text"
        , ""
        , "**Bold**, *italic*, and a [link](/posts/hello-world/). Bare URLs are linked automatically: https://example.com"
        , ""
        , "> A blockquote, for good measure."
        , ""
        , "## Code"
        , ""
        , "```c"
        , "#include <stdio.h>"
        , ""
        , "int main(void) {"
        , "    printf(\"Hello, burogu!\\n\");"
        , "    return 0;"
        , "}"
        , "```"
        , ""
        , "## Math"
        , ""
        , "Inline $E = mc^2$ and a display formula:"
        , ""
        , "$$"
        , "\\int_0^1 x^2 \\, dx = \\frac{1}{3}"
        , "$$"
        , ""
        , "## Image"
        , ""
        , "![Sample image](/img/00/1.png)"
        , ""
        , "## Table"
        , ""
        , "| Feature | Status |"
        , "|---------|--------|"
        , "| Markdown | yes |"
        , "| Syntax highlighting | yes |"
        , "| Math | yes |"
        ]

themeCss :: Text
themeCss =
    T.unlines
        [ "/* Your custom styles are appended to style.css."
        , "   Enable them in config.yaml: theme.extraCss: [theme.css] */"
        ]

pngBytes :: [Word8]
pngBytes =
    [ 0x89
    , 0x50
    , 0x4e
    , 0x47
    , 0x0d
    , 0x0a
    , 0x1a
    , 0x0a
    , 0x00
    , 0x00
    , 0x00
    , 0x0d
    , 0x49
    , 0x48
    , 0x44
    , 0x52
    , 0x00
    , 0x00
    , 0x00
    , 0x01
    , 0x00
    , 0x00
    , 0x00
    , 0x01
    , 0x08
    , 0x02
    , 0x00
    , 0x00
    , 0x00
    , 0x90
    , 0x77
    , 0x53
    , 0xde
    , 0x00
    , 0x00
    , 0x00
    , 0x0c
    , 0x49
    , 0x44
    , 0x41
    , 0x54
    , 0x78
    , 0x9c
    , 0x63
    , 0x60
    , 0x60
    , 0x60
    , 0x00
    , 0x00
    , 0x00
    , 0x04
    , 0x00
    , 0x01
    , 0xf6
    , 0x17
    , 0x38
    , 0x55
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x49
    , 0x45
    , 0x4e
    , 0x44
    , 0xae
    , 0x42
    , 0x60
    , 0x82
    ]

run :: FilePath -> IO ()
run target = do
    exists <- doesDirectoryExist target
    if exists
        then do
            entries <- listDirectory target
            if null entries
                then writeAll
                else do
                    TIO.hPutStrLn stderr ("error: " <> T.pack target <> " is not empty; refusing to initialize")
                    exitFailure
        else writeAll
  where
    writeAll = do
        createDirectoryIfMissing True (target </> "_post")
        createDirectoryIfMissing True (target </> "img" </> "00")
        TIO.writeFile (target </> "_post" </> "2026-07-31-hello-world.md") samplePost
        BS.writeFile (target </> "img" </> "00" </> "1.png") (BS.pack pngBytes)
        TIO.writeFile (target </> "CNAME") "example.com\n"
        TIO.writeFile (target </> "theme.css") themeCss
        TIO.putStrLn ("initialized " <> T.pack target <> ":")
        TIO.putStrLn ("  " <> T.pack target <> "/_post/2026-07-31-hello-world.md")
        TIO.putStrLn ("  " <> T.pack target <> "/img/00/1.png")
        TIO.putStrLn ("  " <> T.pack target <> "/CNAME")
        TIO.putStrLn ("  " <> T.pack target <> "/theme.css")
        TIO.putStrLn "edit config.yaml if needed, then run: cabal run burogu -- preview"
