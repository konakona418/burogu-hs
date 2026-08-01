module Init (run) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.Exit (exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)

configTemplate :: Text
configTemplate =
    T.unlines
        [ "siteName: burogu"
        , "baseUrl: https://example.com"
        , "siteAuthor: Your Name"
        , "siteDescription: A blog generated with burogu"
        , "siteLang: zh-CN"
        , "tagsLabel: Tags"
        , "# deployTarget: user@host:/var/www/lizi.moe   # optional rsync target for `deploy`"
        , "# srcRepo: git@github.com:user/burogu-src.git # optional git repo for `sync`"
        , "theme:"
        , "  math: mathjax          # none | mathjax | katex"
        , "  extraCss: [theme.css]"
        , "  # extraJs: [theme.js]  # optional: JS files under src/ loaded on every page"
        ]

aboutTemplate :: Text
aboutTemplate =
    T.unlines
        [ "---"
        , "title: About"
        , "---"
        , ""
        , "# About"
        , ""
        , "Write something about yourself here. This page lives in `_pages/about.md`;"
        , "the frontmatter `title` becomes both the nav link label and the page title."
        , "Remove the file to drop the page (and its nav link) entirely."
        ]

notFoundTemplate :: Text
notFoundTemplate =
    T.unlines
        [ "---"
        , "title: 404"
        , "---"
        , ""
        , "# Page not found"
        , ""
        , "The page you are looking for does not exist. [Back to the home page](/)."
        ]

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
        createDirectoryIfMissing True (target </> "_pages")
        TIO.writeFile (target </> "_post" </> "2026-07-31-hello-world.md") samplePost
        TIO.writeFile (target </> "_pages" </> "404.md") notFoundTemplate
        TIO.writeFile (target </> "_pages" </> "about.md") aboutTemplate
        TIO.writeFile (target </> "CNAME") "example.com\n"
        TIO.writeFile (target </> "theme.css") themeCss
        writeConfig
        TIO.putStrLn ("initialized " <> T.pack target <> ":")
        TIO.putStrLn ("  " <> T.pack target <> "/_post/2026-07-31-hello-world.md")
        TIO.putStrLn ("  " <> T.pack target <> "/_pages/404.md")
        TIO.putStrLn ("  " <> T.pack target <> "/_pages/about.md")
        TIO.putStrLn ("  " <> T.pack target <> "/CNAME")
        TIO.putStrLn ("  " <> T.pack target <> "/theme.css")
        TIO.putStrLn "edit config.yaml if needed, then run: cabal run burogu -- preview"
    writeConfig = do
        let configPath = takeDirectory target </> "config.yaml"
        already <- doesFileExist configPath
        if already
            then TIO.putStrLn ("  " <> T.pack configPath <> " (already exists, left untouched)")
            else do
                TIO.writeFile configPath configTemplate
                TIO.putStrLn ("  " <> T.pack configPath)
