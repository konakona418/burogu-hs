module Init (run) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    listDirectory,
 )
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (stderr)
import System.Process (cwd, proc, readCreateProcessWithExitCode)

configTemplate :: Text
configTemplate =
    T.unlines
        [ "siteName: burogu"
        , "baseUrl: https://example.com"
        , "siteAuthor: Your Name"
        , "siteDescription: A blog generated with burogu"
        , "siteLang: zh-CN"
        , "tagsLabel: Tags"
        , "siteCopyright: © Your Name"
        , "siteGeneratedBy: Generated with Burogu  # footer credit; set to null to hide"
        , "# deploy:"
        , "#   target: user@host:/var/www/lizi.moe   # rsync deployment (VPS); either target or repo"
        , "#   repo: git@github.com:user/site.git    # git deployment (GitHub Pages etc.)"
        , "#   branch: gh-pages                      # git deployment only; required when repo is set"
        , "# srcRepo: git@github.com:user/burogu-src.git # optional git repo for `sync`"
        , "theme:"
        , "  math: mathjax          # none | mathjax | katex"
        , "  mathUrl: https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js # optional: override the CDN URL"
        , "  extraCss: [theme.css]"
        , "  # extraJs: [theme.js]  # optional: JS files under src/ loaded on every page"
        ]

gitignoreTemplate :: Text
gitignoreTemplate =
    T.unlines
        [ "# build output"
        , "/site"
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
        let configDir = takeDirectory target
            configPath = configDir </> "config.yaml"
        already <- doesFileExist configPath
        if already
            then TIO.putStrLn ("  " <> T.pack configPath <> " (already exists, left untouched)")
            else do
                TIO.writeFile configPath configTemplate
                TIO.putStrLn ("  " <> T.pack configPath)
        initConfigRepo configDir

{- | Make sure the directory holding config.yaml is a git repository,
so the whole site (config + src) is version-controlled; only the build
output is ignored. Creates an initial commit. No-op when the directory
is already inside a work tree.
-}
initConfigRepo :: FilePath -> IO ()
initConfigRepo configDir = do
    inTree <- gitOk configDir ["rev-parse", "--is-inside-work-tree"]
    if inTree
        then TIO.putStrLn ("  " <> T.pack configDir <> " (already inside a git repo, left untouched)")
        else do
            inited <- gitOk configDir ["init", "--quiet"]
            if inited
                then do
                    TIO.writeFile (configDir </> ".gitignore") gitignoreTemplate
                    added <- gitOk configDir ["add", "-A"]
                    committed <- gitOk configDir ["commit", "-q", "-m", "init: site scaffold"]
                    TIO.putStrLn ("  " <> T.pack configDir <> "/.git + .gitignore (initialized)")
                    unless (added && committed) $
                        TIO.hPutStrLn stderr "warning: initial commit failed; set git user.name/user.email and run `git commit` manually"
                else TIO.hPutStrLn stderr "warning: git not available; skipping repository initialization"

gitOk :: FilePath -> [String] -> IO Bool
gitOk dir args = do
    result <- try (readCreateProcessWithExitCode (proc "git" args){cwd = Just dir} "") :: IO (Either IOException (ExitCode, String, String))
    case result of
        Left _ -> pure False
        Right (code, _, _) -> pure (code == ExitSuccess)
