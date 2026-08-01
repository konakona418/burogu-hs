module Main where

import Cli (Command (..), Paths (..), cliInfo)
import Config (DeployConfig (..), SiteConfig (..), Theme (..))
import Css (TokenColor (..), renderCss, tokenColors)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as TL
import Feed (feedUrl, renderAtom)
import Html (groupByTag, render404, renderCustomPage, renderIndex, renderPost, renderTagArchive, renderTagIndex, tagUrl)
import Lucid qualified as L
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Page (CustomPage (..), loadPage, loadPages)
import Post (Post (..), mathMethod, parsePost, warnCaseTags)
import Sitemap (renderSitemap)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Pandoc.Options (HTMLMathMethod (..), defaultMathJaxURL)
import Watch (contentType, parsePath, resolveFile)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Post"
        [ fullFrontmatter
        , dateFromFilenamePrefix
        , frontmatterDateBeatsPrefix
        , invalidDateErrors
        , missingDateErrors
        , titleDefaultsToSlug
        , nonAsciiSlugPreserved
        , tagsParsed
        , tagsWrongTypeErrors
        , emptyTagErrors
        , whitespaceTagErrors
        , reservedCharTagErrors
        , draftParsed
        , draftAllowsMissingDate
        , bodyRendered
        , highlightedCode
        , caseVariantWarning
        , noCaseVariantWarning
        , tagGrouping
        , tagGroupingPreservesOrder
        , tagUrlTest
        , feedUrlTest
        , feedXmlDeclaration
        , feedContainsEntryLink
        , feedUpdated
        , feedEscaping
        , feedSummaryShown
        , feedSummaryHidden
        , ogMetaOnPost
        , ogTypeWebsiteOnIndex
        , ogUrlAbsentWithoutBaseUrl
        , ogDescriptionFallsBack
        , mathjaxInlineMath
        , mathjaxNoMathNoScript
        , plainMathNoScript
        , mathMethodMapping
        , tagsLabelCustomized
        , tagArchiveTitleIsTagName
        , cssRootTokens
        , cssDarkMediaQuery
        , cssTokenRules
        , cssTokenTableCompleteness
        , cssGradientRules
        , cssListSpacing
        , cssMobileBreakpoint
        , cssOverflowRules
        , cssSafeArea
        , viewportFitCover
        , cssExtraCssAppended
        , tagCountHook
        , cliDefaults
        , cliOverrides
        , cliInvalidArg
        , sitemapUrls
        , sitemapLastmod
        , robotsContent
        , notFoundPage
        , extraJsInjected
        , serverParsePath
        , serverResolveFile
        , serverContentType
        , customPageTitle
        , customPageMissing
        , customPageBody
        , customPageHasMath
        , aboutNavShown
        , aboutNavHidden
        , copyrightCustom
        , footerCreditShown
        , footerCreditHidden
        , loadPagesFromDir
        , loadPagesMissingDir
        , loadPagesErrorAggregation
        , navMultiplePages
        ]

fullFrontmatter :: TestTree
fullFrontmatter =
    testCase "full frontmatter extracts all fields" $ do
        result <- parsePost plainMath "hello.md" frontmatter
        assertRight result $ \post -> do
            assertEqual "slug" "hello" (postSlug post)
            assertEqual "title" "Hello, World" (postTitle post)
            assertEqual "date" "2026-07-31" (postDate post)
            assertEqual "tags" ["essay", "test"] (postTags post)
            assertEqual "description" (Just "First test post") (postDescription post)

dateFromFilenamePrefix :: TestTree
dateFromFilenamePrefix =
    testCase "date falls back to filename prefix" $ do
        result <- parsePost plainMath "2026-07-31-hello.md" (frontmatterWith ["title: Hello"])
        assertRight result $ \post -> do
            assertEqual "date" "2026-07-31" (postDate post)
            assertEqual "slug drops the date prefix" "hello" (postSlug post)

frontmatterDateBeatsPrefix :: TestTree
frontmatterDateBeatsPrefix =
    testCase "frontmatter date wins over filename prefix" $ do
        result <- parsePost plainMath "2026-07-31-hello.md" (frontmatterWith ["title: Hello", "date: 2025-01-02"])
        assertRight result $ \post -> do
            assertEqual "date" "2025-01-02" (postDate post)
            assertEqual "slug still drops the date prefix" "hello" (postSlug post)

invalidDateErrors :: TestTree
invalidDateErrors =
    testCase "invalid frontmatter date is a hard error" $ do
        result <- parsePost plainMath "hello.md" (frontmatterWith ["title: Hello", "date: 2026/07/31"])
        assertLeft result
        result2 <- parsePost plainMath "2026-07-31-hello.md" (frontmatterWith ["title: Hello", "date: not-a-date"])
        assertLeft result2

missingDateErrors :: TestTree
missingDateErrors =
    testCase "missing date with no prefix is a hard error" $ do
        result <- parsePost plainMath "hello.md" (frontmatterWith ["title: Hello"])
        assertLeft result

titleDefaultsToSlug :: TestTree
titleDefaultsToSlug =
    testCase "missing title falls back to slug" $ do
        result <- parsePost plainMath "2026-07-31-hello.md" "---\ndate: 2026-07-31\n---\n"
        assertRight result $ \post -> assertEqual "title" "hello" (postTitle post)

nonAsciiSlugPreserved :: TestTree
nonAsciiSlugPreserved =
    testCase "non-ASCII filename is preserved as slug" $ do
        result <- parsePost plainMath "2026-08-02-你好世界.md" "---\ndate: 2026-08-02\n---\n"
        assertRight result $ \post -> assertEqual "slug" "你好世界" (postSlug post)

tagsParsed :: TestTree
tagsParsed =
    testCase "tags list is parsed" $ do
        result <- parsePost plainMath "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [a, b, c]"])
        assertRight result $ \post -> assertEqual "tags" ["a", "b", "c"] (postTags post)

tagsWrongTypeErrors :: TestTree
tagsWrongTypeErrors =
    testCase "tags with a non-list value is a hard error" $ do
        result <- parsePost plainMath "hello.md" (frontmatterWith ["tags: not-a-list"])
        assertLeft result

emptyTagErrors :: TestTree
emptyTagErrors =
    testCase "empty tag is a hard error" $ do
        result <- parsePost plainMath "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [\"\", b]"])
        assertLeft result

whitespaceTagErrors :: TestTree
whitespaceTagErrors =
    testCase "whitespace-only tag is a hard error" $ do
        result <- parsePost plainMath "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [\"   \"]"])
        assertLeft result

reservedCharTagErrors :: TestTree
reservedCharTagErrors =
    testCase "tag with a reserved character is a hard error" $ do
        result <- parsePost plainMath "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [C#]"])
        assertLeft result
        result2 <- parsePost plainMath "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [a b]"])
        assertLeft result2
        result3 <- parsePost plainMath "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [a/b]"])
        assertLeft result3

draftParsed :: TestTree
draftParsed =
    testCase "draft: true is parsed" $ do
        result <- parsePost plainMath "hello.md" (frontmatterWith ["title: Draft", "draft: true", "date: 2026-07-31"])
        assertRight result $ \post -> assertEqual "draft" True (postDraft post)

draftAllowsMissingDate :: TestTree
draftAllowsMissingDate =
    testCase "draft allows a missing date" $ do
        result <- parsePost plainMath "hello.md" (frontmatterWith ["title: Draft", "draft: true"])
        assertRight result $ \post -> assertEqual "draft" True (postDraft post)

bodyRendered :: TestTree
bodyRendered =
    testCase "body is rendered as an HTML fragment" $ do
        result <- parsePost plainMath "hello.md" "---\ntitle: Hello\ndate: 2026-07-31\n---\n# Heading\n\nBody text"
        assertRight result $ \post -> do
            assertBool "contains h1" ("<h1" `textIn` postBodyHtml post)
            assertBool "contains paragraph" ("Body text" `textIn` postBodyHtml post)

highlightedCode :: TestTree
highlightedCode =
    testCase "code blocks are syntax-highlighted with token classes" $ do
        result <- parsePost plainMath "hello.md" "---\ntitle: Hello\ndate: 2026-07-31\n---\n```haskell\n-- a comment\nmain = putStrLn \"hi\"\n```"
        assertRight result $ \post -> do
            assertBool "contains sourceCode class" ("sourceCode" `textIn` postBodyHtml post)
            assertBool "contains a token span" ("<span class=\"co\">" `textIn` postBodyHtml post)

caseVariantWarning :: TestTree
caseVariantWarning =
    testCase "tags differing only in case produce a warning" $
        assertBool
            "warning present"
            (warnCaseTags [postWithTags ["Haskell"], postWithTags ["haskell"]] /= [])

noCaseVariantWarning :: TestTree
noCaseVariantWarning =
    testCase "identical tags do not produce a warning" $
        assertBool
            "no warning"
            (null (warnCaseTags [postWithTags ["Haskell"], postWithTags ["Haskell"]]))

tagGrouping :: TestTree
tagGrouping =
    testCase "groupByTag groups by tag in alphabetical order" $
        assertEqual
            "groups"
            [("Haskell", [postWithTags ["Haskell"]]), ("essay", [postWithTags ["essay"]])]
            (groupByTag [postWithTags ["essay"], postWithTags ["Haskell"]])

tagGroupingPreservesOrder :: TestTree
tagGroupingPreservesOrder =
    testCase "groupByTag preserves the given post order within a tag" $
        assertEqual
            "grouped posts"
            ["older", "newer"]
            (map postTitle (snd (head (groupByTag [olderPost, newerPost]))))
  where
    olderPost = (postWithTags ["essay"]){postTitle = "older", postDate = "2026-01-01"}
    newerPost = (postWithTags ["essay"]){postTitle = "newer", postDate = "2026-02-01"}

tagUrlTest :: TestTree
tagUrlTest =
    testCase "tagUrl points at the tag archive" $
        assertEqual "url" "/tags/Haskell/" (tagUrl "Haskell")

feedUrlTest :: TestTree
feedUrlTest =
    testCase "feedUrl appends feed.xml" $
        assertEqual "url" "https://lizi.moe/feed.xml" (feedUrl "https://lizi.moe")

feedXmlDeclaration :: TestTree
feedXmlDeclaration =
    testCase "feed starts with the XML declaration" $
        assertBool
            "declaration"
            ("<?xml version=\"1.0\" encoding=\"utf-8\"?>" `T.isPrefixOf` renderAtom testConfig "https://lizi.moe" [postWithTags []])

feedContainsEntryLink :: TestTree
feedContainsEntryLink =
    testCase "entries link to absolute post URLs" $
        assertBool
            "entry link"
            ("href=\"https://lizi.moe/posts/test/\"" `textIn` renderAtom testConfig "https://lizi.moe" [postWithTags []])

feedUpdated :: TestTree
feedUpdated =
    testCase "feed and entry carry the post date as updated" $
        assertBool
            "updated"
            ("<updated>2026-01-01</updated>" `textIn` renderAtom testConfig "https://lizi.moe" [postWithTags []])

feedEscaping :: TestTree
feedEscaping =
    testCase "titles and bodies are XML-escaped" $ do
        let feed = renderAtom testConfig "https://lizi.moe" [escapedPost]
        assertBool "title escaped" ("A &amp; B" `textIn` feed)
        assertBool "body escaped" ("&lt;p&gt;hi&lt;/p&gt;" `textIn` feed)

feedSummaryShown :: TestTree
feedSummaryShown =
    testCase "summary is present when a description exists" $
        assertBool
            "summary"
            ("<summary>desc</summary>" `textIn` renderAtom testConfig "https://lizi.moe" [escapedPost])

feedSummaryHidden :: TestTree
feedSummaryHidden =
    testCase "summary is absent without a description" $
        assertBool
            "no summary"
            ("<summary" `notTextIn` renderAtom testConfig "https://lizi.moe" [postWithTags []])

plainMath :: HTMLMathMethod
plainMath = PlainMath

mathJax :: HTMLMathMethod
mathJax = MathJax defaultMathJaxURL

mathjaxInlineMath :: TestTree
mathjaxInlineMath =
    testCase "MathJax renders inline math with TeX delimiters and injects the script" $ do
        result <- parsePost mathJax "hello.md" "---\ntitle: Hello\ndate: 2026-07-31\n---\nInline $x^2$ math"
        assertRight result $ \post -> do
            assertBool "math span" ("<span class=\"math inline\">\\(x^2\\)</span>" `textIn` postBodyHtml post)
            assertBool "no script in body fragment" ("<script" `notTextIn` postBodyHtml post)
            assertBool "post marked as having math" (postHasMath post)
            let page = renderHtml (renderPost testConfig [] post)
            assertBool "script injected in page" ("<script" `textIn` page)
            assertBool "mathjax URL" ("mathjax" `textIn` page)

mathjaxNoMathNoScript :: TestTree
mathjaxNoMathNoScript =
    testCase "no script is injected without math content" $ do
        result <- parsePost mathJax "hello.md" "---\ntitle: Hello\ndate: 2026-07-31\n---\nNo math here"
        assertRight result $ \post -> do
            assertBool "no script in body" ("<script" `notTextIn` postBodyHtml post)
            assertBool "not marked as having math" (not (postHasMath post))
            let page = renderHtml (renderPost testConfig [] post)
            assertBool "no script in page" ("<script" `notTextIn` page)

plainMathNoScript :: TestTree
plainMathNoScript =
    testCase "PlainMath renders without a script tag" $ do
        result <- parsePost plainMath "hello.md" "---\ntitle: Hello\ndate: 2026-07-31\n---\nInline $x^2$ math"
        assertRight result $ \post -> do
            assertBool "math span present" ("math inline" `textIn` postBodyHtml post)
            assertBool "no script in body" ("<script" `notTextIn` postBodyHtml post)
            let noMathConfig = testConfig{siteTheme = (siteTheme testConfig){themeMath = "none"}}
            assertBool "no script in page" ("<script" `notTextIn` renderHtml (renderPost noMathConfig [] post))

mathMethodMapping :: TestTree
mathMethodMapping =
    testCase "mathMethod maps names to pandoc methods" $ do
        assertEqual "none" PlainMath (mathMethod "none" Nothing)
        assertEqual "katex" (KaTeX "https://cdn.example.com/katex/") (mathMethod "katex" (Just "https://cdn.example.com/katex/"))
        assertEqual "mathjax with url" (MathJax "https://cdn.example.com/mathjax.js") (mathMethod "mathjax" (Just "https://cdn.example.com/mathjax.js"))
        assertEqual "mathjax default url" (MathJax defaultMathJaxURL) (mathMethod "mathjax" Nothing)

tagsLabelCustomized :: TestTree
tagsLabelCustomized =
    testCase "custom tagsLabel appears in the nav and tag index title" $ do
        let zhConfig = testConfig{siteTagsLabel = "标签"}
            nav = renderHtml (renderIndex zhConfig [] [])
        assertBool "nav label" (">标签</a>" `textIn` nav)
        let indexPage = renderHtml (renderTagIndex zhConfig [] [("essay", [postWithTags ["essay"]])])
        assertBool "index title" ("<title>标签</title>" `textIn` indexPage)

tagArchiveTitleIsTagName :: TestTree
tagArchiveTitleIsTagName =
    testCase "tag archive title is the bare tag name" $ do
        let page = renderHtml (renderTagArchive testConfig [] "essay" [postWithTags ["essay"]])
        assertBool "title" ("<title>essay</title>" `textIn` page)
        assertBool "no prefix" ("Tag:" `notTextIn` page)

cssRootTokens :: TestTree
cssRootTokens =
    testCase "stylesheet declares design tokens on :root" $ do
        let css = renderCss []
        assertBool ":root block" (":root" `textIn` css)
        assertBool "color token" ("--color-text" `textIn` css)
        assertBool "font token" ("--font-family" `textIn` css)
        assertBool "token variable" ("--token-co" `textIn` css)

cssDarkMediaQuery :: TestTree
cssDarkMediaQuery =
    testCase "stylesheet ships a dark token set via prefers-color-scheme" $ do
        let css = renderCss []
        assertBool "media query" ("prefers-color-scheme: dark" `textIn` css)
        assertBool "dark background" ("#1a1a1a" `textIn` css)

cssTokenRules :: TestTree
cssTokenRules =
    testCase "token rules reference CSS variables" $ do
        let css = renderCss []
        assertBool "comment rule" ("code span.co" `textIn` css)
        assertBool "var reference" ("var(--token-co)" `textIn` css)

cssTokenTableCompleteness :: TestTree
cssTokenTableCompleteness =
    testCase "token table covers all kate classes with light and dark values" $ do
        let classes = map tcClass tokenColors
        assertEqual
            "kate class set"
            (sort ["al", "an", "at", "bn", "bu", "cf", "ch", "cn", "co", "cv", "do", "dt", "dv", "er", "ex", "fl", "fu", "im", "in", "kw", "op", "ot", "pp", "re", "sc", "ss", "st", "va", "vs", "wa"])
            (sort classes)
        assertBool "light values present" (all (not . T.null . tcLight) tokenColors)
        assertBool "dark values present" (all (not . T.null . tcDark) tokenColors)

cssGradientRules :: TestTree
cssGradientRules =
    testCase "tag gradient consumes the tag-count hook" $ do
        let css = renderCss []
        assertBool "hue computation" ("var(--tag-count)" `textIn` css)
        assertBool "hsl usage" ("hsl(var(--tag-hue)" `textIn` css)

cssListSpacing :: TestTree
cssListSpacing =
    testCase "list items space out date, title and tags" $ do
        let css = renderCss []
        assertBool "post-item is flex with gap" ("0 var(--space-list-gap)" `textIn` css)
        assertBool "post-meta is flex with gap" (".post-meta" `textIn` css)
        assertBool "tag-item pairs name and count" ("0 6px" `textIn` css)

cssMobileBreakpoint :: TestTree
cssMobileBreakpoint =
    testCase "a 600px breakpoint scales tokens down for phones" $ do
        let css = renderCss []
        assertBool "media query" ("max-width: 600px" `textIn` css)
        assertBool "smaller font" ("--font-size" `textIn` css)
        assertBool "smaller code" ("font-size : 14px" `textIn` css)

cssOverflowRules :: TestTree
cssOverflowRules =
    testCase "images and tables cannot overflow the content area" $ do
        let css = renderCss []
        assertBool "img constrained" ("max-width : 100%" `textIn` css)
        assertBool "table scrolls" ("overflow-x : auto" `textIn` css)

cssSafeArea :: TestTree
cssSafeArea =
    testCase "body padding accounts for notched-device safe areas" $ do
        let css = renderCss []
        assertBool "left inset" ("safe-area-inset-left" `textIn` css)
        assertBool "right inset" ("safe-area-inset-right" `textIn` css)
        assertBool "tap highlight removed" ("tap-highlight-color" `textIn` css)

viewportFitCover :: TestTree
viewportFitCover =
    testCase "viewport meta opts into full-screen safe areas" $ do
        let page = renderHtml (renderIndex testConfig [] [])
        assertBool "viewport-fit=cover" ("viewport-fit=cover" `textIn` page)

cssExtraCssAppended :: TestTree
cssExtraCssAppended =
    testCase "user CSS is appended after the generated rules" $ do
        let css = renderCss ["/* user css */"]
        assertBool "appended at the end" ("/* user css */" `T.isSuffixOf` css)

tagCountHook :: TestTree
tagCountHook =
    testCase "tag items expose the post count as a CSS variable hook" $ do
        let page = renderHtml (renderTagIndex testConfig [] [("essay", [postWithTags ["essay"], postWithTags ["essay"]])])
        assertBool "hook present" ("style=\"--tag-count: 2\"" `textIn` page)

cliDefaults :: TestTree
cliDefaults =
    testCase "build paths default to config.yaml, src, site" $ do
        let result = execParserPure defaultPrefs cliInfo ["build"]
        case result of
            Success (Build paths) -> do
                assertEqual "config" "config.yaml" (pConfig paths)
                assertEqual "src" "src" (pSrc paths)
                assertEqual "out" "site" (pOut paths)
            Failure err -> assertBool ("expected success, got: " <> show err) False
            CompletionInvoked _ -> assertBool "unexpected completion" False
            Success _ -> assertBool "expected Build command" False

cliOverrides :: TestTree
cliOverrides =
    testCase "build paths are overridden by flags" $ do
        let result = execParserPure defaultPrefs cliInfo ["build", "--config", "cfg.yaml", "--src", "content", "--out", "dist"]
        case result of
            Success (Build paths) -> do
                assertEqual "config" "cfg.yaml" (pConfig paths)
                assertEqual "src" "content" (pSrc paths)
                assertEqual "out" "dist" (pOut paths)
            Failure err -> assertBool ("expected success, got: " <> show err) False
            CompletionInvoked _ -> assertBool "unexpected completion" False
            Success _ -> assertBool "expected Build command" False

cliInvalidArg :: TestTree
cliInvalidArg =
    testCase "unknown flags are rejected" $ do
        let result = execParserPure defaultPrefs cliInfo ["--nope"]
        case result of
            Failure _ -> assertBool "rejected" True
            Success _ -> assertBool "should have failed" False
            CompletionInvoked _ -> assertBool "should have failed" False

sitemapUrls :: TestTree
sitemapUrls =
    testCase "sitemap lists index, posts, tags and feed with absolute URLs" $ do
        let xml = renderSitemap "https://lizi.moe" [postWithTags ["essay"]] []
        assertBool "urlset" ("<urlset" `textIn` xml)
        assertBool "index" ("https://lizi.moe/" `textIn` xml)
        assertBool "post" ("https://lizi.moe/posts/test/" `textIn` xml)
        assertBool "tag index" ("https://lizi.moe/tags/" `textIn` xml)
        assertBool "tag archive" ("https://lizi.moe/tags/essay/" `textIn` xml)
        assertBool "feed" ("https://lizi.moe/feed.xml" `textIn` xml)

sitemapLastmod :: TestTree
sitemapLastmod =
    testCase "posts carry their date as lastmod" $ do
        let xml = renderSitemap "https://lizi.moe" [postWithTags ["essay"]] []
        assertBool "lastmod" ("<lastmod>2026-01-01</lastmod>" `textIn` xml)

robotsContent :: TestTree
robotsContent =
    testCase "robots.txt allows crawling and points at the sitemap" $ do
        let withBase = "User-agent: *\nAllow: /\nSitemap: https://lizi.moe/sitemap.xml\n"
        assertEqual "with baseUrl" withBase (robotsText (Just "https://lizi.moe"))
        assertEqual "without baseUrl" "User-agent: *\nAllow: /\n" (robotsText Nothing)

notFoundPage :: TestTree
notFoundPage =
    testCase "404 page renders with a home link" $ do
        let page = renderHtml (render404 testConfig [])
        assertBool "title" ("<title>404</title>" `textIn` page)
        assertBool "heading" ("<h1>404</h1>" `textIn` page)
        assertBool "home link" ("href=\"/\"" `textIn` page)

robotsText :: Maybe Text -> Text
robotsText baseUrl =
    "User-agent: *\n"
        <> "Allow: /\n"
        <> maybe "" (\b -> "Sitemap: " <> b <> "/sitemap.xml\n") baseUrl

extraJsInjected :: TestTree
extraJsInjected =
    testCase "extra JS files are injected as deferred scripts on every page" $ do
        let jsConfig = testConfig{siteTheme = (siteTheme testConfig){themeExtraJs = ["theme.js"]}}
        let page = renderHtml (renderIndex jsConfig [] [])
        assertBool "script tag" ("<script defer src=\"/theme.js\"" `textIn` page)
        assertBool "no script without extraJs" ("theme.js" `notTextIn` renderHtml (renderIndex testConfig [] []))

serverParsePath :: TestTree
serverParsePath =
    testCase "preview server parses GET paths with percent-decoding" $ do
        assertEqual "simple" (Just "/style.css") (parsePath "GET /style.css HTTP/1.1\r\nHost: x")
        assertEqual "query stripped" (Just "/tags/") (parsePath "GET /tags/?x=1 HTTP/1.1\r\n")
        assertEqual "percent-decoded" (Just ("/posts/" <> encodeUtf8 "你好" <> "/")) (parsePath "GET /posts/%E4%BD%A0%E5%A5%BD/ HTTP/1.1\r\n")
        assertEqual "non-GET rejected" Nothing (parsePath "POST / HTTP/1.1\r\n")

serverResolveFile :: TestTree
serverResolveFile =
    testCase "preview server resolves paths against the output directory" $ do
        assertEqual "root" (Just "site/index.html") (resolveFile "site" "/")
        assertEqual "directory URL" (Just "site/posts/hello/index.html") (resolveFile "site" "/posts/hello/")
        assertEqual "file" (Just "site/img/00/1.png") (resolveFile "site" "/img/00/1.png")
        assertEqual "traversal rejected" Nothing (resolveFile "site" "/../etc/passwd")

serverContentType :: TestTree
serverContentType =
    testCase "preview server maps file extensions to content types" $ do
        assertEqual "html" "text/html; charset=utf-8" (contentType "index.html")
        assertEqual "css" "text/css; charset=utf-8" (contentType "style.css")
        assertEqual "png" "image/png" (contentType "1.png")

customPageTitle :: TestTree
customPageTitle =
    testCase "custom pages read the optional frontmatter title" $ do
        writeFile "/tmp/burogu-test/custom-page-title.md" "---\ntitle: About Me\n---\n# About Me\n\nSome bio.\n"
        result <- loadPage plainMath "/tmp/burogu-test/custom-page-title.md"
        case result of
            Right (Just page) -> assertEqual "title" (Just "About Me") (cpTitle page)
            _ -> assertBool "expected a page" False

customPageMissing :: TestTree
customPageMissing =
    testCase "missing custom page files yield Nothing" $ do
        result <- loadPage plainMath "/tmp/burogu-test/does-not-exist-404.md"
        assertEqual "missing" (Right Nothing) result

customPageBody :: TestTree
customPageBody =
    testCase "custom page bodies are rendered from markdown" $ do
        writeFile "/tmp/burogu-test/custom-page-body.md" "# No title\n\nBody.\n"
        result <- loadPage plainMath "/tmp/burogu-test/custom-page-body.md"
        case result of
            Right (Just page) -> assertBool "h1 rendered" ("<h1" `textIn` cpBodyHtml page)
            _ -> assertBool "expected a page" False

customPageHasMath :: TestTree
customPageHasMath =
    testCase "custom pages detect math for script injection" $ do
        writeFile "/tmp/burogu-test/custom-page-math.md" "# Math\n\n$x^2$\n"
        result <- loadPage plainMath "/tmp/burogu-test/custom-page-math.md"
        case result of
            Right (Just page) -> assertBool "hasMath" (cpHasMath page)
            _ -> assertBool "expected a page" False

aboutNavShown :: TestTree
aboutNavShown =
    testCase "about link appears in the nav when present" $ do
        let page = renderHtml (renderIndex testConfig [("About Me", "/about/")] [])
        assertBool "nav link" ("href=\"/about/\">About Me" `textIn` page)

aboutNavHidden :: TestTree
aboutNavHidden =
    testCase "no about link without an about page" $ do
        let page = renderHtml (renderIndex testConfig [] [])
        assertBool "no link" ("/about/" `notTextIn` page)

copyrightCustom :: TestTree
copyrightCustom =
    testCase "footer uses the configurable copyright" $ do
        let page = renderHtml (renderIndex testConfig{siteCopyright = "自定义版权"} [] [])
        assertBool "custom copyright" ("自定义版权" `textIn` page)

footerCreditShown :: TestTree
footerCreditShown =
    testCase "footer shows the configured generator credit on the same line" $ do
        let page = renderHtml (renderIndex testConfig{siteGeneratedBy = Just "Generated with Burogu"} [] [])
        assertBool "joined line" ("© moe li · Generated with Burogu" `textIn` page)

footerCreditHidden :: TestTree
footerCreditHidden =
    testCase "footer omits the generator credit when unset" $ do
        let page = renderHtml (renderIndex testConfig [] [])
        assertBool "no credit text" ("site-credit" `notTextIn` page)

loadPagesFromDir :: TestTree
loadPagesFromDir =
    testCase "loadPages reads all markdown pages with slugs in order" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/pages-a"
        writeFile "/tmp/burogu-test/pages-a/about.md" "---\ntitle: About\n---\n# About\n"
        writeFile "/tmp/burogu-test/pages-a/projects.md" "# Projects\n"
        result <- loadPages plainMath "/tmp/burogu-test/pages-a"
        case result of
            Right pages -> do
                assertEqual "slugs" ["about", "projects"] (map fst pages)
                assertEqual "titles" [Just "About", Nothing] (map (cpTitle . snd) pages)
            Left errs -> assertBool ("expected success, got: " <> show errs) False

loadPagesMissingDir :: TestTree
loadPagesMissingDir =
    testCase "loadPages on a missing directory yields an empty list" $ do
        result <- loadPages plainMath "/tmp/burogu-test/no-such-pages"
        assertEqual "empty" (Right []) result

loadPagesErrorAggregation :: TestTree
loadPagesErrorAggregation =
    testCase "loadPages aggregates parse errors with filenames" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/pages-bad"
        writeFile "/tmp/burogu-test/pages-bad/ok.md" "# Fine\n"
        BS.writeFile "/tmp/burogu-test/pages-bad/broken.md" (BS.pack [0xff, 0xfe])
        result <- loadPages plainMath "/tmp/burogu-test/pages-bad"
        case result of
            Left errs -> assertBool "filename in error" (any ("broken.md" `T.isInfixOf`) errs)
            Right _ -> assertBool "expected failure" False

navMultiplePages :: TestTree
navMultiplePages =
    testCase "nav renders multiple pages in order" $ do
        let page = renderHtml (renderIndex testConfig [("About", "/about/"), ("Projects", "/projects/")] [])
        assertBool "about link" ("href=\"/about/\">About" `textIn` page)
        assertBool "projects link" ("href=\"/projects/\">Projects" `textIn` page)
        let aboutPos = findSubstring "href=\"/about/\"" page
            projectsPos = findSubstring "href=\"/projects/\"" page
        assertBool "order" (aboutPos < projectsPos)

findSubstring :: Text -> Text -> Int
findSubstring needle haystack =
    case T.breakOn needle haystack of
        (before, _) -> T.length before

escapedPost :: Post
escapedPost =
    (postWithTags ["essay"]){postTitle = "A & B", postBodyHtml = "<p>hi</p>", postDescription = Just "desc"}

ogMetaOnPost :: TestTree
ogMetaOnPost =
    testCase "post pages carry article OG metadata" $ do
        let html = renderHtml (renderPost testConfig [] (postWithTags ["essay"]))
        assertBool "og:title" ("og:title\" content=\"test\"" `textIn` html)
        assertBool "og:type" ("og:type\" content=\"article\"" `textIn` html)
        assertBool "og:url" ("og:url\" content=\"https://lizi.moe/posts/test/\"" `textIn` html)

ogTypeWebsiteOnIndex :: TestTree
ogTypeWebsiteOnIndex =
    testCase "index pages carry website OG metadata" $ do
        let html = renderHtml (renderIndex testConfig [] [postWithTags ["essay"]])
        assertBool "og:type" ("og:type\" content=\"website\"" `textIn` html)
        assertBool "og:url" ("og:url\" content=\"https://lizi.moe/\"" `textIn` html)

ogUrlAbsentWithoutBaseUrl :: TestTree
ogUrlAbsentWithoutBaseUrl =
    testCase "og:url is omitted without baseUrl" $ do
        let html = renderHtml (renderIndex testConfig{siteBaseUrl = Nothing} [] [])
        assertBool "no og:url" ("og:url" `notTextIn` html)
        assertBool "og:title still present" ("og:title" `textIn` html)

ogDescriptionFallsBack :: TestTree
ogDescriptionFallsBack =
    testCase "og:description falls back to siteDescription" $ do
        let html = renderHtml (renderPost testConfig [] (postWithTags []))
        assertBool "fallback description" ("og:description\" content=\"A test blog\"" `textIn` html)

renderHtml :: L.Html () -> Text
renderHtml = TL.toStrict . L.renderText

testConfig :: SiteConfig
testConfig =
    SiteConfig
        { siteName = "burogu"
        , siteAuthor = "moe li"
        , siteDescription = "A test blog"
        , siteLang = "zh-CN"
        , siteBaseUrl = Just "https://lizi.moe"
        , siteTagsLabel = "Tags"
        , siteCopyright = "© moe li"
        , siteGeneratedBy = Nothing
        , siteDeploy = DeployConfig{deployTarget = Nothing, deployRepo = Nothing, deployBranch = Nothing, deployCommitName = Nothing, deployCommitEmail = Nothing}
        , siteSrcRepo = Nothing
        , siteTheme = Theme{themeMath = "mathjax", themeMathUrl = Nothing, themeExtraCss = [], themeExtraJs = []}
        }

frontmatter :: Text
frontmatter =
    frontmatterWith
        [ "title: Hello, World"
        , "date: 2026-07-31"
        , "tags: [essay, test]"
        , "description: First test post"
        ]

frontmatterWith :: [Text] -> Text
frontmatterWith fields = "---\n" <> foldMap (<> "\n") fields <> "---\n\nBody"

postWithTags :: [Text] -> Post
postWithTags tags =
    Post
        { postSlug = "test"
        , postTitle = "test"
        , postDate = "2026-01-01"
        , postTags = tags
        , postDescription = Nothing
        , postDraft = False
        , postBodyHtml = ""
        , postHasMath = False
        }

assertRight :: Either Text Post -> (Post -> IO ()) -> IO ()
assertRight result check = case result of
    Left err -> assertBool ("expected Right, but got Left: " <> T.unpack err) False
    Right post -> check post

assertLeft :: Either Text Post -> IO ()
assertLeft result = assertBool "expected Left" (isLeft result)

textIn :: Text -> Text -> Bool
textIn needle haystack = needle `T.isInfixOf` haystack

notTextIn :: Text -> Text -> Bool
notTextIn needle haystack = not (needle `T.isInfixOf` haystack)
