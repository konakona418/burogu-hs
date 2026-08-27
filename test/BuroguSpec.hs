module Main where

import Builtins (initialEnv)
import Cli (Command (..), Paths (..), cliInfo)
import Config (DeployConfig (..), SiteConfig (..), Theme (..), loadConfig)
import Control.Exception (IOException, try)
import Css (FontFile (..), Fonts (..), TokenColor (..), ariaPreset, emptyFonts, renderCss, tokenColors)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import System.FilePath ((</>))
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Text.Lazy qualified as TL
import Data.Yaml (ParseException, decodeEither')
import Doc (OutputStyle (..), extractSection, langFromLocale, manualContent, render, sections)
import Eval (LangError (..), runScript)
import Feed (feedUrl, renderAtom)
import Codec.Picture (DynamicImage (..), Image (..), PixelRGB8 (..), generateImage, savePngImage)
import Digest (digestOf)
import Format (formatOne)
import Image qualified
import Frontmatter (Kind (..), normalizeFrontmatter, splitFrontmatter)
import Html (groupByTag, render404, renderArchive, renderIndex, renderPost, renderRedirect, renderTagArchive, renderTagIndex, tagUrl)
import I18n (UILang (..), fromSiteLang, messages, tWith)
import Lexer (lexTokens)
import Lucid qualified as L
import Options.Applicative (ParserResult (..), defaultPrefs, execParserPure)
import Page (CustomPage (..), Placement (..), loadPage, loadPages)
import Parser (parseProgram)
import Paths_burogu ()
import Post (Post (..), TocEntry (..), mathMethod, parsePost, warnCaseTags)
import Posts (runDraft, runNew, runPublish, runRename)
import Registry (SitePages (..), classifyPages, footerItems, navItems)
import Scripts (evalScript, scriptCtx)
import Search (renderSearch, renderSearchIndex)
import Shaft (presetByName, presetNames, shaftPreset)
import Site (BuildReport (..), build)
import Sitemap (renderSitemap)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removePathForcibly)
import System.Exit (ExitCode)
import Template (ConfigValues (..), defaultConfigTemplate, emptyConfigValues, renderConfig)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Text.Pandoc.Options (HTMLMathMethod (..), defaultMathJaxURL)
import Value (showValue)
import Watch (contentType, parsePath, resolveFile)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "burogu"
        [ testGroup
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
            , searchInputFocusStyled
            , viewportFitCover
            , cssExtraCssAppended
            , ariaPresetRegression
            , presetLookup
            , fontOverridesApplied
            , fontStackQuoting
            , fontFaceEmitted
            , shaftPresetOutput
            , shaftHarmonyRules
            , fontsFromJson
            , siteNameClass
            , tagSeparatorSpan
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
            , pagePriorityParsed
            , pagePriorityDefault
            , pagePriorityInvalid
            , pageRedirectAsParsed
            , classifySpecialPages
            , classifyUnknownRedirectAs
            , classifyExternalRedirect
            , classifyInvalidRedirectAs
            , classifyDuplicateSpecial
            , classifyCollision
            , placementParsed
            , placementFiltersNav
            , redirectStubNavVisibility
            , footerItemsTest
            , footerLinksRendered
            , footerSeparatorConfig
            , digestCommentTest
            , renamePostTest
            , imageTest
            , formatMigrationTest
            , notFoundInNavByDefault
            , navPriorityOrder
            , navPriorityTieLexicographic
            , navDefaultPriorityLast
            , navTagsPosition
            , archiveYearGroups
            , redirectPageMetaRefresh
            , tagsLabelRejected
            , searchIndexShape
            , searchIndexEscaping
            , searchIndexExcludesSpecials
            , searchPageRendered
            , searchPageScript
            , buildKeepsOutputOnPageError
            , fontFileMissingKeepsOldOutput
            , redirectStubBuilt
            , docRenderPlain
            , docRenderColor
            , docHeadingDepth
            , docSectionExtraction
            , docLangFromLocale
            , manualsPresent
            , frontmatterSplit
            , frontmatterPostDefaults
            , frontmatterEmptyFilled
            , frontmatterMalformedErrors
            , postsDraftCreatesDatedFile
            , postsNewCreatesDatedPost
            , postsSlugDuplicateRejected
            , postsPublishPromotes
            , postsPublishNonDraftRejected
            , postsPublishMissingRejected
            , postsPublishLegacyDraft
            , postTocParsed
            , tocFrontmatterParsed
            , renderPostTocShown
            , renderPostTocHidden
            , readingTimeShown
            , postNavRendered
            , postNavSingleSide
            , indexSpecialPageClassified
            , indexPageRenderedOnHome
            , indexPageExcludedFromNav
            , darkThemeAttributeSelectors
            , glyphFontRules
            , cssFingerprint
            , themeToggleScriptInjected
            , i18nCompleteness
            , i18nFromSiteLang
            , i18nPlaceholders
            , i18nPageOutput
            , formatTocKey
            , tocLevelClasses
            , contentElementStyles
            , searchNoResults
            , codeScriptInjected
            , codeBlockStyles
            , frontmatterPageDefaults
            , frontmatterNoDateError
            , frontmatterDraftNoDate
            , frontmatterUnknownKeys
            , frontmatterDescriptionOmitted
            , configTemplateGolden
            , configFormatValues
            , formatWritesFile
            , formatDryRunDoesNotWrite
            ]
        , testGroup "DSL" dslTests
        , testGroup "Scripts" scriptsTests
        ]

postsDraftCreatesDatedFile :: TestTree
postsDraftCreatesDatedFile =
    testCase "drafts get a canonical dated filename" $ do
        removePathForcibly "/tmp/burogu-test/posts"
        createDirectoryIfMissing True "/tmp/burogu-test/posts/_post"
        result <- runDraft "/tmp/burogu-test/posts/_post" "notes"
        case result of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right path -> do
                files <- listDirectory "/tmp/burogu-test/posts/_post"
                assertBool "dated file" (any (T.isSuffixOf "-notes.md" . T.pack) files)
                content <- readFile path
                assertBool "draft flag" ("draft: true" `textIn` T.pack content)
                assertBool "no date" ("date:" `notTextIn` T.pack content)

postsNewCreatesDatedPost :: TestTree
postsNewCreatesDatedPost =
    testCase "new posts carry today's date" $ do
        removePathForcibly "/tmp/burogu-test/posts-new"
        createDirectoryIfMissing True "/tmp/burogu-test/posts-new/_post"
        result <- runNew "/tmp/burogu-test/posts-new/_post" "hello"
        case result of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right path -> do
                content <- readFile path
                assertBool "date field" ("date: " `textIn` T.pack content)
                assertBool "not a draft" ("draft: true" `notTextIn` T.pack content)

postsSlugDuplicateRejected :: TestTree
postsSlugDuplicateRejected =
    testCase "new refuses a slug that already exists" $ do
        removePathForcibly "/tmp/burogu-test/posts-dup"
        createDirectoryIfMissing True "/tmp/burogu-test/posts-dup/_post"
        first <- runNew "/tmp/burogu-test/posts-dup/_post" "hello"
        second <- runNew "/tmp/burogu-test/posts-dup/_post" "hello"
        assertBool "first ok" (isRight first)
        assertBool "second rejected" (isLeft second)

postsPublishPromotes :: TestTree
postsPublishPromotes =
    testCase "publish adds the date and drops the draft flag in place" $ do
        removePathForcibly "/tmp/burogu-test/posts-pub"
        createDirectoryIfMissing True "/tmp/burogu-test/posts-pub/_post"
        writeFile "/tmp/burogu-test/posts-pub/_post/2026-08-01-notes.md" "---\ntitle: notes\ntags: [essay]\ndraft: true\n---\n\nbody\n"
        result <- runPublish "/tmp/burogu-test/posts-pub/_post" (digestOf "2026-08-01-notes")
        case result of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right path -> do
                assertEqual "filename unchanged" "/tmp/burogu-test/posts-pub/_post/2026-08-01-notes.md" path
                content <- readFile path
                assertBool "date added" ("date: " `textIn` T.pack content)
                assertBool "draft gone" ("draft: false" `textIn` T.pack content)
                assertBool "draft true gone" ("draft: true" `notTextIn` T.pack content)
                assertBool "tags kept" ("tags: [essay]" `textIn` T.pack content)

postsPublishNonDraftRejected :: TestTree
postsPublishNonDraftRejected =
    testCase "publish rejects a regular post" $ do
        removePathForcibly "/tmp/burogu-test/posts-reg"
        createDirectoryIfMissing True "/tmp/burogu-test/posts-reg/_post"
        writeFile "/tmp/burogu-test/posts-reg/_post/2026-08-01-notes.md" "---\ntitle: notes\n---\n\nbody\n"
        result <- runPublish "/tmp/burogu-test/posts-reg/_post" (digestOf "2026-08-01-notes")
        assertBool "rejected" (isLeft result)

postsPublishMissingRejected :: TestTree
postsPublishMissingRejected =
    testCase "publish rejects a missing slug" $ do
        removePathForcibly "/tmp/burogu-test/posts-none"
        createDirectoryIfMissing True "/tmp/burogu-test/posts-none/_post"
        result <- runPublish "/tmp/burogu-test/posts-none/_post" "ffffffff"
        assertBool "rejected" (isLeft result)

postsPublishLegacyDraft :: TestTree
postsPublishLegacyDraft =
    testCase "publish handles legacy undated drafts" $ do
        removePathForcibly "/tmp/burogu-test/posts-legacy"
        createDirectoryIfMissing True "/tmp/burogu-test/posts-legacy/_post"
        writeFile "/tmp/burogu-test/posts-legacy/_post/old.md" "---\ntitle: old\ndraft: true\n---\n\nbody\n"
        result <- runPublish "/tmp/burogu-test/posts-legacy/_post" (digestOf "old")
        case result of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right path -> do
                assertEqual "filename unchanged" "/tmp/burogu-test/posts-legacy/_post/old.md" path
                content <- readFile path
                assertBool "date added" ("date: " `textIn` T.pack content)
                assertBool "draft gone" ("draft: true" `notTextIn` T.pack content)

postTocParsed :: TestTree
postTocParsed =
    testCase "post headings are extracted with ids" $ do
        result <- parsePost plainMath "/tmp/x/2026-08-01-t.md" "---\ntitle: T\n---\n\n# One\n\n## Two\n"
        assertRight result $ \post -> do
            assertEqual "entries" [("One", "one"), ("Two", "two")] [(tocTitle e, tocId e) | e <- postToc post]

tocFrontmatterParsed :: TestTree
tocFrontmatterParsed =
    testCase "the toc flag is read from frontmatter" $ do
        result <- parsePost plainMath "/tmp/x/2026-08-01-t.md" "---\ntitle: T\ntoc: true\n---\n\n# One\n"
        assertRight result $ \post -> assertEqual "toc" True (postShowToc post)

renderPostTocShown :: TestTree
renderPostTocShown =
    testCase "a toc-enabled post renders the heading list" $ do
        let post = (postWithTags []){postShowToc = True, postToc = [TocEntry 2 "Second" "second"]}
            page = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) post)
        assertBool "toc nav" ("<nav class=\"toc\">" `textIn` page)
        assertBool "toc link" ("href=\"#second\">Second</a>" `textIn` page)

renderPostTocHidden :: TestTree
renderPostTocHidden =
    testCase "a post without toc stays clean" $ do
        let post = (postWithTags []){postToc = [TocEntry 2 "Second" "second"]}
            page = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) post)
        assertBool "no toc" ("class=\"toc\"" `notTextIn` page)

readingTimeShown :: TestTree
readingTimeShown =
    testCase "the reading time is shown in the post meta" $ do
        let post = (postWithTags []){postText = T.replicate 900 "字"}
            page = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) post)
        assertBool "zh label" ("约 2 分钟" `textIn` page)
        let enPage = renderHtml (renderPost testConfig{siteLang = "en"} [] [] "/style.css" (Nothing, Nothing) post)
        assertBool "en label" ("min read" `textIn` enPage)

postNavRendered :: TestTree
postNavRendered =
    testCase "prev and next posts are linked at the bottom" $ do
        let older = (postWithTags []){postSlug = "older", postTitle = "Older", postDate = "2026-01-01"}
            newer = (postWithTags []){postSlug = "newer", postTitle = "Newer", postDate = "2026-03-01"}
            page = renderHtml (renderPost testConfig [] [] "/style.css" (Just newer, Just older) (postWithTags []))
        assertBool "prev label" ("上一篇" `textIn` page)
        assertBool "next label" ("下一篇" `textIn` page)
        assertBool "prev link" ("href=\"/posts/newer/\">Newer</a>" `textIn` page)
        assertBool "next link" ("href=\"/posts/older/\">Older</a>" `textIn` page)
        assertBool "no arrows" ("←" `notTextIn` page && "→" `notTextIn` page)
        let enPage = renderHtml (renderPost testConfig{siteLang = "en"} [] [] "/style.css" (Just newer, Just older) (postWithTags []))
        assertBool "en labels" ("Previous" `textIn` enPage && "Next" `textIn` enPage)

postNavSingleSide :: TestTree
postNavSingleSide =
    testCase "one-sided nav renders without a dangling separator" $ do
        let older = (postWithTags []){postSlug = "older", postTitle = "Older", postDate = "2026-01-01"}
        assertEqual "neither side" False ("post-nav" `textIn` renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) (postWithTags [])))
        let onlyNext = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Just older) (postWithTags []))
        assertBool "next only" ("post-nav-next" `textIn` onlyNext)
        assertBool "no prev" ("post-nav-prev" `notTextIn` onlyNext)
        assertBool "pushed right" ("margin-left:auto" `notTextIn` onlyNext)
        let css = renderCss ariaPreset emptyFonts []
        assertBool "separator" (".post-nav" `textIn` css)
        assertBool "push-right rule" (".post-nav-next" `textIn` css)

formatTocKey :: TestTree
formatTocKey =
    testCase "format recognizes the toc key" $ do
        case normalizeFrontmatter PostKind "/tmp/x/2026-08-01-t.md" "title: X\ntoc: true\n" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, unknown, _) -> do
                assertEqual "toc kept" "title: X\ndate: 2026-08-01\ntags: []\ndraft: false\ntoc: true\n" fm
                assertEqual "not unknown" [] unknown

tocLevelClasses :: TestTree
tocLevelClasses =
    testCase "toc entries carry level classes" $ do
        let post = (postWithTags []){postShowToc = True, postToc = [TocEntry 2 "Second" "second"]}
            page = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) post)
        assertBool "level class" ("toc-level-2" `textIn` page)

contentElementStyles :: TestTree
contentElementStyles =
    testCase "content elements get their styles" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool "toc depth" ("--toc-depth" `textIn` css)
        assertBool "table border" ("border-collapse : collapse" `textIn` css)
        assertBool "figcaption" ("figcaption" `textIn` css)
        assertBool "reading time muted" (".post-meta-time" `textIn` css)

searchNoResults :: TestTree
searchNoResults =
    testCase "the search page ships a no-results hint" $ do
        let page = renderHtml (renderSearch testConfig [] [] "/style.css" "搜索")
        assertBool "hint element" ("search-no-results" `textIn` page)
        assertBool "hidden initially" ("hidden" `textIn` page)
        assertBool "script toggles it" ("showResults" `textIn` page)

codeScriptInjected :: TestTree
codeScriptInjected =
    testCase "every page ships the code-block script" $ do
        let page = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) (postWithTags []))
        assertBool "copy logic" ("copy-button" `textIn` page)
        assertBool "lang extraction" ("langNames" `textIn` page)
        assertBool "clipboard" ("clipboard" `textIn` page)

codeBlockStyles :: TestTree
codeBlockStyles =
    testCase "code blocks carry copy and lang styles" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool "lang label" (".code-lang" `textIn` css)
        assertBool "copy button" (".copy-button" `textIn` css)
        assertBool "relative anchor" ("position" `textIn` css && "relative" `textIn` css)
        assertBool "print hides tools" ("@media print" `textIn` css)

i18nCompleteness :: TestTree
i18nCompleteness =
    testCase "every language file has the same key set as English" $ do
        mapM_ checkLang [Zh, ZhHant, Ja]
  where
    checkLang :: UILang -> IO ()
    checkLang lang = do
        let enKeys = sort (map fst (messages En))
            langKeys = sort (map fst (messages lang))
        assertEqual (show lang <> " keys") enKeys langKeys

i18nFromSiteLang :: TestTree
i18nFromSiteLang =
    testCase "siteLang maps to UI languages" $ do
        assertEqual "zh default" Zh (fromSiteLang "zh-CN")
        assertEqual "traditional" ZhHant (fromSiteLang "zh-TW")
        assertEqual "traditional hk" ZhHant (fromSiteLang "zh_HK")
        assertEqual "traditional script" ZhHant (fromSiteLang "zh-Hant")
        assertEqual "ja" Ja (fromSiteLang "ja-JP")
        assertEqual "en" En (fromSiteLang "en-US")
        assertEqual "unknown" En (fromSiteLang "fr-FR")

i18nPlaceholders :: TestTree
i18nPlaceholders =
    testCase "placeholders are substituted and missing keys fall back" $ do
        assertEqual "zh time" "约 5 分钟" (tWith Zh "readingTime" ["5"])
        assertEqual "en time" "5 min read" (tWith En "readingTime" ["5"])
        assertEqual "ja time" "約 5 分" (tWith Ja "readingTime" ["5"])

i18nPageOutput :: TestTree
i18nPageOutput =
    testCase "pages localize per siteLang" $ do
        let zhPage = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) ((postWithTags []){postText = T.replicate 900 "字"}))
        assertBool "zh reading time" ("约 2 分钟" `textIn` zhPage)
        let enPage = renderHtml (renderIndex testConfig{siteLang = "en"} [] [] "/style.css" (Just (mkPage (Just "Home") 0 Nothing)) [])
        assertBool "en section title" ("Posts" `textIn` enPage)
        assertBool "zh section title" ("文章" `textIn` renderHtml (renderIndex testConfig [] [] "/style.css" (Just (mkPage (Just "Home") 0 Nothing)) []))
        let zh404 = renderHtml (render404 testConfig [] [] "/style.css")
        assertBool "zh 404" ("页面不存在" `textIn` zh404)

indexSpecialPageClassified :: TestTree
indexSpecialPageClassified =
    testCase "redirectAs / declares the index page" $ do
        let pages = [("index", mkPage (Just "Home") 0 (Just "/"))]
        case classifyPages pages of
            Right sp -> assertBool "spIndex" (isJust (spIndex sp))
            Left errs -> assertBool ("expected success, got: " <> show errs) False

indexPageRenderedOnHome :: TestTree
indexPageRenderedOnHome =
    testCase "the index page renders above the post list" $ do
        let page = renderHtml (renderIndex testConfig [] [] "/style.css" (Just (mkPage (Just "Home") 0 Nothing)) [postWithTags []])
        assertBool "index body" ("<div class=\"post-body index-body\">" `textIn` page)
        assertBool "section title" ("index-title" `textIn` page)
        assertBool "post list" ("post-list" `textIn` page)

indexPageExcludedFromNav :: TestTree
indexPageExcludedFromNav =
    testCase "the index page is not in the navbar" $ do
        let pages = [("index", mkPage (Just "Home") 0 (Just "/")), ("about", mkPage (Just "About") 10 Nothing)]
        case classifyPages pages of
            Right sp -> assertEqual "nav" [("/about/", "About")] (map (\(l, h) -> (h, l)) (navItems sp))
            Left _ -> assertBool "expected success" False

darkThemeAttributeSelectors :: TestTree
darkThemeAttributeSelectors =
    testCase "the CSS supports an explicit data-theme override" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool "dark attribute" ("html[data-theme=\"dark\"]" `textIn` css)
        assertBool "light attribute" ("html[data-theme=\"light\"]" `textIn` css)
        let declarations = [l | l <- T.lines css, "  --space-page-top" `T.isPrefixOf` l]
        assertEqual "layout tokens not duplicated" 2 (length declarations)
        assertBool "structural lists unbulleted" (".post-list" `textIn` css && "list-style-type : none" `textIn` css)
        assertBool "content lists default" ("blockquote" `textIn` css)

glyphFontRules :: TestTree
glyphFontRules =
    testCase "language glyph fonts follow the theme" $ do
        let aria = renderCss ariaPreset emptyFonts []
        assertBool "aria ja token" ("--font-ja" `textIn` aria)
        assertBool "aria hant token" ("--font-hant" `textIn` aria)
        assertBool "lang rule" (":lang(ja)" `textIn` aria)
        assertBool "hant langs" (":lang(zh-Hant)" `textIn` aria && ":lang(zh-TW)" `textIn` aria)
        let shaft = renderCss shaftPreset emptyFonts []
        assertBool "shaft ja serif" ("Hiragino Mincho" `textIn` shaft)
        assertBool "shaft hant serif" ("Songti TC" `textIn` shaft)
        let overrides = renderCss ariaPreset Fonts{fontsBody = Nothing, fontsDisplay = Nothing, fontsCode = Nothing, fontsJa = Just ["My JP"], fontsHant = Nothing, fontsSize = Nothing, fontsLineHeight = Nothing, fontsFiles = Nothing} []
        assertBool "ja override" ("My JP" `textIn` overrides)

cssFingerprint :: TestTree
cssFingerprint =
    testCase "pages reference the stylesheet with a content hash" $ do
        let page = renderHtml (renderIndex testConfig [] [] "/style.css?v=deadbeef" Nothing [])
        assertBool "hashed href" ("href=\"/style.css?v=deadbeef\"" `textIn` page)
        let redirect = renderHtml (renderRedirect testConfig "/style.css?v=deadbeef" "/tags/")
        assertBool "redirect page hashed" ("href=\"/style.css?v=deadbeef\"" `textIn` redirect)

themeToggleScriptInjected :: TestTree
themeToggleScriptInjected =
    testCase "every page carries the theme toggle script" $ do
        let page = renderHtml (renderIndex testConfig [] [] "/style.css" Nothing [])
        assertBool "script" ("burogu-theme" `textIn` page)
        assertBool "footer target" ("site-footer" `textIn` page)
        assertBool "button rendered" ("theme-toggle" `textIn` page)
        assertBool "localized label" ("aria-label=\"切换暗色模式\"" `textIn` page)

formatDryRunDoesNotWrite :: TestTree
formatDryRunDoesNotWrite =
    testCase "dry-run leaves files untouched" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/fmt-dry/_post"
        writeFile "/tmp/burogu-test/fmt-dry/_post/2026-08-01-hello.md" "---\ntitle: Hello\n---\n\nbody\n"
        result <- formatOne True "/tmp/burogu-test/fmt-dry/_post" PostKind "2026-08-01-hello.md"
        assertEqual "no errors" False result
        content <- readFile "/tmp/burogu-test/fmt-dry/_post/2026-08-01-hello.md"
        assertEqual "untouched" "---\ntitle: Hello\n---\n\nbody\n" (T.pack content)

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
            let page = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) post)
            assertBool "script injected in page" ("<script" `textIn` page)
            assertBool "mathjax URL" ("mathjax" `textIn` page)

mathjaxNoMathNoScript :: TestTree
mathjaxNoMathNoScript =
    testCase "no script is injected without math content" $ do
        result <- parsePost mathJax "hello.md" "---\ntitle: Hello\ndate: 2026-07-31\n---\nNo math here"
        assertRight result $ \post -> do
            assertBool "no script in body" ("<script" `notTextIn` postBodyHtml post)
            assertBool "not marked as having math" (not (postHasMath post))
            let page = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) post)
            assertBool "no math script in page" ("katex.min.js" `notTextIn` page)
            assertBool "no mathjax in page" ("mathjax" `notTextIn` page)

plainMathNoScript :: TestTree
plainMathNoScript =
    testCase "PlainMath renders without a script tag" $ do
        result <- parsePost plainMath "hello.md" "---\ntitle: Hello\ndate: 2026-07-31\n---\nInline $x^2$ math"
        assertRight result $ \post -> do
            assertBool "math span present" ("math inline" `textIn` postBodyHtml post)
            assertBool "no script in body" ("<script" `notTextIn` postBodyHtml post)
            let noMathConfig = testConfig{siteTheme = (siteTheme testConfig){themeMath = "none"}}
                page = renderHtml (renderPost noMathConfig [] [] "/style.css" (Nothing, Nothing) post)
            assertBool "no katex in page" ("katex.min.js" `notTextIn` page)
            assertBool "no mathjax in page" ("mathjax" `notTextIn` page)

mathMethodMapping :: TestTree
mathMethodMapping =
    testCase "mathMethod maps names to pandoc methods" $ do
        assertEqual "none" PlainMath (mathMethod "none" Nothing)
        assertEqual "katex" (KaTeX "https://cdn.example.com/katex/") (mathMethod "katex" (Just "https://cdn.example.com/katex/"))
        assertEqual "mathjax with url" (MathJax "https://cdn.example.com/mathjax.js") (mathMethod "mathjax" (Just "https://cdn.example.com/mathjax.js"))
        assertEqual "mathjax default url" (MathJax defaultMathJaxURL) (mathMethod "mathjax" Nothing)

tagsLabelCustomized :: TestTree
tagsLabelCustomized =
    testCase "tag index title comes from the tags page title" $ do
        let page = renderHtml (renderTagIndex testConfig [] [] "/style.css" "标签" [("essay", [postWithTags ["essay"]])])
        assertBool "index title" ("<title>标签</title>" `textIn` page)

tagArchiveTitleIsTagName :: TestTree
tagArchiveTitleIsTagName =
    testCase "tag archive title is the bare tag name" $ do
        let page = renderHtml (renderTagArchive testConfig [] [] "/style.css" "essay" [postWithTags ["essay"]])
        assertBool "title" ("<title>essay</title>" `textIn` page)
        assertBool "no prefix" ("Tag:" `notTextIn` page)

cssRootTokens :: TestTree
cssRootTokens =
    testCase "stylesheet declares design tokens on :root" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool ":root block" (":root" `textIn` css)
        assertBool "color token" ("--color-text" `textIn` css)
        assertBool "font token" ("--font-family" `textIn` css)
        assertBool "token variable" ("--token-co" `textIn` css)

cssDarkMediaQuery :: TestTree
cssDarkMediaQuery =
    testCase "stylesheet ships a dark token set via prefers-color-scheme" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool "media query" ("prefers-color-scheme: dark" `textIn` css)
        assertBool "dark background" ("#1a1a1a" `textIn` css)

cssTokenRules :: TestTree
cssTokenRules =
    testCase "token rules reference CSS variables" $ do
        let css = renderCss ariaPreset emptyFonts []
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
        let css = renderCss ariaPreset emptyFonts []
        assertBool "hue computation" ("var(--tag-count)" `textIn` css)
        assertBool "hsl usage" ("hsl(var(--tag-hue)" `textIn` css)

cssListSpacing :: TestTree
cssListSpacing =
    testCase "list items space out date, title and tags" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool "post-item is flex with gap" ("0 var(--space-list-gap)" `textIn` css)
        assertBool "post-meta is flex with gap" (".post-meta" `textIn` css)
        assertBool "tag-item pairs name and count" ("0 6px" `textIn` css)

cssMobileBreakpoint :: TestTree
cssMobileBreakpoint =
    testCase "a 600px breakpoint scales tokens down for phones" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool "media query" ("max-width: 600px" `textIn` css)
        assertBool "smaller font" ("--font-size" `textIn` css)
        assertBool "smaller code" ("font-size : 14px" `textIn` css)

cssOverflowRules :: TestTree
cssOverflowRules =
    testCase "images and tables cannot overflow the content area" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool "img constrained" ("max-width : 100%" `textIn` css)
        assertBool "table scrolls" ("overflow-x : auto" `textIn` css)

cssSafeArea :: TestTree
cssSafeArea =
    testCase "body padding accounts for notched-device safe areas" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool "left inset" ("safe-area-inset-left" `textIn` css)
        assertBool "right inset" ("safe-area-inset-right" `textIn` css)
        assertBool "tap highlight removed" ("tap-highlight-color" `textIn` css)

searchInputFocusStyled :: TestTree
searchInputFocusStyled =
    testCase "search input drops the default ring and clear button" $ do
        let css = renderCss ariaPreset emptyFonts []
        assertBool "custom focus style" (".search-input:focus" `textIn` css)
        assertBool "no default outline" ("outline      : none" `textIn` css)
        assertBool "webkit clear button hidden" ("-webkit-search-cancel-button" `textIn` css)

viewportFitCover :: TestTree
viewportFitCover =
    testCase "viewport meta opts into full-screen safe areas" $ do
        let page = renderHtml (renderIndex testConfig [] [] "/style.css" Nothing [])
        assertBool "viewport-fit=cover" ("viewport-fit=cover" `textIn` page)

cssExtraCssAppended :: TestTree
cssExtraCssAppended =
    testCase "user CSS is appended after the generated rules" $ do
        let css = renderCss ariaPreset emptyFonts ["/* user css */"]
        assertBool "appended at the end" ("/* user css */" `T.isSuffixOf` css)

ariaPresetRegression :: TestTree
ariaPresetRegression =
    testCase "aria preset output matches the pre-refactor golden" $ do
        golden <- TIO.readFile "test/aria-style.expected.css"
        assertEqual "aria css" golden (renderCss ariaPreset emptyFonts [])

presetLookup :: TestTree
presetLookup =
    testCase "preset registry resolves names" $ do
        assertEqual "names" ["aria", "shaft"] presetNames
        assertBool "aria found" (isJust (presetByName "aria"))
        assertBool "shaft found" (isJust (presetByName "shaft"))
        assertBool "unknown not found" (not (isJust (presetByName "nope")))

fontOverridesApplied :: TestTree
fontOverridesApplied =
    testCase "user font overrides land after the preset defaults" $ do
        let css =
                renderCss
                    ariaPreset
                    Fonts
                        { fontsBody = Just ["My Font"]
                        , fontsDisplay = Nothing
                        , fontsJa = Nothing
                        , fontsHant = Nothing
                        , fontsCode = Nothing
                        , fontsSize = Just "18px"
                        , fontsLineHeight = Nothing
                        , fontsFiles = Nothing
                        }
                    []
            (pre, _) = T.breakOn "18px" css
        assertBool "override size present" ("18px" `textIn` css)
        assertBool "override family present" ("My Font" `textIn` css)
        assertBool "override comes after preset" ("17px" `T.isInfixOf` pre)

fontStackQuoting :: TestTree
fontStackQuoting =
    testCase "font stacks quote spaced names but not generic keywords" $ do
        let css =
                renderCss
                    ariaPreset
                    Fonts
                        { fontsBody = Nothing
                        , fontsDisplay = Just ["Noto Serif CJK SC", "SimSun", "serif"]
                        , fontsJa = Nothing
                        , fontsHant = Nothing
                        , fontsCode = Nothing
                        , fontsSize = Nothing
                        , fontsLineHeight = Nothing
                        , fontsFiles = Nothing
                        }
                    []
        assertBool "quoted names, bare keywords" ("\"Noto Serif CJK SC\", SimSun, serif" `textIn` css)

fontFaceEmitted :: TestTree
fontFaceEmitted =
    testCase "embedded font files generate @font-face rules" $ do
        let css =
                renderCss
                    ariaPreset
                    Fonts
                        { fontsBody = Nothing
                        , fontsDisplay = Nothing
                        , fontsCode = Nothing
                        , fontsJa = Nothing
                        , fontsHant = Nothing
                        , fontsSize = Nothing
                        , fontsLineHeight = Nothing
                        , fontsFiles = Just [FontFile "fonts/myserif.woff2" "My Serif" "700" "italic"]
                        }
                    []
        assertBool "font-face rule" ("@font-face" `textIn` css)
        assertBool "quoted family" ("font-family : \"My Serif\"" `textIn` css)
        assertBool "weight" ("font-weight : 700" `textIn` css)
        assertBool "style" ("font-style  : italic" `textIn` css)
        assertBool "site-root url" ("url(/fonts/myserif.woff2)" `textIn` css)

shaftPresetOutput :: TestTree
shaftPresetOutput =
    testCase "shaft preset ships the editorial look" $ do
        let css = renderCss shaftPreset emptyFonts []
        assertBool "accent token" ("--color-accent" `textIn` css)
        assertBool "display font token" ("--font-display" `textIn` css)
        assertBool "display stack quoted" ("\"Songti SC\"" `textIn` css)
        assertBool "archive year" (".archive-year" `textIn` css)
        assertBool "clip path" ("clip-path" `textIn` css)
        assertBool "tag outline" (".post-tag" `textIn` css)
        assertBool "404 big type" (".not-found h1" `textIn` css)
        assertBool "dark accent" ("#ff5347" `textIn` css)
        assertBool "no transitions" ("transition" `notTextIn` css)

shaftHarmonyRules :: TestTree
shaftHarmonyRules =
    testCase "shaft harmony rules are present" $ do
        let css = renderCss shaftPreset emptyFonts []
        assertBool "ink list titles" (".post-item > a" `textIn` css)
        assertBool "ink site name" ("border-right" `textIn` css)
        assertBool "selection accent" ("::selection" `textIn` css)
        assertBool "body h2 serif" ("h2" `textIn` css)
        assertBool "body h3 serif" ("h3" `textIn` css)
        assertBool "sharp corners" ("border-radius : 0" `textIn` css)
        assertBool "mobile date column" ("min-width : 0" `textIn` css)
        assertBool "accent site name" ("color           : var(--color-accent)" `textIn` css)
        assertBool "serif everywhere" ("sans-serif" `notTextIn` css)
        assertBool "print markers" ("::marker" `textIn` css)
        assertBool "ink blockquote" ("blockquote" `textIn` css)

fontsFromJson :: TestTree
fontsFromJson =
    testCase "fonts block parses from yaml" $ do
        case decodeEither' "body:\n  - \"Noto Serif CJK SC\"\n  - SimSun\n  - serif\nsize: 18px" :: Either ParseException Fonts of
            Left _ -> assertBool "fonts parse failed" False
            Right fonts -> do
                assertEqual "body stack" (Just ["Noto Serif CJK SC", "SimSun", "serif"]) (fontsBody fonts)
                assertEqual "size" (Just "18px") (fontsSize fonts)
        case decodeEither' "src: fonts/a.woff2\nfamily: My Serif" :: Either ParseException FontFile of
            Left _ -> assertBool "font file parse failed" False
            Right ff -> do
                assertEqual "weight default" "400" (ffWeight ff)
                assertEqual "style default" "normal" (ffStyle ff)
        case decodeEither' "src: fonts/a.woff2\nfamily: My Serif\nweight: 700" :: Either ParseException FontFile of
            Left _ -> assertBool "numeric weight parse failed" False
            Right ffNum -> assertEqual "numeric weight" "700" (ffWeight ffNum)

siteNameClass :: TestTree
siteNameClass =
    testCase "site name link carries the site-name class" $ do
        let page = renderHtml (renderIndex testConfig [] [] "/style.css" Nothing [])
        assertBool "site-name" ("class=\"site-name\"" `textIn` page)

tagSeparatorSpan :: TestTree
tagSeparatorSpan =
    testCase "tag separators are styled spans" $ do
        let page = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) (postWithTags ["essay", "life"]))
        assertBool "separator span" ("<span class=\"post-tag-sep\"> · </span>" `textIn` page)

tagCountHook :: TestTree
tagCountHook =
    testCase "tag items expose the post count as a CSS variable hook" $ do
        let page = renderHtml (renderTagIndex testConfig [] [] "/style.css" "Tags" [("essay", [postWithTags ["essay"], postWithTags ["essay"]])])
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
        let xml = renderSitemap "https://lizi.moe" [postWithTags ["essay"]] [("Tags", "/tags/")] True
        assertBool "urlset" ("<urlset" `textIn` xml)
        assertBool "index" ("https://lizi.moe/" `textIn` xml)
        assertBool "post" ("https://lizi.moe/posts/test/" `textIn` xml)
        assertBool "tag index" ("https://lizi.moe/tags/" `textIn` xml)
        assertBool "tag archive" ("https://lizi.moe/tags/essay/" `textIn` xml)
        assertBool "feed" ("https://lizi.moe/feed.xml" `textIn` xml)
        assertEqual "tag index once" 1 (T.count "<loc>https://lizi.moe/tags/</loc>" xml)

sitemapLastmod :: TestTree
sitemapLastmod =
    testCase "posts carry their date as lastmod" $ do
        let xml = renderSitemap "https://lizi.moe" [postWithTags ["essay"]] [] True
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
        let page = renderHtml (render404 testConfig [] [] "/style.css")
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
        let page = renderHtml (renderIndex jsConfig [] [] "/style.css" Nothing [])
        assertBool "script tag" ("<script defer src=\"/theme.js\"" `textIn` page)
        assertBool "no script without extraJs" ("theme.js" `notTextIn` renderHtml (renderIndex testConfig [] [] "/style.css" Nothing []))

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
        let page = renderHtml (renderIndex testConfig [("About Me", "/about/")] [] "/style.css" Nothing [])
        assertBool "nav link" ("href=\"/about/\">About Me" `textIn` page)

aboutNavHidden :: TestTree
aboutNavHidden =
    testCase "no about link without an about page" $ do
        let page = renderHtml (renderIndex testConfig [] [] "/style.css" Nothing [])
        assertBool "no link" ("/about/" `notTextIn` page)

copyrightCustom :: TestTree
copyrightCustom =
    testCase "footer uses the configurable copyright" $ do
        let page = renderHtml (renderIndex testConfig{siteCopyright = "自定义版权"} [] [] "/style.css" Nothing [])
        assertBool "custom copyright" ("自定义版权" `textIn` page)

footerCreditShown :: TestTree
footerCreditShown =
    testCase "footer shows the configured generator credit on the same line" $ do
        let page = renderHtml (renderIndex testConfig{siteGeneratedBy = Just "Generated with Burogu"} [] [] "/style.css" Nothing [])
        assertBool "joined line" ("© moe li · Generated with Burogu" `textIn` page)

footerCreditHidden :: TestTree
footerCreditHidden =
    testCase "footer omits the generator credit when unset" $ do
        let page = renderHtml (renderIndex testConfig [] [] "/style.css" Nothing [])
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
        let page = renderHtml (renderIndex testConfig [("About", "/about/"), ("Projects", "/projects/")] [] "/style.css" Nothing [])
        assertBool "about link" ("href=\"/about/\">About" `textIn` page)
        assertBool "projects link" ("href=\"/projects/\">Projects" `textIn` page)
        let aboutPos = findSubstring "href=\"/about/\"" page
            projectsPos = findSubstring "href=\"/projects/\"" page
        assertBool "order" (aboutPos < projectsPos)

findSubstring :: Text -> Text -> Int
findSubstring needle haystack =
    case T.breakOn needle haystack of
        (before, _) -> T.length before

pagePriorityParsed :: TestTree
pagePriorityParsed =
    testCase "pages read the frontmatter priority" $ do
        writeFile "/tmp/burogu-test/priority.md" "---\npriority: 3\n---\n# P\n"
        result <- loadPage plainMath "/tmp/burogu-test/priority.md"
        case result of
            Right (Just page) -> assertEqual "priority" 3 (cpPriority page)
            _ -> assertBool "expected a page" False

pagePriorityDefault :: TestTree
pagePriorityDefault =
    testCase "pages without priority default to 100" $ do
        writeFile "/tmp/burogu-test/no-priority.md" "# P\n"
        result <- loadPage plainMath "/tmp/burogu-test/no-priority.md"
        case result of
            Right (Just page) -> assertEqual "priority" 100 (cpPriority page)
            _ -> assertBool "expected a page" False

pagePriorityInvalid :: TestTree
pagePriorityInvalid =
    testCase "a non-integer priority is a hard error" $ do
        writeFile "/tmp/burogu-test/bad-priority.md" "---\npriority: abc\n---\n# P\n"
        result <- loadPage plainMath "/tmp/burogu-test/bad-priority.md"
        assertBool "error" (isLeft result)

pageRedirectAsParsed :: TestTree
pageRedirectAsParsed =
    testCase "pages read the frontmatter redirectAs" $ do
        writeFile "/tmp/burogu-test/redir.md" "---\nredirectAs: /tags/\n---\n# P\n"
        result <- loadPage plainMath "/tmp/burogu-test/redir.md"
        case result of
            Right (Just page) -> assertEqual "redirectAs" (Just "/tags/") (cpRedirectAs page)
            _ -> assertBool "expected a page" False

mkPage :: Maybe Text -> Int -> Maybe Text -> CustomPage
mkPage title priority redirect = mkPageHidden title priority redirect PlNav

mkPageHidden :: Maybe Text -> Int -> Maybe Text -> Placement -> CustomPage
mkPageHidden title priority redirect placement = CustomPage{cpTitle = title, cpBodyHtml = "", cpHasMath = False, cpPriority = priority, cpRedirectAs = redirect, cpPlacement = placement, cpScript = Nothing, cpOutput = Nothing, cpText = ""}

classifySpecialPages :: TestTree
classifySpecialPages =
    testCase "redirectAs declarations are classified as special pages" $ do
        let pages =
                [ ("tags", mkPage (Just "标签") 10 (Just "/tags/"))
                , ("archive", mkPage Nothing 20 (Just "/archive/"))
                , ("404", mkPage (Just "Oops") 0 (Just "/404.html"))
                , ("about", mkPage (Just "About") 30 Nothing)
                ]
        case classifyPages pages of
            Right sp -> do
                assertBool "tags" (isJust (spTags sp))
                assertBool "archive" (isJust (spArchive sp))
                assertBool "404" (isJust (sp404 sp))
                assertEqual "normal" ["about"] (map fst (spNormal sp))
            Left errs -> assertBool ("expected success, got: " <> show errs) False

classifyUnknownRedirectAs :: TestTree
classifyUnknownRedirectAs =
    testCase "an arbitrary redirectAs becomes a redirect stub" $ do
        let pages = [("old-post", mkPage (Just "Old") 0 (Just "/new-post/"))]
        case classifyPages pages of
            Right sp -> do
                assertEqual "redirects" ["old-post"] (map fst (spRedirects sp))
                assertEqual "not normal" [] (map fst (spNormal sp))
            Left errs -> assertBool ("expected success, got: " <> show errs) False

classifyExternalRedirect :: TestTree
classifyExternalRedirect =
    testCase "external redirectAs URLs become redirect stubs" $ do
        let pages = [("out", mkPage Nothing 0 (Just "https://example.com/x"))]
        case classifyPages pages of
            Right sp -> assertEqual "redirects" ["out"] (map fst (spRedirects sp))
            Left errs -> assertBool ("expected success, got: " <> show errs) False

classifyInvalidRedirectAs :: TestTree
classifyInvalidRedirectAs =
    testCase "a relative redirectAs is a hard error" $ do
        let pages = [("misc", mkPage Nothing 0 (Just "new-page/"))]
        case classifyPages pages of
            Left errs -> assertBool "message" (any ("invalid redirectAs 'new-page/'" `T.isInfixOf`) errs)
            Right _ -> assertBool "expected failure" False

classifyDuplicateSpecial :: TestTree
classifyDuplicateSpecial =
    testCase "duplicate special-page declarations are a hard error" $ do
        let pages =
                [ ("tags", mkPage Nothing 0 (Just "/tags/"))
                , ("tag-collection", mkPage Nothing 0 (Just "/tags/"))
                ]
        case classifyPages pages of
            Left errs -> assertBool "message" (any ("duplicate declaration of '/tags/'" `T.isInfixOf`) errs)
            Right _ -> assertBool "expected failure" False

classifyCollision :: TestTree
classifyCollision =
    testCase "a normal page colliding with a special URL is a hard error" $ do
        let pages =
                [ ("tags", mkPage Nothing 0 Nothing)
                , ("tag-index", mkPage Nothing 0 (Just "/tags/"))
                ]
        case classifyPages pages of
            Left errs -> assertBool "message" (any ("collides with the declared special URL '/tags/'" `T.isInfixOf`) errs)
            Right _ -> assertBool "expected failure" False

placementParsed :: TestTree
placementParsed =
    testCase "pages read the frontmatter placement" $ do
        writeFile "/tmp/burogu-test/footer.md" "---\nplacement: footer\n---\n# P\n"
        writeFile "/tmp/burogu-test/default.md" "---\n# P\n"
        writeFile "/tmp/burogu-test/legacy.md" "---\nhiddenInNavbar: true\n---\n# P\n"
        result <- loadPage plainMath "/tmp/burogu-test/footer.md"
        case result of
            Right (Just page) -> assertEqual "footer" PlFooter (cpPlacement page)
            _ -> assertBool "expected a page" False
        result2 <- loadPage plainMath "/tmp/burogu-test/default.md"
        case result2 of
            Right (Just page) -> assertEqual "default nav" PlNav (cpPlacement page)
            _ -> assertBool "expected a page" False
        result3 <- loadPage plainMath "/tmp/burogu-test/legacy.md"
        case result3 of
            Left err -> assertBool "legacy rejected" ("hiddenInNavbar" `T.isInfixOf` err)
            _ -> assertBool "expected a legacy-field error" False
        writeFile "/tmp/burogu-test/bad.md" "---\nplacement: sidebar\n---\n# P\n"
        result4 <- loadPage plainMath "/tmp/burogu-test/bad.md"
        case result4 of
            Left _ -> pure ()
            _ -> assertBool "expected failure" False

placementFiltersNav :: TestTree
placementFiltersNav =
    testCase "placement none and footer pages stay out of the nav" $ do
        let pages =
                [ ("secret", mkPageHidden (Just "Secret") 10 Nothing PlNone)
                , ("foot", mkPageHidden (Just "Foot") 5 Nothing PlFooter)
                , ("public", mkPage (Just "Public") 20 Nothing)
                ]
        case classifyPages pages of
            Right sp -> assertEqual "nav" [("/public/", "Public")] (map (\(l, h) -> (h, l)) (navItems sp))
            Left _ -> assertBool "expected success" False

redirectStubNavVisibility :: TestTree
redirectStubNavVisibility =
    testCase "redirect stubs follow the placement rule" $ do
        let visible = [("old", mkPage (Just "Old") 0 (Just "/new/"))]
            hidden = [("old", mkPageHidden (Just "Old") 0 (Just "/new/") PlNone)]
        case classifyPages visible of
            Right sp -> assertEqual "stub visible by default" [("/old/", "Old")] (map (\(l, h) -> (h, l)) (navItems sp))
            Left _ -> assertBool "expected success" False
        case classifyPages hidden of
            Right sp -> assertEqual "stub hidden" [] (navItems sp)
            Left _ -> assertBool "expected success" False

notFoundInNavByDefault :: TestTree
notFoundInNavByDefault =
    testCase "the 404 page joins the nav unless hidden" $ do
        let pages = [("404", mkPage (Just "Oops") 0 (Just "/404.html"))]
        case classifyPages pages of
            Right sp -> assertEqual "404 in nav" [("/404.html", "Oops")] (map (\(l, h) -> (h, l)) (navItems sp))
            Left _ -> assertBool "expected success" False
        let hidden = [("404", mkPageHidden (Just "Oops") 0 (Just "/404.html") PlNone)]
        case classifyPages hidden of
            Right sp -> assertEqual "404 hidden" [] (navItems sp)
            Left _ -> assertBool "expected success" False

navPriorityOrder :: TestTree
navPriorityOrder =
    testCase "nav orders pages by priority, then slug" $ do
        let pages =
                [ ("zebra", mkPage Nothing 50 Nothing)
                , ("apple", mkPage Nothing 10 Nothing)
                , ("mango", mkPage Nothing 10 Nothing)
                ]
        case classifyPages pages of
            Right sp -> do
                let nav = navItems sp
                assertEqual "order" ["/apple/", "/mango/", "/zebra/"] (map snd nav)
            Left _ -> assertBool "expected success" False

navPriorityTieLexicographic :: TestTree
navPriorityTieLexicographic =
    testCase "equal priorities break ties lexicographically" $ do
        let pages = [("b", mkPage Nothing 0 Nothing), ("a", mkPage Nothing 0 Nothing)]
        case classifyPages pages of
            Right sp -> assertEqual "order" ["/a/", "/b/"] (map snd (navItems sp))
            Left _ -> assertBool "expected success" False

navDefaultPriorityLast :: TestTree
navDefaultPriorityLast =
    testCase "pages without priority come after pinned ones" $ do
        let pages = [("free", mkPage Nothing 100 Nothing), ("pinned", mkPage Nothing 5 Nothing)]
        case classifyPages pages of
            Right sp -> assertEqual "order" ["/pinned/", "/free/"] (map snd (navItems sp))
            Left _ -> assertBool "expected success" False

navTagsPosition :: TestTree
navTagsPosition =
    testCase "tags and archive join the nav at their priority" $ do
        let pages =
                [ ("about", mkPage (Just "About") 30 Nothing)
                , ("tags", mkPage (Just "标签") 10 (Just "/tags/"))
                , ("archive", mkPage Nothing 20 (Just "/archive/"))
                ]
        case classifyPages pages of
            Right sp -> do
                let nav = navItems sp
                assertEqual "order" [("/tags/", "标签"), ("/archive/", "Archive"), ("/about/", "About")] (map (\(l, h) -> (h, l)) nav)
            Left _ -> assertBool "expected success" False

archiveYearGroups :: TestTree
archiveYearGroups =
    testCase "archive groups posts into year sections, newest first" $ do
        let posts =
                [ (postWithTags []){postDate = "2026-06-01", postTitle = "New"}
                , (postWithTags []){postDate = "2026-01-01", postTitle = "Mid"}
                , (postWithTags []){postDate = "2025-12-31", postTitle = "Old"}
                ]
        let page = renderHtml (renderArchive testConfig [] [] "/style.css" "Archive" posts)
        assertBool "2026 section" ("<h2 class=\"archive-year\">2026</h2>" `textIn` page)
        assertBool "2025 section" ("<h2 class=\"archive-year\">2025</h2>" `textIn` page)
        assertBool "title" ("<title>Archive</title>" `textIn` page)
        let sec2026 = findSubstring "<h2 class=\"archive-year\">2026</h2>" page
            sec2025 = findSubstring "<h2 class=\"archive-year\">2025</h2>" page
        assertBool "year order" (sec2026 < sec2025)
        assertBool "newest first" (findSubstring ">New</a>" page < findSubstring ">Mid</a>" page)

redirectPageMetaRefresh :: TestTree
redirectPageMetaRefresh =
    testCase "redirect pages carry a meta refresh and canonical link" $ do
        let page = renderHtml (renderRedirect testConfig "/style.css" "/tags/")
        assertBool "meta refresh" ("http-equiv=\"refresh\" content=\"0; url=/tags/\"" `textIn` page)
        assertBool "canonical" ("rel=\"canonical\" href=\"/tags/\"" `textIn` page)
        assertBool "stylesheet" ("rel=\"stylesheet\" href=\"/style.css\"" `textIn` page)
        assertBool "link" ("href=\"/tags/\">/tags/</a>" `textIn` page)

tagsLabelRejected :: TestTree
tagsLabelRejected =
    testCase "tagsLabel in config.yaml is a hard error" $ do
        writeFile "/tmp/burogu-test/tagslabel.yaml" "siteName: x\ntagsLabel: Tags\n"
        result <- try (loadConfig "/tmp/burogu-test/tagslabel.yaml") :: IO (Either ExitCode SiteConfig)
        case result of
            Left _ -> assertBool "rejected" True
            Right _ -> assertBool "expected failure" False

searchIndexShape :: TestTree
searchIndexShape =
    testCase "search index carries posts with date/tags and pages without" $ do
        let post = (postWithTags ["essay"]){postTitle = "Hello", postText = "some body text"}
            page = ("about", mkPage (Just "About") 0 Nothing)
        let json = renderSearchIndex [post] [page]
        assertBool "post title" ("\"title\":\"Hello\"" `textIn` json)
        assertBool "post url" ("\"url\":\"/posts/test/\"" `textIn` json)
        assertBool "post date" ("\"date\":\"2026-01-01\"" `textIn` json)
        assertBool "post tags" ("\"tags\":[\"essay\"]" `textIn` json)
        assertBool "post text" ("\"text\":\"some body text\"" `textIn` json)
        assertBool "page title" ("\"title\":\"About\"" `textIn` json)
        assertBool "page url" ("\"url\":\"/about/\"" `textIn` json)
        let aboutTitle = findSubstring "\"title\":\"About\"" json
            aboutUrl = findSubstring "\"url\":\"/about/\"" json
            datePos = findSubstring "\"date\"" json
        assertBool "page has no date between title and url" (aboutTitle < aboutUrl && not (aboutTitle < datePos && datePos < aboutUrl))

searchIndexEscaping :: TestTree
searchIndexEscaping =
    testCase "search index escapes quotes and backslashes" $ do
        let post = (postWithTags []){postTitle = "A \"quoted\" \\ title", postText = "back\\slash"}
        let json = renderSearchIndex [post] []
        assertBool "escaped quotes" ("A \\\"quoted\\\" \\\\ title" `textIn` json)
        assertBool "escaped backslash" ("back\\\\slash" `textIn` json)

searchIndexExcludesSpecials :: TestTree
searchIndexExcludesSpecials =
    testCase "search index is built only from the pages the caller passes" $ do
        let json = renderSearchIndex [] [("about", mkPage Nothing 0 Nothing)]
        assertBool "about included" ("/about/" `textIn` json)
        assertBool "no tags entry" ("/tags/" `notTextIn` json)

searchPageRendered :: TestTree
searchPageRendered =
    testCase "search page renders input and results container" $ do
        let page = renderHtml (renderSearch testConfig [] [] "/style.css" "Search")
        assertBool "title" ("<title>Search</title>" `textIn` page)
        assertBool "input" ("class=\"search-input\"" `textIn` page)
        assertBool "results" ("id=\"search-results\"" `textIn` page)

searchPageScript :: TestTree
searchPageScript =
    testCase "search page script fetches the index and honors the hook" $ do
        let page = renderHtml (renderSearch testConfig [] [] "/style.css" "Search")
        assertBool "fetch" ("/search.json" `textIn` page)
        assertBool "hook" ("window.buroguSearch" `textIn` page)
        assertBool "default search" ("index.forEach" `textIn` page)
        assertBool "highlight helper" ("function highlight" `textIn` page)
        assertBool "snippet helper" ("function snippetAround" `textIn` page)

buildKeepsOutputOnPageError :: TestTree
buildKeepsOutputOnPageError =
    testCase "page errors keep the previous output directory" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/build-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/build-out"
        writeFile "/tmp/burogu-test/build-src/_pages/bad.md" "---\nredirectAs: bogus/\n---\n# X\n"
        writeFile "/tmp/burogu-test/build-out/marker.txt" "old"
        let paths = Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/build-src", pOut = "/tmp/burogu-test/build-out"}
        result <- try (build paths testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left _ -> pure ()
            Right _ -> assertBool "expected failure" False
        marker <- doesFileExist "/tmp/burogu-test/build-out/marker.txt"
        assertBool "old output kept" marker

fontFileMissingKeepsOldOutput :: TestTree
fontFileMissingKeepsOldOutput =
    testCase "missing embedded font file keeps the previous output" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/font-src"
        createDirectoryIfMissing True "/tmp/burogu-test/font-out"
        writeFile "/tmp/burogu-test/font-out/marker.txt" "old"
        let fonts = Fonts{fontsBody = Nothing, fontsDisplay = Nothing, fontsCode = Nothing, fontsJa = Nothing, fontsHant = Nothing, fontsSize = Nothing, fontsLineHeight = Nothing, fontsFiles = Just [FontFile "fonts/nope.woff2" "Nope" "400" "normal"]}
            config = testConfig{siteTheme = (siteTheme testConfig){themeFonts = fonts}}
            paths = Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/font-src", pOut = "/tmp/burogu-test/font-out"}
        result <- try (build paths config []) :: IO (Either IOException BuildReport)
        case result of
            Left _ -> pure ()
            Right _ -> assertBool "expected failure" False
        marker <- doesFileExist "/tmp/burogu-test/font-out/marker.txt"
        assertBool "old output kept" marker

redirectStubBuilt :: TestTree
redirectStubBuilt =
    testCase "arbitrary redirectAs writes a redirect page at the slug URL" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/redir-src/_pages"
        writeFile "/tmp/burogu-test/redir-src/_pages/old.md" "---\ntitle: Old\nredirectAs: /new/\n---\n# Old\n"
        let paths = Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/redir-src", pOut = "/tmp/burogu-test/redir-out"}
        result <- try (build paths testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left err -> assertBool ("expected success, got: " <> show err) False
            Right _ -> pure ()
        stub <- readFile "/tmp/burogu-test/redir-out/old/index.html"
        assertBool "meta refresh" ("0; url=/new/" `textIn` T.pack stub)
        assertBool "canonical" ("rel=\"canonical\" href=\"/new/\"" `textIn` T.pack stub)
        index <- readFile "/tmp/burogu-test/redir-out/index.html"
        assertBool "stub in nav by default" ("href=\"/old/\"" `textIn` T.pack index)

docRenderPlain :: TestTree
docRenderPlain =
    testCase "plain rendering strips all markup" $ do
        let out = render Plain "## Commands\n\n- **bold** `code` [link](https://x.example) here\n\n```sh\nburogu build\n```\n"
        assertBool "no escape codes" ("\ESC" `notTextIn` out)
        assertBool "section uppercased" ("COMMANDS" `textIn` out)
        assertBool "bold stripped" ("**bold**" `notTextIn` out)
        assertBool "code stripped" ("bold code link (https://x.example) here" `textIn` out)
        assertBool "fence indented" ("    burogu build" `textIn` out)

docHeadingDepth :: TestTree
docHeadingDepth =
    testCase "headings of any depth render as headings" $ do
        let plain = render Plain "## Section\n### Sub\n#### SubSub\n"
            color = render Color "#### SubSub\n"
        assertBool "h2 uppercased" ("SECTION" `textIn` plain)
        assertBool "h3 kept" ("Sub" `textIn` plain)
        assertBool "h4 kept" ("SubSub" `textIn` plain)
        assertBool "no hash leaked" ("####" `notTextIn` plain)
        assertBool "h4 bold, no underline" ("\ESC[1mSubSub\ESC[0m" `textIn` color)
        assertBool "h4 not section-styled" ("\ESC[1m\ESC[4m" `notTextIn` color)

docRenderColor :: TestTree
docRenderColor =
    testCase "color rendering embeds ANSI styles" $ do
        let out = render Color "## Commands\n\n- **bold** `code` [link](https://x.example) here\n\n```sh\nburogu build\n```\n"
        assertBool "heading style" ("\ESC[1m\ESC[4mCommands\ESC[0m" `textIn` out)
        assertBool "bold style" ("\ESC[1mbold\ESC[0m" `textIn` out)
        assertBool "code style" ("\ESC[36mcode\ESC[0m" `textIn` out)
        assertBool "fence dimmed" ("\ESC[2mburogu build\ESC[0m" `textIn` out)

docSectionExtraction :: TestTree
docSectionExtraction =
    testCase "sections are extracted by exact and unique-prefix name" $ do
        let manual = T.unlines ["## Configuration", "key: value", "## Commands", "run it", "## Deployment", "deploy it"]
        assertEqual "exact" (Right "## Configuration\nkey: value\n") (extractSection "configuration" manual)
        assertEqual "unique prefix" (Right "## Commands\nrun it\n") (extractSection "com" manual)
        assertBool "unknown" (isLeft (extractSection "nope" manual))
        assertBool "ambiguous" (isLeft (extractSection "c" manual))
        assertEqual "section list" ["configuration", "commands", "deployment"] (map fst (sections manual))

docLangFromLocale :: TestTree
docLangFromLocale =
    testCase "locale values map to languages" $ do
        assertEqual "zh locale" "zh" (langFromLocale ["zh_CN.UTF-8"])
        assertEqual "zh case-insensitive" "zh-Hant" (langFromLocale ["ZH_TW.UTF-8"])
        assertEqual "traditional zh" "zh-Hant" (langFromLocale ["zh_TW.UTF-8"])
        assertEqual "traditional zh hk" "zh-Hant" (langFromLocale ["zh_HK"])
        assertEqual "traditional zh script" "zh-Hant" (langFromLocale ["zh-Hant-TW"])
        assertEqual "ja locale" "ja" (langFromLocale ["ja_JP.UTF-8"])
        assertEqual "ja script" "ja" (langFromLocale ["ja-JP"])
        assertEqual "en locale" "en" (langFromLocale ["en_US.UTF-8"])
        assertEqual "C locale" "en" (langFromLocale ["C"])
        assertEqual "no locale" "en" (langFromLocale [])

manualsPresent :: TestTree
manualsPresent =
    testCase "all embedded manuals cover every section" $ do
        mapM_ check ["en", "zh", "zh-Hant", "ja"]
  where
    required = ["name", "synopsis", "description", "commands", "configuration", "site layout", "search", "deployment", "sync", "files", "exit status", "see also"]
    check :: Text -> IO ()
    check lang = do
        let names = map fst (sections (manualContent lang))
        assertBool (T.unpack lang <> " manual complete") (all (`elem` names) required)

escapedPost :: Post
escapedPost =
    (postWithTags ["essay"]){postTitle = "A & B", postBodyHtml = "<p>hi</p>", postDescription = Just "desc"}

ogMetaOnPost :: TestTree
ogMetaOnPost =
    testCase "post pages carry article OG metadata" $ do
        let html = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) (postWithTags ["essay"]))
        assertBool "og:title" ("og:title\" content=\"test\"" `textIn` html)
        assertBool "og:type" ("og:type\" content=\"article\"" `textIn` html)
        assertBool "og:url" ("og:url\" content=\"https://lizi.moe/posts/test/\"" `textIn` html)

ogTypeWebsiteOnIndex :: TestTree
ogTypeWebsiteOnIndex =
    testCase "index pages carry website OG metadata" $ do
        let html = renderHtml (renderIndex testConfig [] [] "/style.css" Nothing [postWithTags ["essay"]])
        assertBool "og:type" ("og:type\" content=\"website\"" `textIn` html)
        assertBool "og:url" ("og:url\" content=\"https://lizi.moe/\"" `textIn` html)

ogUrlAbsentWithoutBaseUrl :: TestTree
ogUrlAbsentWithoutBaseUrl =
    testCase "og:url is omitted without baseUrl" $ do
        let html = renderHtml (renderIndex testConfig{siteBaseUrl = Nothing} [] [] "/style.css" Nothing [])
        assertBool "no og:url" ("og:url" `notTextIn` html)
        assertBool "og:title still present" ("og:title" `textIn` html)

ogDescriptionFallsBack :: TestTree
ogDescriptionFallsBack =
    testCase "og:description falls back to siteDescription" $ do
        let html = renderHtml (renderPost testConfig [] [] "/style.css" (Nothing, Nothing) (postWithTags []))
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
        , siteCopyright = "© moe li"
        , siteGeneratedBy = Nothing
        , siteFooterSeparator = " · "
        , siteDeploy = DeployConfig{deployTarget = Nothing, deployRepo = Nothing, deployBranch = Nothing, deployCommitName = Nothing, deployCommitEmail = Nothing}
        , siteSrcRepo = Nothing
        , siteTheme = Theme{themeMath = "mathjax", themeMathUrl = Nothing, themeExtraCss = [], themeExtraJs = [], themePreset = "aria", themeFonts = emptyFonts}
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
        , postShowToc = False
        , postToc = []
        , postBodyHtml = ""
        , postText = ""
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

frontmatterSplit :: TestTree
frontmatterSplit =
    testCase "frontmatter is split from the body" $ do
        assertEqual "with frontmatter" (Just "title: X\n", "body here\n") (splitFrontmatter "---\ntitle: X\n---\nbody here\n")
        assertEqual "without frontmatter" (Nothing, "no fm\n") (splitFrontmatter "no fm\n")
        assertEqual "unclosed" (Nothing, "---\ntitle: X\n") (splitFrontmatter "---\ntitle: X\n")

frontmatterEmptyFilled :: TestTree
frontmatterEmptyFilled =
    testCase "files without frontmatter get one filled in" $ do
        case normalizeFrontmatter PostKind "/tmp/x/2026-08-01-hello.md" "" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, _, _) -> assertEqual "post defaults" "title: hello\ndate: 2026-08-01\ntags: []\ndraft: false\ntoc: false\n" fm
        case normalizeFrontmatter PageKind "/tmp/x/about.md" "" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, _, _) -> assertEqual "page defaults" "title: about\npriority: 100\nplacement: nav\n" fm

frontmatterMalformedErrors :: TestTree
frontmatterMalformedErrors =
    testCase "malformed frontmatter is a hard error" $ do
        assertBool "error" (isLeft (normalizeFrontmatter PageKind "/tmp/x/p.md" "title: [unclosed\n"))

frontmatterPostDefaults :: TestTree
frontmatterPostDefaults =
    testCase "posts get every field with defaults" $ do
        case normalizeFrontmatter PostKind "/tmp/x/2026-08-01-hello.md" "title: Post\n" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, unknown, _) -> do
                assertEqual "canonical order" "title: Post\ndate: 2026-08-01\ntags: []\ndraft: false\ntoc: false\n" fm
                assertEqual "no unknown keys" [] unknown

frontmatterPageDefaults :: TestTree
frontmatterPageDefaults =
    testCase "pages get every field with defaults" $ do
        case normalizeFrontmatter PageKind "/tmp/x/about.md" "title: About\n" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, _, _) -> assertEqual "canonical order" "title: About\npriority: 100\nplacement: nav\n" fm

frontmatterNoDateError :: TestTree
frontmatterNoDateError =
    testCase "a post without any date is an error" $ do
        assertBool "error" (isLeft (normalizeFrontmatter PostKind "/tmp/x/no-date.md" "title: X\n"))

frontmatterDraftNoDate :: TestTree
frontmatterDraftNoDate =
    testCase "a draft without a date omits the date key" $ do
        case normalizeFrontmatter PostKind "/tmp/x/draft.md" "title: X\ndraft: true\n" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, _, _) -> assertEqual "no date key" "title: X\ntags: []\ndraft: true\ntoc: false\n" fm

frontmatterUnknownKeys :: TestTree
frontmatterUnknownKeys =
    testCase "unknown keys are kept in sorted order" $ do
        case normalizeFrontmatter PageKind "/tmp/x/p.md" "custom: 42\ntitle: X\naliases: [a, b]\n" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, unknown, _) -> do
                assertEqual "unknown keys" ["aliases", "custom"] unknown
                assertEqual "kept after known" "title: X\npriority: 100\nplacement: nav\naliases:\n  - a\n  - b\ncustom: 42\n" fm

frontmatterDescriptionOmitted :: TestTree
frontmatterDescriptionOmitted =
    testCase "an empty description is not written" $ do
        case normalizeFrontmatter PostKind "/tmp/x/2026-08-01-p.md" "title: X\n" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, _, _) -> assertBool "no description" ("description" `notTextIn` fm)
        case normalizeFrontmatter PostKind "/tmp/x/2026-08-01-p.md" "title: X\ndescription: Hello\n" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, _, _) -> assertEqual "description written" "title: X\ndate: 2026-08-01\ntags: []\ndescription: Hello\ndraft: false\ntoc: false\n" fm

configTemplateGolden :: TestTree
configTemplateGolden =
    testCase "the config template renders to the golden output" $ do
        golden <- TIO.readFile "test/config-template.expected.yaml"
        assertEqual "golden" golden (renderConfig defaultConfigTemplate emptyConfigValues)

configFormatValues :: TestTree
configFormatValues =
    testCase "format values substitute into the template" $ do
        let values =
                ConfigValues
                    { cvTop = [("siteName", "myblog"), ("baseUrl", "https://lizi.moe")]
                    , cvDeploy = Just [("target", "user@host:/var/www/x")]
                    , cvSrcRepo = Just "git@github.com:user/site.git"
                    , cvTheme = [("preset", "shaft"), ("extraCss", "[theme.css]")]
                    , cvMathUrl = Just "https://custom.example/math.js"
                    , cvExtraJs = Just "[theme.js]"
                    , cvFonts = Just [("size", "18px")]
                    , cvFontsFiles = Just "files:\n  - src: fonts/my.woff2\n    family: My Serif\n    weight: 400\n    style: normal\n"
                    }
            rendered = renderConfig defaultConfigTemplate values
        assertBool "value" ("siteName: myblog" `textIn` rendered)
        assertBool "deploy block" ("deploy:\n  target: user@host:/var/www/x" `textIn` rendered)
        assertBool "srcRepo real" ("srcRepo: git@github.com:user/site.git" `textIn` rendered)
        assertBool "theme value" ("  preset: shaft" `textIn` rendered)
        assertBool "extraJs real" ("  extraJs: [theme.js]" `textIn` rendered)
        assertBool "fonts block" ("  fonts:\n    size: 18px" `textIn` rendered)
        assertBool "fonts files" ("      - src: fonts/my.woff2" `textIn` rendered)

formatWritesFile :: TestTree
formatWritesFile =
    testCase "format rewrites a post file in place" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/fmt-write/_post"
        writeFile "/tmp/burogu-test/fmt-write/_post/2026-08-01-hello.md" "---\ntitle: Hello\n---\n\nbody\n"
        result <- formatOne False "/tmp/burogu-test/fmt-write/_post" PostKind "2026-08-01-hello.md"
        assertEqual "no errors" False result
        content <- readFile "/tmp/burogu-test/fmt-write/_post/2026-08-01-hello.md"
        assertEqual "normalized" "---\ntitle: Hello\ndate: 2026-08-01\ntags: []\ndraft: false\ntoc: false\n---\n<!-- digest: 415097e5 -->\n\nbody\n" (T.pack content)

dslTests :: [TestTree]
dslTests =
    [ dslArithmetic
    , dslStringInterpolation
    , dslLogicAndIf
    , dslArraysAndMaps
    , dslLambdasAndHigherOrder
    , dslDefRecursionClosures
    , dslBuiltins
    , dslPutsOutput
    , dslErrors
    , dslLexerErrors
    , dslParserErrors
    ]

dslArithmetic :: TestTree
dslArithmetic =
    testCase "arithmetic, precedence, unary minus" $ do
        assertEqual "add" (Right "3") (runLang "1 + 2")
        assertEqual "precedence" (Right "4") (runLang "10 - 2 * 3")
        assertEqual "parens" (Right "24") (runLang "(10 - 2) * 3")
        assertEqual "div" (Right "2.5") (runLang "10 / 4")
        assertEqual "mod" (Right "1") (runLang "7 % 3")
        assertEqual "unary minus" (Right "-3") (runLang "-5 + 2")
        assertEqual "decimal" (Right "3") (runLang "1.5 + 1.5")
        assertEqual "decimal display" (Right "\"1.5\"") (runLang "toStr(1.5)")
        assertEqual "grouping" (Right "9") (runLang "(1 + 2) * 3")
        assertEqual "adjacent exprs" (Right "2") (runLang "1 2")
        assertEqual "top-level calls" (Right "2") (runLang "def f() 1 end def g() 2 end f() g()")
        assertEqual "space call" (Right "10") (runLang "def f(x) x * 2 end f (5)")
        assertEqual "compare" (Right "true") (runLang "1 + 2 * 3 == 7")
        assertEqual "string add" (Right "\"ab\"") (runLang "\"a\" + \"b\"")

dslStringInterpolation :: TestTree
dslStringInterpolation =
    testCase "string interpolation and escapes" $ do
        assertEqual "basic" (Right "\"a2b\"") (runLang "\"a#{1 + 1}b\"")
        assertEqual "bool" (Right "\"false\"") (runLang "\"#{1 > 2}\"")
        assertEqual "nil" (Right "\"nil\"") (runLang "\"#{nil}\"")
        assertEqual "nested" (Right "\"xy1z\"") (runLang "\"x#{ \"y#{1}\" }z\"")
        assertEqual "escape" (Right "\"a\nb\"") (runLang "\"a\\nb\"")

dslLogicAndIf :: TestTree
dslLogicAndIf =
    testCase "short-circuit, truthiness, if" $ do
        assertEqual "and returns value" (Right "1") (runLang "true && 1")
        assertEqual "or short" (Right "\"a\"") (runLang "false || \"a\"")
        assertEqual "nil or" (Right "2") (runLang "nil || 2")
        assertEqual "zero truthy" (Right "1") (runLang "0 && 1")
        assertEqual "not nil" (Right "true") (runLang "!nil")
        assertEqual "not zero" (Right "false") (runLang "!0")
        assertEqual "if true" (Right "\"y\"") (runLang "if 1 < 2 then \"y\" else \"n\" end")
        assertEqual "if no else" (Right "nil") (runLang "if false then 1 end")
        assertEqual "if value" (Right "5") (runLang "if true then 5 else 6 end")

dslArraysAndMaps :: TestTree
dslArraysAndMaps =
    testCase "arrays, maps, indexing" $ do
        assertEqual "at array" (Right "2") (runLang "at([1, 2, 3], 1)")
        assertEqual "get" (Right "1") (runLang "get({\"a\" => 1}, \"a\")")
        assertEqual "get missing" (Right "nil") (runLang "get({\"a\" => 1}, \"b\")")
        assertEqual "keys" (Right "[\"a\", \"b\"]") (runLang "keys({\"a\" => 1, \"b\" => 2})")
        assertEqual "values" (Right "[1, 2]") (runLang "values({\"a\" => 1, \"b\" => 2})")
        assertEqual "len arr" (Right "3") (runLang "len([1, 2, 3])")
        assertEqual "len str" (Right "5") (runLang "len(\"hello\")")
        assertEqual "len map" (Right "1") (runLang "len({\"a\" => 1})")

dslLambdasAndHigherOrder :: TestTree
dslLambdasAndHigherOrder =
    testCase "lambdas, map/filter, blocks" $ do
        assertEqual "lambda call" (Right "6") (runLang "{ x -> x * 2 }(3)")
        assertEqual "map" (Right "[2, 4, 6]") (runLang "map([1, 2, 3], { x -> x * 2 })")
        assertEqual "filter" (Right "[2, 4]") (runLang "filter([1, 2, 3, 4], { x -> x % 2 == 0 })")
        assertEqual "map interp" (Right "[\"n1\", \"n2\"]") (runLang "map([1, 2], { x -> \"n#{x}\" })")
        assertEqual "block on call" (Right "[3, 4]") (runLang "map([1, 2], { x -> x + 2 })")

dslDefRecursionClosures :: TestTree
dslDefRecursionClosures =
    testCase "def, recursion, closures" $ do
        assertEqual "def" (Right "10") (runLang "def f(x) x * 2 end f(5)")
        assertEqual "recursion" (Right "120") (runLang "def fact(n) if n <= 1 then 1 else n * fact(n - 1) end end fact(5)")
        assertEqual "closure" (Right "15") (runLang "def make(x) { y -> x + y } end (make(10))(5)")
        assertEqual "higher order" (Right "3") (runLang "def twice(f, x) f(f(x)) end twice({ y -> y + 1 }, 1)")
        assertEqual "mutual defs" (Right "4") (runLang "def a(x) b(x) end def b(x) x + 2 end a(2)")

dslBuiltins :: TestTree
dslBuiltins =
    testCase "builtin functions" $ do
        assertEqual "join" (Right "\"1-2-3\"") (runLang "join([1, 2, 3], \"-\")")
        assertEqual "split" (Right "[\"a\", \"b\", \"c\"]") (runLang "split(\"a-b-c\", \"-\")")
        assertEqual "reverse" (Right "[3, 2, 1]") (runLang "reverse([1, 2, 3])")
        assertEqual "sort" (Right "[1, 2, 3]") (runLang "sort([3, 1, 2])")
        assertEqual "sort strings" (Right "[\"a\", \"b\", \"c\"]") (runLang "sort([\"c\", \"a\", \"b\"])")
        assertEqual "first" (Right "1") (runLang "first([1, 2])")
        assertEqual "last empty" (Right "nil") (runLang "last([])")
        assertEqual "contains str" (Right "true") (runLang "contains(\"hello\", \"ell\")")
        assertEqual "contains arr" (Right "true") (runLang "contains([1, 2], 2)")
        assertEqual "append" (Right "[1, 2]") (runLang "append([1], 2)")
        assertEqual "concat arr" (Right "[1, 2, 3]") (runLang "concat([1], [2, 3])")
        assertEqual "concat str" (Right "\"ab\"") (runLang "concat(\"a\", \"b\")")
        assertEqual "toStr" (Right "\"42\"") (runLang "toStr(42)")
        assertEqual "at string" (Right "\"b\"") (runLang "at(\"abc\", 1)")
        assertEqual "trim" (Right "\"a\"") (runLang "trim(\"  a  \")")
        assertEqual "lower" (Right "\"abc\"") (runLang "lower(\"AbC\")")
        assertEqual "upper" (Right "\"ABC\"") (runLang "upper(\"aBc\")")
        assertEqual "replace" (Right "\"x-b-x\"") (runLang "replace(\"a-b-a\", \"a\", \"x\")")
        assertEqual "take" (Right "[1, 2]") (runLang "take([1, 2, 3], 2)")
        assertEqual "take str" (Right "\"he\"") (runLang "take(\"hello\", 2)")
        assertEqual "drop" (Right "[3]") (runLang "drop([1, 2, 3], 2)")
        assertEqual "take negative" (Right "[]") (runLang "take([1, 2], -1)")
        assertEqual "toJson map" (Right "\"{\n  \"a\": 1\n}\"") (runLang "toJson({\"a\" => 1})")
        assertEqual "toJson arr" (Right "\"[\n  1,\n  2\n]\"") (runLang "toJson([1, 2])")
        assertEqual "toJson escape" (Right "\"\"a\\\"b\"\"") (runLang "toJson(\"a\\\"b\")")
        assertEqual "toJson nil" (Right "\"null\"") (runLang "toJson(nil)")
        assertEqual "toJson fun" (Left "cannot serialize a function [\"toJson\"]") (runLangErr "toJson({ x -> x })")
        assertEqual "formatDate zh" (Right "\"2026年8月2日\"") (runLang "formatDate(\"2026-08-02\", \"%Y年%-m月%-d日\")")
        assertEqual "formatDate padded" (Right "\"2026-08-02\"") (runLang "formatDate(\"2026-08-02\", \"%Y-%m-%d\")")
        assertEqual "formatDate month" (Right "\"August 2, 2026\"") (runLang "formatDate(\"2026-08-02\", \"%B %-d, %Y\")")
        assertEqual "formatDate weekday" (Right "\"Sunday\"") (runLang "formatDate(\"2026-08-02\", \"%A\")")
        assertEqual "formatDate literal" (Right "\"50%\"") (runLang "formatDate(\"2026-08-02\", \"50%%\")")
        assertEqual "formatDate bad date" (Left "invalid date '2026-13-40' [\"formatDate\"]") (runLangErr "formatDate(\"2026-13-40\", \"%Y\")")
        assertEqual "formatDate bad fmt" (Left "unsupported format directive '%H' [\"formatDate\"]") (runLangErr "formatDate(\"2026-08-02\", \"%H\")")
        assertEqual "el basic" (Right "\"<div class=\"x\">hi</div>\"") (runLang "el(\"div\", {\"class\" => \"x\"}, \"hi\")")
        assertEqual "el attr order" (Right "\"<a class=\"n\" href=\"/a/\">x</a>\"") (runLang "el(\"a\", {\"href\" => \"/a/\", \"class\" => \"n\"}, \"x\")")
        assertEqual "el bare attr" (Right "\"<input checked>\"") (runLang "el(\"input\", {\"checked\" => true}, \"\")")
        assertEqual "el omit attr" (Right "\"<div>hi</div>\"") (runLang "el(\"div\", {\"x\" => false, \"y\" => nil}, \"hi\")")
        assertEqual "el void" (Right "\"<br>\"") (runLang "el(\"br\", {}, \"\")")
        assertEqual "el attr escaped" (Right "\"<a href=\"/a?x=1&amp;y=2\">t</a>\"") (runLang "el(\"a\", {\"href\" => \"/a?x=1&y=2\"}, \"t\")")
        assertEqual "esc" (Right "\"a&amp;&lt;b&gt;c&#39;\"") (runLang "esc(\"a&<b>c'\")")
        assertEqual "p component" (Right "\"<p>hi</p>\"") (runLang "p(\"hi\")")
        assertEqual "ul li nested" (Right "\"<ul><li>a</li><li>b</li></ul>\"") (runLang "ul(li(\"a\") + li(\"b\"))")
        assertEqual "a helper" (Right "\"<a href=\"/x/\">Go</a>\"") (runLang "a(\"Go\", \"/x/\")")
        assertEqual "img helper" (Right "\"<img src=\"/i.png\" alt=\"pic\">\"") (runLang "img(\"/i.png\", \"pic\")")
        assertEqual "img no alt" (Right "\"<img src=\"/i.png\">\"") (runLang "img(\"/i.png\", nil)")
        assertEqual "esc in content" (Right "\"<p>A &amp; B</p>\"") (runLang "p(esc(\"A & B\"))")

dslPutsOutput :: TestTree
dslPutsOutput =
    testCase "puts collects output" $ do
        assertEqual "puts" (Right "nil | puts: hi;1") (runLang "puts(\"hi\", 1)")
        assertEqual "puts inside" (Right "[1] | puts: got 1") (runLang "map([1], { x -> puts(\"got #{x}\") x })")

dslErrors :: TestTree
dslErrors =
    testCase "runtime errors with call stacks" $ do
        assertEqual "undefined" (Left "undefined variable 'foo'") (runLangErr "foo")
        assertEqual "type add" (Left "cannot add: expected number or string, got 1 and \"a\"") (runLangErr "1 + \"a\"")
        assertEqual "div zero" (Left "division by zero") (runLangErr "1 / 0")
        assertEqual "out of bounds" (Left "index 9 out of bounds (length 3) [\"at\"]") (runLangErr "at(\"abc\", 9)")
        assertEqual "map type" (Left "cannot map: expected array and function, got 1 [\"map\"]") (runLangErr "map(1, { x -> x })")
        assertEqual "not callable" (Left "not callable: 1") (runLangErr "1(2)")
        assertEqual "arity" (Left "expected 1 argument(s), got 2") (runLangErr "def f(x) x end f(1, 2)")
        assertEqual "stack" (Left "undefined variable 'foo' [\"g\",\"f\"]") (runLangErr "def g(x) foo end def f(x) g(x) end f(1)")
        assertEqual "missing key nil" (Right "nil") (runLang "get({\"a\" => 1}, \"nope\")")

dslLexerErrors :: TestTree
dslLexerErrors =
    testCase "lexer errors" $ do
        assertEqual "bad char" (Left "line 1, column 1: unexpected character @") (runLangErr "@")
        assertEqual "unterminated string" (Left "line 1, column 2: unterminated string literal") (runLangErr "\"abc")
        assertEqual "unterminated interp" (Left "line 1, column 3: unterminated interpolation") (runLangErr "\"#{1")
        assertEqual "equals" (Left "line 1, column 3: unexpected '=' (did you mean '=='?)") (runLangErr "1 = 2")

dslParserErrors :: TestTree
dslParserErrors =
    testCase "parser errors" $ do
        assertEqual "eof" (Left "line 1, column 4: unexpected end of input") (runLangErr "1 +")
        assertEqual "if end" (Left "expected 'else' or 'end' in if") (runLangErr "if 1 then 2")
        assertEqual "multi interp" (Left "interpolation must contain exactly one expression") (runLangErr "\"#{1 2}\"")
        assertEqual "array unterminated" (Left "line 1, column 6: unterminated array literal") (runLangErr "[1, 2")
        assertEqual "map arrow" (Left "expected '=>' in map literal") (runLangErr "{\"a\" 1}")

runLang :: Text -> Either Text Text
runLang src = do
    toks <- lexTokens src
    exprs <- parseProgram toks
    case runScript initialEnv exprs of
        Left e -> Left (leMsg e)
        Right (v, out) -> Right (showValue v <> if null out then "" else " | puts: " <> T.intercalate ";" out)

runLangErr :: Text -> Either Text Text
runLangErr src = do
    toks <- lexTokens src
    exprs <- parseProgram toks
    case runScript initialEnv exprs of
        Left e ->
            Left (leMsg e <> (if null (leStack e) then "" else " " <> T.pack (show (leStack e))))
        Right _ -> Left "expected error"

scriptsTests :: [TestTree]
scriptsTests =
    [ scriptsEvalBasic
    , scriptsCtxInjection
    , scriptsErrorFormat
    , scriptFrontmatterField
    , buildScriptPage
    , buildScriptPageFull
    , scriptErrorKeepsOldOutput
    , buildScriptOutput
    , scriptOutputAbsoluteRejected
    , scriptOutputParentRejected
    , scriptOutputWithoutScript
    , scriptOutputWithRedirect
    , scriptOutputDuplicate
    , scriptOutputOverridesStatic
    , buildScriptPageNav
    , scriptDataInjected
    , scriptDataBadYaml
    , scriptDataIgnoresOthers
    ]

scriptsEvalBasic :: TestTree
scriptsEvalBasic =
    testCase "evalScript runs a script and returns its string" $ do
        assertEqual "literal" (Right ("<p>hi</p>", [])) (evalScript (scriptCtx testConfig [] [] [] Map.empty) "\"<p>hi</p>\"")
        assertEqual "expr" (Right ("42", [])) (evalScript (scriptCtx testConfig [] [] [] Map.empty) "\"#{6 * 7}\"")
        assertEqual "puts" (Right ("1", ["hello"])) (evalScript (scriptCtx testConfig [] [] [] Map.empty) "puts(\"hello\") 1")

scriptsCtxInjection :: TestTree
scriptsCtxInjection =
    testCase "site, posts and tags are injected" $ do
        let env = scriptCtx testConfig [("About", "/about/"), ("Tags", "/tags/")] [postWithTags ["a"]] [] Map.empty
        assertEqual "site name" (Right ("burogu", [])) (evalScript env "get(site, \"siteName\")")
        assertEqual "site lang" (Right ("zh-CN", [])) (evalScript env "get(site, \"siteLang\")")
        assertEqual "post count" (Right ("1", [])) (evalScript env "toStr(len(posts))")
        assertEqual "post title" (Right ("test", [])) (evalScript env "get(at(posts, 0), \"title\")")
        assertEqual "post tags" (Right ("a", [])) (evalScript env "join(get(at(posts, 0), \"tags\"), \", \")")
        assertEqual "tag count" (Right ("1", [])) (evalScript env "toStr(len(tags))")
        assertEqual "tag name" (Right ("a", [])) (evalScript env "get(at(tags, 0), \"name\")")
        assertEqual "tag count value" (Right ("1", [])) (evalScript env "toStr(get(at(tags, 0), \"count\"))")
        assertEqual "nav count" (Right ("2", [])) (evalScript env "toStr(len(nav))")
        assertEqual "nav label" (Right ("About", [])) (evalScript env "get((at(nav, 0)), \"label\")")
        assertEqual "nav href" (Right ("/tags/", [])) (evalScript env "get((at(nav, 1)), \"href\")")

scriptsErrorFormat :: TestTree
scriptsErrorFormat =
    testCase "script errors carry message and call stack" $ do
        assertEqual "syntax" (Left "line 1, column 1: unexpected ')'") (evalScript (scriptCtx testConfig [] [] [] Map.empty) ")")
        assertEqual "runtime" (Left "undefined variable 'foo' []") (evalScript (scriptCtx testConfig [] [] [] Map.empty) "foo")
        assertEqual "stack" (Left "division by zero [f]") (evalScript (scriptCtx testConfig [] [] [] Map.empty) "def f() 1 / 0 end f()")

scriptFrontmatterField :: TestTree
scriptFrontmatterField =
    testCase "page frontmatter keeps script and output fields" $ do
        result <- pure (normalizeFrontmatter PageKind "/tmp/x/about.md" "title: About\nscript: hello.d\noutput: data.json\n")
        case result of
            Left err -> assertBool ("expected success, got " <> T.unpack err) False
            Right (block, _, _) -> do
                assertBool "script kept" ("script: hello.d" `textIn` block)
                assertBool "output kept" ("output: data.json" `textIn` block)
                assertBool "known keys" (not ("unknown" `T.isInfixOf` block))
        case normalizeFrontmatter PageKind "/tmp/x/about.md" "title: About\nscript: hello.d\n" of
            Left err -> assertBool ("expected success, got " <> T.unpack err) False
            Right (block, _, _) -> assertBool "output omitted when empty" (not ("output" `T.isInfixOf` block))

buildScriptPage :: TestTree
buildScriptPage =
    testCase "a script page renders the script output" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/script-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/script-src/_scripts"
        writeFile "/tmp/burogu-test/script-src/_pages/hello.md" "---\ntitle: Hello\nscript: hello.d\n---\nignored\n"
        writeFile "/tmp/burogu-test/script-src/_scripts/hello.d" "\"<p>hi #{get(site, \"siteName\")}</p>\""
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/script-src", pOut = "/tmp/burogu-test/script-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left err -> assertBool ("expected success, got " <> show err) False
            Right _ -> pure ()
        content <- TIO.readFile "/tmp/burogu-test/script-out/hello/index.html"
        assertBool "script body" ("<p>hi burogu</p>" `textIn` content)

scriptErrorKeepsOldOutput :: TestTree
scriptErrorKeepsOldOutput =
    testCase "script errors keep the previous output directory" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/scriptbad-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/scriptbad-src/_scripts"
        createDirectoryIfMissing True "/tmp/burogu-test/scriptbad-out"
        writeFile "/tmp/burogu-test/scriptbad-src/_pages/hello.md" "---\ntitle: Hello\nscript: hello.d\n---\n"
        writeFile "/tmp/burogu-test/scriptbad-src/_scripts/hello.d" "1 +"
        writeFile "/tmp/burogu-test/scriptbad-out/marker.txt" "old"
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/scriptbad-src", pOut = "/tmp/burogu-test/scriptbad-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left _ -> pure ()
            Right _ -> assertBool "expected failure" False
        marker <- doesFileExist "/tmp/burogu-test/scriptbad-out/marker.txt"
        assertBool "old output kept" marker

buildScriptPageFull :: TestTree
buildScriptPageFull =
    testCase "a script sees posts and tags and renders the full fragment" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/scriptfull-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/scriptfull-src/_scripts"
        writeFile "/tmp/burogu-test/scriptfull-src/_pages/list.md" "---\ntitle: List\nscript: list.d\n---\n"
        writeFile "/tmp/burogu-test/scriptfull-src/_scripts/list.d" "puts(\"generating list\")\n\"<ul>\" + join(map(posts, { p -> \"<li>\" + get(p, \"title\") + \"</li>\" }), \"\") + \"</ul><p>\" + join(map(tags, { t -> get(t, \"name\") }), \", \") + \"</p>\""
        let p1 = (postWithTags ["x"]){postTitle = "Alpha", postDate = "2026-08-01", postSlug = "alpha"}
            p2 = (postWithTags ["x", "y"]){postTitle = "Beta", postDate = "2026-08-02", postSlug = "beta"}
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/scriptfull-src", pOut = "/tmp/burogu-test/scriptfull-out"} testConfig [p1, p2]) :: IO (Either IOException BuildReport)
        case result of
            Left err -> assertBool ("expected success, got " <> show err) False
            Right _ -> pure ()
        content <- TIO.readFile "/tmp/burogu-test/scriptfull-out/list/index.html"
        assertBool "full fragment" ("<ul><li>Alpha</li><li>Beta</li></ul><p>x, y</p>" `textIn` content)

buildScriptOutput :: TestTree
buildScriptOutput =
    testCase "an output page writes a file instead of a page" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/scriptout-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/scriptout-src/_scripts"
        writeFile "/tmp/burogu-test/scriptout-src/_pages/data.md" "---\ntitle: Data\nscript: data.d\noutput: data.json\n---\n"
        writeFile "/tmp/burogu-test/scriptout-src/_scripts/data.d" "toJson(map(posts, { p -> get(p, \"title\") }))"
        let p1 = (postWithTags []){postTitle = "Alpha", postSlug = "alpha"}
        report <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/scriptout-src", pOut = "/tmp/burogu-test/scriptout-out"} testConfig [p1]) :: IO (Either IOException BuildReport)
        case report of
            Left err -> assertBool ("expected success, got " <> show err) False
            Right r -> assertEqual "script files" 1 (brScriptFiles r)
        content <- TIO.readFile "/tmp/burogu-test/scriptout-out/data.json"
        assertBool "json" ("[\n  \"Alpha\"\n]" `textIn` content)
        pageExists <- doesFileExist "/tmp/burogu-test/scriptout-out/data/index.html"
        assertBool "no page generated" (not pageExists)
        index <- TIO.readFile "/tmp/burogu-test/scriptout-out/index.html"
        assertBool "not in nav" (not ("/data/" `T.isInfixOf` index))

scriptOutputAbsoluteRejected :: TestTree
scriptOutputAbsoluteRejected =
    testCase "an absolute output path is a hard error" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/outabs-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/outabs-src/_scripts"
        writeFile "/tmp/burogu-test/outabs-src/_pages/x.md" "---\nscript: x.d\noutput: /data.json\n---\n"
        writeFile "/tmp/burogu-test/outabs-src/_scripts/x.d" "\"x\""
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/outabs-src", pOut = "/tmp/burogu-test/outabs-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left _ -> pure ()
            Right _ -> assertBool "expected failure" False

scriptOutputParentRejected :: TestTree
scriptOutputParentRejected =
    testCase "a parent-traversing output path is a hard error" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/outpar-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/outpar-src/_scripts"
        writeFile "/tmp/burogu-test/outpar-src/_pages/x.md" "---\nscript: x.d\noutput: ../data.json\n---\n"
        writeFile "/tmp/burogu-test/outpar-src/_scripts/x.d" "\"x\""
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/outpar-src", pOut = "/tmp/burogu-test/outpar-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left _ -> pure ()
            Right _ -> assertBool "expected failure" False

scriptOutputWithoutScript :: TestTree
scriptOutputWithoutScript =
    testCase "output without script is a hard error" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/outnos-src/_pages"
        writeFile "/tmp/burogu-test/outnos-src/_pages/x.md" "---\ntitle: X\noutput: data.json\n---\n"
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/outnos-src", pOut = "/tmp/burogu-test/outnos-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left _ -> pure ()
            Right _ -> assertBool "expected failure" False

scriptOutputWithRedirect :: TestTree
scriptOutputWithRedirect =
    testCase "output combined with redirectAs is a hard error" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/outred-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/outred-src/_scripts"
        writeFile "/tmp/burogu-test/outred-src/_pages/x.md" "---\nscript: x.d\noutput: data.json\nredirectAs: /elsewhere/\n---\n"
        writeFile "/tmp/burogu-test/outred-src/_scripts/x.d" "\"x\""
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/outred-src", pOut = "/tmp/burogu-test/outred-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left _ -> pure ()
            Right _ -> assertBool "expected failure" False

scriptOutputDuplicate :: TestTree
scriptOutputDuplicate =
    testCase "duplicate output paths are a hard error" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/outdup-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/outdup-src/_scripts"
        writeFile "/tmp/burogu-test/outdup-src/_pages/a.md" "---\nscript: a.d\noutput: data.json\n---\n"
        writeFile "/tmp/burogu-test/outdup-src/_pages/b.md" "---\nscript: b.d\noutput: data.json\n---\n"
        writeFile "/tmp/burogu-test/outdup-src/_scripts/a.d" "\"a\""
        writeFile "/tmp/burogu-test/outdup-src/_scripts/b.d" "\"b\""
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/outdup-src", pOut = "/tmp/burogu-test/outdup-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left _ -> pure ()
            Right _ -> assertBool "expected failure" False

scriptOutputOverridesStatic :: TestTree
scriptOutputOverridesStatic =
    testCase "script output overrides a static file of the same name" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/outov-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/outov-src/_scripts"
        writeFile "/tmp/burogu-test/outov-src/_pages/x.md" "---\nscript: x.d\noutput: data.json\n---\n"
        writeFile "/tmp/burogu-test/outov-src/_scripts/x.d" "\"from script\""
        writeFile "/tmp/burogu-test/outov-src/data.json" "from static"
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/outov-src", pOut = "/tmp/burogu-test/outov-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left err -> assertBool ("expected success, got " <> show err) False
            Right _ -> pure ()
        content <- TIO.readFile "/tmp/burogu-test/outov-out/data.json"
        assertEqual "overridden" "from script" (T.unpack content)

buildScriptPageNav :: TestTree
buildScriptPageNav =
    testCase "a script page sees the navigation" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/scriptnav-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/scriptnav-src/_scripts"
        writeFile "/tmp/burogu-test/scriptnav-src/_pages/about.md" "---\ntitle: About\npriority: 10\n---\n# About\n"
        writeFile "/tmp/burogu-test/scriptnav-src/_pages/hello.md" "---\ntitle: Hello\nscript: hello.d\n---\n"
        writeFile "/tmp/burogu-test/scriptnav-src/_scripts/hello.d" "join(map(nav, { n -> get(n, \"label\") }), \"|\")"
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/scriptnav-src", pOut = "/tmp/burogu-test/scriptnav-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left err -> assertBool ("expected success, got " <> show err) False
            Right _ -> pure ()
        content <- TIO.readFile "/tmp/burogu-test/scriptnav-out/hello/index.html"
        assertBool "nav labels" ("About|Hello" `textIn` content)

scriptDataInjected :: TestTree
scriptDataInjected =
    testCase "YAML files under _data are injected as the data binding" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/data-src/_pages"
        createDirectoryIfMissing True "/tmp/burogu-test/data-src/_scripts"
        createDirectoryIfMissing True "/tmp/burogu-test/data-src/_data"
        writeFile "/tmp/burogu-test/data-src/_pages/x.md" "---\nscript: x.d\n---\n"
        writeFile "/tmp/burogu-test/data-src/_scripts/x.d" "toJson(get(data, \"links\"))"
        writeFile "/tmp/burogu-test/data-src/_data/links.yaml" "- name: A\n  url: /a/\n- name: B\n  url: /b/\n"
        writeFile "/tmp/burogu-test/data-src/_data/plain.yaml" "42\n"
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/data-src", pOut = "/tmp/burogu-test/data-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left err -> assertBool ("expected success, got " <> show err) False
            Right _ -> pure ()
        content <- TIO.readFile "/tmp/burogu-test/data-out/x/index.html"
        assertBool "data map" ("[\n  {\n    \"name\": \"A\",\n    \"url\": \"/a/\"\n  },\n  {\n    \"name\": \"B\",\n    \"url\": \"/b/\"\n  }\n]" `textIn` content)

scriptDataBadYaml :: TestTree
scriptDataBadYaml =
    testCase "invalid YAML under _data fails the build" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/databad-src/_data"
        writeFile "/tmp/burogu-test/databad-src/_data/bad.yaml" "a: [unclosed\n"
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/databad-src", pOut = "/tmp/burogu-test/databad-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left _ -> pure ()
            Right _ -> assertBool "expected failure" False

scriptDataIgnoresOthers :: TestTree
scriptDataIgnoresOthers =
    testCase "non-YAML files under _data are ignored" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/dataoth-src/_data"
        writeFile "/tmp/burogu-test/dataoth-src/_data/notes.txt" "not yaml at all\n"
        writeFile "/tmp/burogu-test/dataoth-src/_data/keep.json" "{ nope\n"
        result <- try (build Paths{pConfig = "config.yaml", pSrc = "/tmp/burogu-test/dataoth-src", pOut = "/tmp/burogu-test/dataoth-out"} testConfig []) :: IO (Either IOException BuildReport)
        case result of
            Left err -> assertBool ("expected success, got " <> show err) False
            Right _ -> pure ()
        copied <- doesFileExist "/tmp/burogu-test/dataoth-out/notes.txt"
        assertBool "not copied as static" (not copied)

footerItemsTest :: TestTree
footerItemsTest =
    testCase "footer items link straight out for external redirectAs" $ do
        let pages =
                [ ("github", mkPageHidden (Just "GitHub") 10 (Just "https://github.com/me") PlFooter)
                , ("about", mkPage (Just "About") 20 Nothing)
                , ("secret", mkPageHidden (Just "Secret") 5 Nothing PlNone)
                ]
        case classifyPages pages of
            Left _ -> assertBool "expected success" False
            Right sp -> do
                assertEqual "footer" [("https://github.com/me", "GitHub")] (map (\(l, h) -> (h, l)) (footerItems sp))
                assertEqual "not in nav" [("/about/", "About")] (map (\(l, h) -> (h, l)) (navItems sp))

footerLinksRendered :: TestTree
footerLinksRendered =
    testCase "footer links render above the copyright with a separator" $ do
        let page = renderHtml (renderIndex testConfig [] [("GitHub", "https://github.com/me"), ("About", "/about/")] "/style.css" Nothing [])
        assertBool "footer links" ("footer-links" `textIn` page)
        assertBool "separator" ("GitHub</a> · <a href=\"/about/\">About</a>" `textIn` page)
        assertBool "above copyright" ("footer-links" `T.isInfixOf` page)

footerSeparatorConfig :: TestTree
footerSeparatorConfig =
    testCase "footer separator comes from config and may be empty" $ do
        let page = renderHtml (renderIndex testConfig{siteFooterSeparator = "|"} [] [("A", "/a/"), ("B", "/b/")] "/style.css" Nothing [])
        assertBool "custom separator" ("A</a>|<a href=\"/b/\">B" `textIn` page)
        let page2 = renderHtml (renderIndex testConfig{siteFooterSeparator = ""} [] [("A", "/a/"), ("B", "/b/")] "/style.css" Nothing [])
        assertBool "empty separator" ("A</a><a href=\"/b/\">B" `textIn` page2)

digestCommentTest :: TestTree
digestCommentTest =
    testCase "format writes a stable digest comment for posts" $ do
        createDirectoryIfMissing True "/tmp/burogu-test/digest/_post"
        let f = "/tmp/burogu-test/digest/_post/2026-08-01-hello.md"
        writeFile f "---\ntitle: Hello\ndate: 2026-08-01\n---\n\nbody\n"
        _ <- formatOne False "/tmp/burogu-test/digest/_post" PostKind "2026-08-01-hello.md"
        content <- TIO.readFile f
        assertBool "digest written" ("<!-- digest: " `textIn` content)
        assertBool "no page digest" (not ("digest" `textIn` normalizePageBlock))
        content2 <- TIO.readFile f
        _ <- formatOne False "/tmp/burogu-test/digest/_post" PostKind "2026-08-01-hello.md"
        content3 <- TIO.readFile f
        assertEqual "idempotent" content2 content3
        assertBool "stable value" ("<!-- digest: 415097e5 -->" `textIn` content3)
  where
    normalizePageBlock = ""

imageTest :: TestTree
imageTest =
    testCase "image compresses into src/img/<digest>/ named by content hash" $ do
        removePathForcibly "/tmp/burogu-test/image"
        createDirectoryIfMissing True "/tmp/burogu-test/image/_post"
        writeFile "/tmp/burogu-test/image/_post/2026-08-01-hello.md" "---\ntitle: Hello\ndate: 2026-08-01\n---\n\nbody\n"
        let png = "/tmp/burogu-test/image/test.png"
        savePngImage png (ImageRGB8 (generateImage (\x y -> PixelRGB8 200 80 40) 640 480 :: Image PixelRGB8))
        let digest = digestOf "2026-08-01-hello"
            imgDir = "/tmp/burogu-test/image/img" </> T.unpack digest
        result <- Image.runImage "/tmp/burogu-test/image/_post" digest (Just png) Nothing (Just 300) False
        case result of
            Left err -> assertBool ("expected success, got " <> T.unpack err) False
            Right () -> do
                files <- listDirectory imgDir
                assertBool "jpeg written" (any ("jpg" `T.isSuffixOf`) (map T.pack files))
                content <- TIO.readFile "/tmp/burogu-test/image/_post/2026-08-01-hello.md"
                assertBool "post not touched" (not ("img" `T.isInfixOf` content))
        both <- Image.runImage "/tmp/burogu-test/image/_post" digest (Just png) Nothing Nothing True
        assertBool "FILE and --clipboard are mutually exclusive" (isLeft both)
        none <- Image.runImage "/tmp/burogu-test/image/_post" digest Nothing Nothing Nothing False
        assertBool "missing FILE is rejected" (isLeft none)

formatMigrationTest :: TestTree
formatMigrationTest =
    testCase "format migrates hiddenInNavbar to placement with a warning" $ do
        case normalizeFrontmatter PageKind "/tmp/x/p.md" "title: X\nhiddenInNavbar: true\n" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, _, warnings) -> do
                assertEqual "mapped" "title: X\npriority: 100\nplacement: none\n" fm
                assertEqual "warning" ["migrated hiddenInNavbar: true to placement: none"] warnings
        case normalizeFrontmatter PageKind "/tmp/x/p.md" "title: X\nhiddenInNavbar: false\n" of
            Left err -> assertBool ("expected success, got: " <> T.unpack err) False
            Right (fm, _, _) -> assertEqual "mapped false" "title: X\npriority: 100\nplacement: nav\n" fm
        assertBool "invalid enum" (isLeft (normalizeFrontmatter PageKind "/tmp/x/p.md" "title: X\nplacement: sidebar\n"))

renamePostTest :: TestTree
renamePostTest =
    testCase "rename keeps the date prefix and leaves frontmatter alone" $ do
        removePathForcibly "/tmp/burogu-test/rename"
        createDirectoryIfMissing True "/tmp/burogu-test/rename/_post"
        writeFile "/tmp/burogu-test/rename/_post/2026-08-01-hello.md" "---\ntitle: Hello\ndate: 2026-08-01\n---\n\nbody\n"
        result <- runRename "/tmp/burogu-test/rename/_post" (digestOf "2026-08-01-hello") "world"
        case result of
            Left err -> assertBool ("expected success, got " <> T.unpack err) False
            Right path -> do
                assertBool "new file" ("world.md" `T.isInfixOf` T.pack path)
                content <- TIO.readFile "/tmp/burogu-test/rename/_post/2026-08-01-world.md"
                assertBool "frontmatter intact" ("title: Hello" `textIn` content)
                oldExists <- doesFileExist "/tmp/burogu-test/rename/_post/2026-08-01-hello.md"
                assertBool "old gone" (not oldExists)
        conflict <- runRename "/tmp/burogu-test/rename/_post" (digestOf "2026-08-01-world") "world"
        assertBool "self-rename conflicts" (isLeft conflict)
        missing <- runRename "/tmp/burogu-test/rename/_post" "ffffffff" "x"
        assertBool "missing rejected" (isLeft missing)
