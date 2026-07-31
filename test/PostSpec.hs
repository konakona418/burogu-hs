module Main where

import Config (SiteConfig (..), Theme (..))
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import Feed (feedUrl, renderAtom)
import Html (groupByTag, tagUrl)
import Post (Post (..), parsePost, warnCaseTags)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

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
        ]

fullFrontmatter :: TestTree
fullFrontmatter =
    testCase "full frontmatter extracts all fields" $ do
        result <- parsePost "tango" "hello.md" frontmatter
        assertRight result $ \post -> do
            assertEqual "slug" "hello" (postSlug post)
            assertEqual "title" "Hello, World" (postTitle post)
            assertEqual "date" "2026-07-31" (postDate post)
            assertEqual "tags" ["essay", "test"] (postTags post)
            assertEqual "description" (Just "First test post") (postDescription post)

dateFromFilenamePrefix :: TestTree
dateFromFilenamePrefix =
    testCase "date falls back to filename prefix" $ do
        result <- parsePost "tango" "2026-07-31-hello.md" (frontmatterWith ["title: Hello"])
        assertRight result $ \post -> do
            assertEqual "date" "2026-07-31" (postDate post)
            assertEqual "slug drops the date prefix" "hello" (postSlug post)

frontmatterDateBeatsPrefix :: TestTree
frontmatterDateBeatsPrefix =
    testCase "frontmatter date wins over filename prefix" $ do
        result <- parsePost "tango" "2026-07-31-hello.md" (frontmatterWith ["title: Hello", "date: 2025-01-02"])
        assertRight result $ \post -> do
            assertEqual "date" "2025-01-02" (postDate post)
            assertEqual "slug still drops the date prefix" "hello" (postSlug post)

invalidDateErrors :: TestTree
invalidDateErrors =
    testCase "invalid frontmatter date is a hard error" $ do
        result <- parsePost "tango" "hello.md" (frontmatterWith ["title: Hello", "date: 2026/07/31"])
        assertLeft result
        result2 <- parsePost "tango" "2026-07-31-hello.md" (frontmatterWith ["title: Hello", "date: not-a-date"])
        assertLeft result2

missingDateErrors :: TestTree
missingDateErrors =
    testCase "missing date with no prefix is a hard error" $ do
        result <- parsePost "tango" "hello.md" (frontmatterWith ["title: Hello"])
        assertLeft result

titleDefaultsToSlug :: TestTree
titleDefaultsToSlug =
    testCase "missing title falls back to slug" $ do
        result <- parsePost "tango" "2026-07-31-hello.md" "---\ndate: 2026-07-31\n---\n"
        assertRight result $ \post -> assertEqual "title" "hello" (postTitle post)

nonAsciiSlugPreserved :: TestTree
nonAsciiSlugPreserved =
    testCase "non-ASCII filename is preserved as slug" $ do
        result <- parsePost "tango" "2026-08-02-你好世界.md" "---\ndate: 2026-08-02\n---\n"
        assertRight result $ \post -> assertEqual "slug" "你好世界" (postSlug post)

tagsParsed :: TestTree
tagsParsed =
    testCase "tags list is parsed" $ do
        result <- parsePost "tango" "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [a, b, c]"])
        assertRight result $ \post -> assertEqual "tags" ["a", "b", "c"] (postTags post)

tagsWrongTypeErrors :: TestTree
tagsWrongTypeErrors =
    testCase "tags with a non-list value is a hard error" $ do
        result <- parsePost "tango" "hello.md" (frontmatterWith ["tags: not-a-list"])
        assertLeft result

emptyTagErrors :: TestTree
emptyTagErrors =
    testCase "empty tag is a hard error" $ do
        result <- parsePost "tango" "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [\"\", b]"])
        assertLeft result

whitespaceTagErrors :: TestTree
whitespaceTagErrors =
    testCase "whitespace-only tag is a hard error" $ do
        result <- parsePost "tango" "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [\"   \"]"])
        assertLeft result

reservedCharTagErrors :: TestTree
reservedCharTagErrors =
    testCase "tag with a reserved character is a hard error" $ do
        result <- parsePost "tango" "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [C#]"])
        assertLeft result
        result2 <- parsePost "tango" "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [a b]"])
        assertLeft result2
        result3 <- parsePost "tango" "hello.md" (frontmatterWith ["date: 2026-07-31", "tags: [a/b]"])
        assertLeft result3

draftParsed :: TestTree
draftParsed =
    testCase "draft: true is parsed" $ do
        result <- parsePost "tango" "hello.md" (frontmatterWith ["title: Draft", "draft: true", "date: 2026-07-31"])
        assertRight result $ \post -> assertEqual "draft" True (postDraft post)

draftAllowsMissingDate :: TestTree
draftAllowsMissingDate =
    testCase "draft allows a missing date" $ do
        result <- parsePost "tango" "hello.md" (frontmatterWith ["title: Draft", "draft: true"])
        assertRight result $ \post -> assertEqual "draft" True (postDraft post)

bodyRendered :: TestTree
bodyRendered =
    testCase "body is rendered as an HTML fragment" $ do
        result <- parsePost "tango" "hello.md" "---\ntitle: Hello\ndate: 2026-07-31\n---\n# Heading\n\nBody text"
        assertRight result $ \post -> do
            assertBool "contains h1" ("<h1" `textIn` postBodyHtml post)
            assertBool "contains paragraph" ("Body text" `textIn` postBodyHtml post)

highlightedCode :: TestTree
highlightedCode =
    testCase "code blocks are syntax-highlighted with token classes" $ do
        result <- parsePost "tango" "hello.md" "---\ntitle: Hello\ndate: 2026-07-31\n---\n```haskell\n-- a comment\nmain = putStrLn \"hi\"\n```"
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

escapedPost :: Post
escapedPost =
    (postWithTags ["essay"]){postTitle = "A & B", postBodyHtml = "<p>hi</p>", postDescription = Just "desc"}

testConfig :: SiteConfig
testConfig =
    SiteConfig
        { siteName = "burogu"
        , siteAuthor = "moe li"
        , siteDescription = "A test blog"
        , siteLang = "zh-CN"
        , siteBaseUrl = Just "https://lizi.moe"
        , siteTheme = Theme{themeHighlightStyle = "tango"}
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
