{-# LANGUAGE TemplateHaskell #-}

{- | Host-side support for the layout script: the embedded default
shell, the non-markup helpers the shell composes with (@og-tags@,
@math-tags@, @theme-js@, @code-js@, @t@), and the model globals
(posts/pages/tags/data) exposed to scripts. The renderer itself lives
in "Html".
-}
module Layout (
    LayoutError (..),
    buildModelGlobals,
    configMap,
    defaultProgram,
    layoutHelpers,
    loadLayoutProgram,
    navMap,
    presetProgram,
    resolveProgram,
    siteMap,
) where

import Control.Exception (Exception)
import Data.FileEmbed (embedFile)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Pandoc.Options (defaultKaTeXURL, defaultMathJaxURL)

import Cli (Paths (..))
import Config (SiteConfig (..), Theme (..))
import I18n (fromSiteLang, t, tWith)
import Page (CustomPage (..))
import Post (Post (..))
import Script qualified as S
import Script.Value (Env, Html (..), Value (..))

-- | A layout script failed at runtime; the build reports it and stops.
newtype LayoutError = LayoutError [Text]
    deriving (Show)

instance Exception LayoutError

{- | Each built-in preset ships its own page shell (embedded). Users
override it by pointing @theme.layout@ at a script under src/ in
config.yaml.
-}
presetLayoutSource :: Text -> Text
presetLayoutSource preset = case preset of
    "shaft" -> decodeUtf8 $(embedFile "app/templates/shaft.d")
    _ -> decodeUtf8 $(embedFile "app/templates/aria.d")

-- | The compiled shell a preset ships.
presetProgram :: Text -> S.Program
presetProgram preset = case S.compile (presetLayoutSource preset) of
    Left errs -> error ("internal: " <> T.unpack preset <> " layout does not compile: " <> T.unpack (T.intercalate "; " errs))
    Right p -> p

-- | The shell used when nothing else applies (tests / unknown preset).
defaultProgram :: S.Program
defaultProgram = presetProgram "aria"

{- | The program to run: a user @theme.layout@ override wins, otherwise
the preset's own shell.
-}
resolveProgram :: SiteConfig -> Maybe S.Program -> S.Program
resolveProgram config = fromMaybe (presetProgram (themePreset (siteTheme config)))

{- | Read and compile the user's layout script, if configured. A missing
file or a compile error fails the build before anything is written.
Returns Nothing when no override is configured (the embedded default is
used).
-}
loadLayoutProgram :: Paths -> SiteConfig -> IO (Maybe S.Program)
loadLayoutProgram paths config = case themeLayout (siteTheme config) of
    Nothing -> pure Nothing
    Just rel -> do
        let file = pSrc paths </> T.unpack rel
        exists <- doesFileExist file
        if not exists
            then ioError (userError ("theme.layout file not found in " <> pSrc paths <> ": " <> T.unpack rel))
            else do
                content <- TIO.readFile file
                case S.compile content of
                    Left errs -> ioError (userError ("theme.layout " <> T.unpack rel <> ":\n" <> T.unpack (T.intercalate "\n" errs)))
                    Right program -> pure (Just program)

-- | The model globals: posts, pages, tags and data.
buildModelGlobals :: [Post] -> [(Text, CustomPage)] -> Env -> Env
buildModelGlobals posts pages dataEnv =
    Map.fromList
        [ ("posts", VArr (V.fromList (map postMap posts)))
        , ("pages", VArr (V.fromList (map pageMap pages)))
        , ("tags", VArr (V.fromList (map tagMap tagCounts)))
        , ("data", VMap (Map.toList dataEnv))
        ]
  where
    tagCounts = Map.toList (Map.fromListWith (+) [(t', 1 :: Int) | p <- posts, t' <- postTags p])

siteMap :: SiteConfig -> Value
siteMap config =
    VMap $
        [ ("siteName", VStr (siteName config))
        , ("siteAuthor", VStr (siteAuthor config))
        , ("siteDescription", VStr (siteDescription config))
        , ("siteLang", VStr (siteLang config))
        , ("siteCopyright", VStr (siteCopyright config))
        , ("footerSeparator", VStr (siteFooterSeparator config))
        ]
            <> maybe [] (\u -> [("baseUrl", VStr u)]) (siteBaseUrl config)
            <> maybe [] (\g -> [("siteGeneratedBy", VStr g)]) (siteGeneratedBy config)

configMap :: SiteConfig -> Value
configMap config =
    VMap $
        [("theme", themeMap (siteTheme config))]
            <> maybe [] (\r -> [("srcRepo", VStr r)]) (siteSrcRepo config)

themeMap :: Theme -> Value
themeMap theme =
    VMap $
        [ ("preset", VStr (themePreset theme))
        , ("math", VStr (themeMath theme))
        , ("extraCss", VArr (V.fromList (map VStr (themeExtraCss theme))))
        , ("extraJs", VArr (V.fromList (map VStr (themeExtraJs theme))))
        ]
            <> maybe [] (\u -> [("mathUrl", VStr u)]) (themeMathUrl theme)

navMap :: [(Text, Text)] -> Value
navMap nav = VArr (V.fromList [VMap [("label", VStr l), ("href", VStr h)] | (l, h) <- nav])

postMap :: Post -> Value
postMap p =
    VMap $
        [ ("title", VStr (postTitle p))
        , ("date", VStr (postDate p))
        , ("tags", VArr (V.fromList (map VStr (postTags p))))
        , ("url", VStr ("/posts/" <> postSlug p <> "/"))
        , ("draft", VBool (postDraft p))
        , ("text", VStr (postText p))
        ]
            <> maybe [] (\d -> [("description", VStr d)]) (postDescription p)

pageMap :: (Text, CustomPage) -> Value
pageMap (slug, p) =
    VMap $
        [ ("slug", VStr slug)
        , ("url", VStr ("/" <> slug <> "/"))
        ]
            <> maybe [] (\tt -> [("title", VStr tt)]) (cpTitle p)
            <> maybe [] (\r -> [("redirectAs", VStr r)]) (cpRedirectAs p)

tagMap :: (Text, Int) -> Value
tagMap (name, count) = VMap [("name", VStr name), ("count", VNum (fromIntegral count))]

{- | The non-markup helpers the default shell (and user shells) compose
with. Markup itself is written in the script.
-}
layoutHelpers :: SiteConfig -> [(Text, Value)]
layoutHelpers config =
    [ ("og-tags", VNative "og-tags" ogTagsFn)
    , ("math-tags", VNative "math-tags" mathTagsFn)
    , ("theme-js", VNative "theme-js" (\_ -> ok (rawScriptNode themeScript)))
    , ("code-js", VNative "code-js" (\_ -> ok (rawScriptNode codeScript)))
    , ("t", VNative "t" tFn)
    ]
  where
    theme = siteTheme config
    lang = fromSiteLang (siteLang config)

    ok v = Right (v, [])

    ogTagsFn [VMap page] = Right (VArr (V.fromList (ogNodes page)), [])
    ogTagsFn _ = Left (errAt "og-tags expects the page map")

    ogNodes page =
        let title = fieldText "title" page ""
            ogType = fieldText "ogType" page "website"
            ogDescription = case field "ogDescription" page of
                VStr d -> d
                _ -> siteDescription config
         in [ meta "og:title" title
            , meta "og:type" ogType
            , meta "og:site_name" (siteName config)
            , meta "og:description" ogDescription
            ]
                <> maybe [] (\baseUrl -> [meta "og:url" (baseUrl <> fieldText "ogPath" page "")]) (siteBaseUrl config)

    meta property content =
        VHtml (HElem "meta" [("property", VStr property), ("content", VStr content)] [])

    mathTagsFn [VMap page] = Right (VArr (V.fromList (mathNodes (S.truthy (field "hasMath" page)))), [])
    mathTagsFn _ = Left (errAt "math-tags expects the page map")

    mathNodes hasMath = case (themeMath theme, hasMath) of
        ("mathjax", True) -> [scriptTag [("defer", VBool True), ("src", VStr (fromMaybe defaultMathJaxURL (themeMathUrl theme))), ("type", VStr "text/javascript")] []]
        ("katex", True) -> [tag "link" [("rel", VStr "stylesheet"), ("href", VStr (katexBase <> "katex.min.css"))] [], scriptTag [("defer", VBool True), ("src", VStr (katexBase <> "katex.min.js"))] [], rawScriptNode katexScript]
        _ -> []

    katexBase = fromMaybe defaultKaTeXURL (themeMathUrl theme)

    tFn (VStr key : args) = Right (VStr (case [s | VStr s <- args] of [] -> t lang key; as -> tWith lang key as), [])
    tFn _ = Left (errAt "t expects a key")

    tag name attrs children = VHtml (HElem name attrs (map VHtml children))
    scriptTag attrs children = tag "script" attrs children
    rawScriptNode text = VHtml (HElem "script" [] [VHtml (HRaw text)])

    field k m = fromMaybe VNil (lookup k m)
    fieldText k m d = case field k m of
        VStr s -> s
        _ -> d

    errAt m = S.LangError m [] Nothing

themeScript :: Text
themeScript = decodeUtf8 $(embedFile "js/theme.js")

codeScript :: Text
codeScript = decodeUtf8 $(embedFile "js/code.js")

katexScript :: Text
katexScript = decodeUtf8 $(embedFile "js/katex.js")
