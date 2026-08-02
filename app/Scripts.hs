module Scripts (evalScript, runPageScripts, scriptCtx) where

import Builtins (initialEnv)
import Cli (Paths (..))
import Config (SiteConfig (..), Theme (..))
import Control.Exception (IOException, catch)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Eval (LangError (..), runScript)
import Lexer (lexTokens)
import Page (CustomPage (..))
import Parser (parseProgram)
import Post (Post (..))
import System.FilePath ((</>))
import System.IO (stderr)
import Value (Env, Value (..), strOf)

-- | The script directory inside src.
scriptsDirName :: FilePath
scriptsDirName = "_scripts"

{- | Evaluate every page that declares a `script:` frontmatter field:
the script (a file under `src/_scripts/`) runs with the site context
bound and its string result becomes the page body (raw HTML). A page
with both `script:` and `output:` is a file generator: its script
result is returned as an output file instead (path relative to the
site root) and the page itself is dropped. `puts` output goes to
stderr. Errors are aggregated as one message per failing page.
-}
runPageScripts :: Paths -> SiteConfig -> [Post] -> [(Text, CustomPage)] -> IO (Either [Text] ([(Text, CustomPage)], [(FilePath, Text)]))
runPageScripts paths config posts pages = do
    results <- mapM runOne pages
    let errs = [e | Left e <- results]
    if not (null errs)
        then pure (Left errs)
        else do
            let keptPages = [p | Right (Just p, _) <- results]
                files = [(o, c) | Right (_, fs) <- results, (o, c) <- fs]
            case duplicateOutput (map fst files) of
                Just dup -> pure (Left [dup])
                Nothing -> pure (Right (keptPages, files))
  where
    env = scriptCtx config posts pages

    runOne :: (Text, CustomPage) -> IO (Either Text (Maybe (Text, CustomPage), [(FilePath, Text)]))
    runOne (slug, page)
        | isJust (cpOutput page) && isNothing (cpScript page) =
            pure (Left (slug <> ": 'output' requires a 'script' field"))
        | isJust (cpOutput page) && isJust (cpRedirectAs page) =
            pure (Left (slug <> ": 'output' and 'redirectAs' cannot be combined"))
        | isJust (cpOutput page) = do
            let outPath = fromJust (cpOutput page)
            case validateOutput outPath of
                Left err -> pure (Left (slug <> ": " <> err))
                Right path -> do
                    eresult <- evalOne slug (fromJust (cpScript page))
                    case eresult of
                        Left err -> pure (Left err)
                        Right (body, out) -> do
                            mapM_ (TIO.hPutStrLn stderr) out
                            pure (Right (Nothing, [(path, body)]))
        | isJust (cpScript page) = do
            eresult <- evalOne slug (fromJust (cpScript page))
            case eresult of
                Left err -> pure (Left err)
                Right (body, out) -> do
                    mapM_ (TIO.hPutStrLn stderr) out
                    pure (Right (Just (slug, page{cpBodyHtml = body, cpHasMath = False, cpText = body}), []))
        | otherwise = pure (Right (Just (slug, page), []))

    evalOne :: Text -> Text -> IO (Either Text (Text, [Text]))
    evalOne slug scriptName = do
        econtent <-
            (Right <$> TIO.readFile (pSrc paths </> scriptsDirName </> T.unpack scriptName))
                `catch` \(e :: IOException) -> pure (Left (T.pack (show e)))
        pure $ case econtent of
            Left err -> Left (slug <> ": script " <> scriptName <> ": " <> err)
            Right content -> case evalScript env content of
                Left err -> Left (slug <> ": script " <> scriptName <> ": " <> err)
                Right r -> Right r

    validateOutput :: Text -> Either Text FilePath
    validateOutput p
        | T.null p = Left "'output' must not be empty"
        | "/" `T.isPrefixOf` p = Left ("'output' must be a relative path, got '" <> p <> "'")
        | ".." `elem` T.splitOn "/" p = Left ("'output' must not contain '..', got '" <> p <> "'")
        | otherwise = Right (T.unpack p)

    duplicateOutput :: [FilePath] -> Maybe Text
    duplicateOutput paths' =
        case [p | (p, n) <- Map.toList (Map.fromListWith (+) [(p, 1 :: Int) | p <- paths']), n > 1] of
            [] -> Nothing
            dup : _ -> Just ("duplicate output path: " <> T.pack dup)

{- | Parse and evaluate a script, returning its string result and the
collected `puts` output.
-}
evalScript :: Env -> Text -> Either Text (Text, [Text])
evalScript env content = do
    toks <- lexTokens content
    exprs <- parseProgram toks
    (value, out) <- case runScript env exprs of
        Left e -> Left (leMsg e <> " [" <> T.intercalate ", " (leStack e) <> "]")
        Right r -> Right r
    body <- either Left Right (strOf value)
    Right (body, out)

{- | The global bindings visible to scripts: site, posts, pages, tags
and config.
-}
scriptCtx :: SiteConfig -> [Post] -> [(Text, CustomPage)] -> Env
scriptCtx config posts pages =
    Map.union initialEnv $
        Map.fromList
            [ ("site", siteMap config)
            , ("posts", VArr (V.fromList (map postMap posts)))
            , ("pages", VArr (V.fromList (map pageMap pages)))
            , ("tags", VArr (V.fromList (map tagMap tagCounts)))
            , ("config", configMap config)
            ]
  where
    tagCounts = Map.toList (Map.fromListWith (+) [(t, 1 :: Int) | p <- posts, t <- postTags p])

siteMap :: SiteConfig -> Value
siteMap config =
    VMap . Map.fromList $
        [ ("siteName", VStr (siteName config))
        , ("siteAuthor", VStr (siteAuthor config))
        , ("siteDescription", VStr (siteDescription config))
        , ("siteLang", VStr (siteLang config))
        , ("siteCopyright", VStr (siteCopyright config))
        ]
            <> maybe [] (\u -> [("baseUrl", VStr u)]) (siteBaseUrl config)
            <> maybe [] (\g -> [("siteGeneratedBy", VStr g)]) (siteGeneratedBy config)

postMap :: Post -> Value
postMap p =
    VMap . Map.fromList $
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
    VMap . Map.fromList $
        [ ("slug", VStr slug)
        , ("url", VStr ("/" <> slug <> "/"))
        ]
            <> maybe [] (\t -> [("title", VStr t)]) (cpTitle p)
            <> maybe [] (\r -> [("redirectAs", VStr r)]) (cpRedirectAs p)

tagMap :: (Text, Int) -> Value
tagMap (name, count) =
    VMap (Map.fromList [("name", VStr name), ("count", VNum (fromIntegral count))])

configMap :: SiteConfig -> Value
configMap config =
    VMap . Map.fromList $
        [ ("theme", themeMap (siteTheme config))
        ]
            <> maybe [] (\r -> [("srcRepo", VStr r)]) (siteSrcRepo config)

themeMap :: Theme -> Value
themeMap theme =
    VMap . Map.fromList $
        [ ("preset", VStr (themePreset theme))
        , ("math", VStr (themeMath theme))
        , ("extraCss", VArr (V.fromList (map VStr (themeExtraCss theme))))
        , ("extraJs", VArr (V.fromList (map VStr (themeExtraJs theme))))
        ]
            <> maybe [] (\u -> [("mathUrl", VStr u)]) (themeMathUrl theme)
