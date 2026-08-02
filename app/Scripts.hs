module Scripts (evalScript, filterScriptPages, loadData, runPageScripts, scriptCtx) where

import Builtins (initialEnv)
import Cli (Paths (..))
import Config (SiteConfig (..), Theme (..))
import Control.Exception (IOException, catch)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Data.Yaml (decodeEither')
import Eval (LangError (..), runScript)
import Lexer (lexTokens)
import Page (CustomPage (..))
import Parser (parseProgram)
import Post (Post (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO (stderr)
import Value (Env, Value (..), strOf)

-- | The script directory inside src.
scriptsDirName :: FilePath
scriptsDirName = "_scripts"

{- | The user data directory inside src: every YAML file becomes an
entry of the `data` binding (key = file name without the extension).
-}
dataDirName :: FilePath
dataDirName = "_data"

{- | Load `src/_data/*.yaml` into the `data` binding: a map of file
name (without extension) to the parsed YAML value. Files with other
extensions are ignored. Parse errors are aggregated.
-}
loadData :: Paths -> IO (Either [Text] Env)
loadData paths = do
    exists <- doesDirectoryExist dataDir
    if not exists
        then pure (Right Map.empty)
        else do
            names <- listDirectory dataDir
            let yamls = sortOn id [n | n <- names, T.isSuffixOf ".yaml" (T.pack n)]
            results <- mapM loadOne yamls
            let errs = [e | Left e <- results]
            if null errs
                then pure (Right (Map.fromList [(n, aesonToValue v) | Right (n, v) <- results]))
                else pure (Left errs)
  where
    dataDir = pSrc paths </> dataDirName

    loadOne :: FilePath -> IO (Either Text (Text, A.Value))
    loadOne name = do
        econtent <-
            (Right <$> TIO.readFile (dataDir </> name))
                `catch` \(e :: IOException) -> pure (Left (T.pack (show e)))
        pure $ case econtent of
            Left err -> Left (T.pack name <> ": " <> err)
            Right content -> case decodeEither' (encodeUtf8 content) of
                Left err -> Left (T.pack name <> ": invalid YAML: " <> T.pack (show err))
                Right value -> Right (baseName name, value)

    baseName :: FilePath -> Text
    baseName n = fromMaybe (T.pack n) (T.stripSuffix ".yaml" (T.pack n))

{- | Convert an aeson value into a script value. Numbers keep their
integer form when whole.
-}
aesonToValue :: A.Value -> Value
aesonToValue v = case v of
    A.Object m -> VMap (Map.fromList [(K.toText k, aesonToValue x) | (k, x) <- KM.toList m])
    A.Array items -> VArr (V.fromList (map aesonToValue (V.toList items)))
    A.String t -> VStr t
    A.Number n -> case (floatingOrInteger n :: Either Double Integer) of
        Right i -> VNum (fromIntegral i)
        Left d -> VNum (fromRational (toRational d))
    A.Bool b -> VBool b
    A.Null -> VNil

{- | Validate the script declarations and drop output pages before
classification (so `nav` never sees them). Returns the remaining
pages and the output specs (slug, script, output path).
-}
filterScriptPages :: [(Text, CustomPage)] -> Either [Text] ([(Text, CustomPage)], [(Text, Text, Text)])
filterScriptPages pages =
    case traverse checkOne pages of
        Left err -> Left [err]
        Right results -> do
            let kept = [p | (Just p, _) <- results]
                specs = [spec | (_, Just spec) <- results]
            case duplicateOutput (map (\(_, _, o) -> o) specs) of
                Just dup -> Left [dup]
                Nothing -> Right (kept, specs)
  where
    checkOne :: (Text, CustomPage) -> Either Text (Maybe (Text, CustomPage), Maybe (Text, Text, Text))
    checkOne (slug, page) = case (cpOutput page, cpScript page) of
        (Just _, Nothing) -> Left (slug <> ": 'output' requires a 'script' field")
        (Just _, Just _)
            | isJust (cpRedirectAs page) ->
                Left (slug <> ": 'output' and 'redirectAs' cannot be combined")
        (Just out, Just script) -> do
            path <- validateOutput out
            Right (Nothing, Just (slug, script, path))
        _ -> Right (Just (slug, page), Nothing)

    validateOutput :: Text -> Either Text Text
    validateOutput p
        | T.null p = Left "'output' must not be empty"
        | "/" `T.isPrefixOf` p = Left ("'output' must be a relative path, got '" <> p <> "'")
        | ".." `elem` T.splitOn "/" p = Left ("'output' must not contain '..', got '" <> p <> "'")
        | otherwise = Right p

    duplicateOutput :: [Text] -> Maybe Text
    duplicateOutput paths' =
        case [p | (p, n) <- Map.toList (Map.fromListWith (+) [(p, 1 :: Int) | p <- paths']), n > 1] of
            [] -> Nothing
            dup : _ -> Just ("duplicate output path: " <> dup)

{- | Evaluate every script page (its result becomes the page body) and
every output spec (its result becomes a site file). `puts` output
goes to stderr. Errors are aggregated as one message per failing
page.
-}
runPageScripts :: Paths -> SiteConfig -> [(Text, Text)] -> [Post] -> [(Text, CustomPage)] -> [(Text, Text, Text)] -> Env -> IO (Either [Text] ([(Text, CustomPage)], [(FilePath, Text)]))
runPageScripts paths config nav posts pages outputSpecs dataEnv = do
    pageResults <- mapM runPage pages
    specResults <- mapM runSpec outputSpecs
    let errs = [e | Left e <- pageResults] <> [e | Left e <- specResults]
    if null errs
        then pure (Right ([p | Right p <- pageResults], [(o, c) | Right (o, c) <- specResults]))
        else pure (Left errs)
  where
    env = scriptCtx config nav posts pages dataEnv

    runPage :: (Text, CustomPage) -> IO (Either Text (Text, CustomPage))
    runPage (slug, page) = case cpScript page of
        Nothing -> pure (Right (slug, page))
        Just scriptName -> do
            eresult <- evalOne slug scriptName
            case eresult of
                Left err -> pure (Left err)
                Right (body, out) -> do
                    mapM_ (TIO.hPutStrLn stderr) out
                    pure (Right (slug, page{cpBodyHtml = body, cpHasMath = False, cpText = body}))

    runSpec :: (Text, Text, Text) -> IO (Either Text (FilePath, Text))
    runSpec (slug, scriptName, outPath) = do
        let outFile = T.unpack outPath
        eresult <- evalOne slug scriptName
        case eresult of
            Left err -> pure (Left err)
            Right (body, out) -> do
                mapM_ (TIO.hPutStrLn stderr) out
                pure (Right (outFile, body))

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

{- | The global bindings visible to scripts: site, nav, posts, pages,
tags, config and data (the `_data/` YAML files).
-}
scriptCtx :: SiteConfig -> [(Text, Text)] -> [Post] -> [(Text, CustomPage)] -> Env -> Env
scriptCtx config nav posts pages dataEnv =
    Map.union initialEnv $
        Map.fromList
            [ ("site", siteMap config)
            , ("nav", navMap nav)
            , ("posts", VArr (V.fromList (map postMap posts)))
            , ("pages", VArr (V.fromList (map pageMap pages)))
            , ("tags", VArr (V.fromList (map tagMap tagCounts)))
            , ("config", configMap config)
            , ("data", VMap dataEnv)
            ]
  where
    tagCounts = Map.toList (Map.fromListWith (+) [(t, 1 :: Int) | p <- posts, t <- postTags p])

navMap :: [(Text, Text)] -> Value
navMap nav =
    VArr (V.fromList [VMap (Map.fromList [("label", VStr l), ("href", VStr h)]) | (l, h) <- nav])

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
