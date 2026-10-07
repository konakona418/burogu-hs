{- | Host integration for page scripts: load @src/_data/*.yaml@ into the
script environment, validate the frontmatter @script@/@output@
declarations, and evaluate each script with the new language. A body
script's rendered result becomes the page body; an @output@ script's
result is written to a site file.
-}
module Scripts (evalScript, filterScriptPages, loadData, runPageScripts, scriptsEnv) where

import Cli (Paths (..))
import Config (SiteConfig (..))
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
import Layout (buildModelGlobals, configMap, layoutHelpers, navMap, siteMap)
import Page (CustomPage (..))
import Post (Post (..))
import Script qualified as S
import Script.Value (Env, Value (..))
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.IO (stderr)

-- | The script directory inside src.
scriptsDirName :: FilePath
scriptsDirName = "_scripts"

-- | The user data directory inside src.
dataDirName :: FilePath
dataDirName = "_data"

{- | Load @src/_data/*.yaml@ into an environment: a map of file name
(without extension) to the parsed YAML value. Files with other
extensions are ignored; parse errors are aggregated.
-}
loadData :: Paths -> IO (Either [Text] Env)
loadData paths = do
    let dir = pSrc paths </> dataDirName
    exists <- doesDirectoryExist dir
    if not exists
        then pure (Right Map.empty)
        else do
            names <- listDirectory dir
            let yamls = sortOn id [n | n <- names, ".yaml" `T.isSuffixOf` T.pack n]
            results <- mapM (loadOne dir) yamls
            let errs = [e | Left e <- results]
            if null errs
                then pure (Right (Map.fromList [(n, aesonToValue v) | Right (n, v) <- results]))
                else pure (Left errs)
  where
    loadOne dir name = do
        econtent <- (Right <$> TIO.readFile (dir </> name)) `catch` \(e :: IOException) -> pure (Left (T.pack (show e)))
        pure $ case econtent of
            Left err -> Left (T.pack name <> ": " <> err)
            Right content -> case decodeEither' (encodeUtf8 content) of
                Left err -> Left (T.pack name <> ": invalid YAML: " <> T.pack (show err))
                Right value -> Right (baseName name, value)

    baseName n = fromMaybe (T.pack n) (T.stripSuffix ".yaml" (T.pack n))

-- | Convert a YAML value into a script value. Whole numbers stay whole.
aesonToValue :: A.Value -> Value
aesonToValue v = case v of
    A.Object m -> VMap (sortOn fst [(K.toText k, aesonToValue x) | (k, x) <- KM.toList m])
    A.Array items -> VArr (V.fromList (map aesonToValue (V.toList items)))
    A.String t -> VStr t
    A.Number n -> case (floatingOrInteger n :: Either Double Integer) of
        Right i -> VNum (fromIntegral i)
        Left d -> VNum (fromRational (toRational d))
    A.Bool b -> VBool b
    A.Null -> VNil

{- | Validate the script declarations and drop output pages before
classification (so `nav` never sees them). Returns the remaining pages
and the output specs (slug, script, output path).
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
    checkOne (slug, page) = case (cpOutput page, cpScript page) of
        (Just _, Nothing) -> Left (slug <> ": 'output' requires a 'script' field")
        (Just _, Just _)
            | isJust (cpRedirectAs page) ->
                Left (slug <> ": 'output' and 'redirectAs' cannot be combined")
        (Just out, Just script) -> do
            path <- validateOutput slug out
            Right (Nothing, Just (slug, script, path))
        _ -> Right (Just (slug, page), Nothing)

    validateOutput slug p
        | T.null p = Left (slug <> ": 'output' must not be empty")
        | "/" `T.isPrefixOf` p = Left (slug <> ": 'output' must be a relative path, got '" <> p <> "'")
        | ".." `elem` T.splitOn "/" p = Left (slug <> ": 'output' must not contain '..', got '" <> p <> "'")
        | otherwise = Right p

    duplicateOutput paths' =
        case [p | (p, n) <- Map.toList (Map.fromListWith (+) [(p, 1 :: Int) | p <- paths']), n > 1] of
            [] -> Nothing
            dup : _ -> Just ("duplicate output path: " <> dup)

{- | The environment visible to body and output scripts: site, nav,
config and the model globals (posts/pages/tags/data), plus the built-in
library and the layout helpers.
-}
scriptsEnv :: SiteConfig -> [(Text, Text)] -> [Post] -> [(Text, CustomPage)] -> Env -> Env
scriptsEnv config nav posts pages dataEnv =
    Map.unions
        [ Map.fromList
            [ ("site", siteMap config)
            , ("nav-links", navMap nav)
            , ("config", configMap config)
            ]
        , buildModelGlobals posts pages dataEnv
        , Map.fromList (layoutHelpers config)
        , S.initialEnv
        ]

-- | Parse and evaluate a script, returning its rendered HTML and the
-- collected `puts` output.
evalScript :: Env -> Text -> Either Text (Text, [Text])
evalScript env content = case S.evalText env content of
    Left e -> Left (S.formatError e)
    Right (value, out) -> (,out) <$> S.renderResult value

{- | Evaluate every body script (its result becomes the page body) and
every output spec (its result becomes a site file). `puts` output goes
to stderr; errors are aggregated as one message per failing script.
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
    env = scriptsEnv config nav posts pages dataEnv

    runPage (slug, page) = case cpScript page of
        Nothing -> pure (Right (slug, page))
        Just scriptName -> do
            eresult <- evalFile slug scriptName
            case eresult of
                Left err -> pure (Left err)
                Right (body, out) -> do
                    mapM_ (TIO.hPutStrLn stderr) out
                    pure (Right (slug, page{cpBodyHtml = body, cpHasMath = False, cpText = body}))

    runSpec (slug, scriptName, outPath) = do
        eresult <- evalFile slug scriptName
        case eresult of
            Left err -> pure (Left err)
            Right (body, out) -> do
                mapM_ (TIO.hPutStrLn stderr) out
                pure (Right (T.unpack outPath, body))

    evalFile slug scriptName = do
        econtent <-
            (Right <$> TIO.readFile (pSrc paths </> scriptsDirName </> T.unpack scriptName))
                `catch` \(e :: IOException) -> pure (Left (T.pack (show e)))
        pure $ case econtent of
            Left err -> Left (slug <> ": script " <> scriptName <> ": " <> err)
            Right content -> case evalScript env content of
                Left err -> Left (slug <> ": script " <> scriptName <> ": " <> err)
                Right r -> Right r
