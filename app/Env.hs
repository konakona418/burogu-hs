module Env (envValue) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (lookupEnv)
import System.IO.Error (catchIOError, isDoesNotExistError)

{- | Look up a value from the process environment first, then from a
KEY=VALUE line in .env (gitignored). Comments and blank lines are
skipped.
-}
envValue :: Text -> IO (Maybe Text)
envValue key = do
    fromProcess <- lookupEnv (T.unpack key)
    case fromProcess of
        Just value -> pure (Just (T.pack value))
        Nothing -> fromDotEnv
  where
    fromDotEnv :: IO (Maybe Text)
    fromDotEnv = do
        econtent <-
            (Just <$> TIO.readFile ".env") `catchIOError` \(e :: IOError) ->
                pure (if isDoesNotExistError e then Nothing else Just (T.pack (show e)))
        pure (econtent >>= lookupLine)
    lookupLine :: Text -> Maybe Text
    lookupLine content =
        case filter match (T.lines content) of
            [] -> Nothing
            line : _ ->
                let (k, rest) = T.breakOn "=" line
                 in if T.strip k == key then Just (T.strip (T.drop 1 rest)) else Nothing
    match :: Text -> Bool
    match line =
        not (T.null (T.strip line))
            && not ("#" `T.isPrefixOf` T.strip line)
            && "=" `T.isInfixOf` line
            && T.strip (fst (T.breakOn "=" line)) == key
