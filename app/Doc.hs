{-# LANGUAGE TemplateHaskell #-}

module Doc (OutputStyle (..), extractSection, langFromLocale, manualContent, render, run, sections) where

import Control.Exception (IOException, catch)
import Data.FileEmbed (embedFile)
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as TIO
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)

data OutputStyle = Color | Plain

data Segment = SegText Text | SegBold Text | SegCode Text | SegLink Text Text

data BlockState = Normal | Fence

{- | The manuals are embedded into the binary at compile time, so the
installed binary needs no data files.
-}
manualEn :: Text
manualEn = decodeUtf8 manualEnBytes
  where
    manualEnBytes = $(embedFile "docs/manual.en.md")

manualZh :: Text
manualZh = decodeUtf8 manualZhBytes
  where
    manualZhBytes = $(embedFile "docs/manual.zh.md")

manualZhHant :: Text
manualZhHant = decodeUtf8 manualZhHantBytes
  where
    manualZhHantBytes = $(embedFile "docs/manual.zh-Hant.md")

manualJa :: Text
manualJa = decodeUtf8 manualJaBytes
  where
    manualJaBytes = $(embedFile "docs/manual.ja.md")

manualContent :: Text -> Text
manualContent lang = case lang of
    "zh" -> manualZh
    "zh-Hant" -> manualZhHant
    "ja" -> manualJa
    _ -> manualEn

run :: Maybe Text -> Maybe Text -> Maybe Text -> IO ()
run mSection mLang mColor = do
    lang <- resolveLang mLang
    let content = manualContent lang
    style <- resolveStyle mColor
    body <- case mSection of
        Nothing -> pure content
        Just sec -> case extractSection sec content of
            Left err -> die (T.unpack err)
            Right section -> pure section
    TIO.putStr (render style body)

{- | Resolve the manual language: an explicit --lang wins; otherwise
follow the locale (LC_ALL, then LC_MESSAGES, then LANG). ja* locales
select Japanese; zh_TW/zh_HK/zh_MO/zh-Hant select Traditional Chinese,
other zh* locales Simplified Chinese; everything else English.
-}
resolveLang :: Maybe Text -> IO Text
resolveLang (Just lang)
    | lang `elem` ["en", "zh", "zh-Hant", "ja"] = pure lang
    | otherwise = die ("unknown language '" <> T.unpack lang <> "'; use en, zh, zh-Hant or ja")
resolveLang Nothing = do
    envs <- firstEnv ["LC_ALL", "LC_MESSAGES", "LANG"]
    pure (langFromLocale envs)

{- | The language implied by a list of locale environment values
(empty list = no locale set).
-}
langFromLocale :: [Text] -> Text
langFromLocale = maybe "en" pick . listToMaybeEnv
  where
    pick env =
        let lower = T.toLower env
         in if "ja" `T.isPrefixOf` lower
                then "ja"
                else
                    if "zh" `T.isPrefixOf` lower
                        then
                            if any (`T.isInfixOf` lower) ["hant", "tw", "hk", "mo"]
                                then "zh-Hant"
                                else "zh"
                        else "en"

firstEnv :: [String] -> IO [Text]
firstEnv = go []
  where
    go acc [] = pure (reverse acc)
    go acc (k : ks) = do
        m <- tryGet k
        case m of
            Just v -> pure (reverse (v : acc))
            Nothing -> go acc ks

-- | The first non-empty value in the list.
listToMaybeEnv :: [Text] -> Maybe Text
listToMaybeEnv [] = Nothing
listToMaybeEnv (v : vs)
    | T.null v = listToMaybeEnv vs
    | otherwise = Just v

tryGet :: String -> IO (Maybe Text)
tryGet key = (Just . T.pack <$> getEnv key) `catch` \(_ :: IOException) -> pure Nothing

resolveStyle :: Maybe Text -> IO OutputStyle
resolveStyle (Just mode)
    | mode == "always" = pure Color
    | mode == "never" = pure Plain
    | mode == "auto" = autoStyle
    | otherwise = die ("unknown --color mode '" <> T.unpack mode <> "'; use auto, always or never")
resolveStyle Nothing = autoStyle

autoStyle :: IO OutputStyle
autoStyle = do
    tty <- hIsTerminalDevice stdout
    noColor <- isSet "NO_COLOR"
    term <- fromMaybe "" <$> tryGet "TERM"
    pure (if tty && not noColor && term /= "dumb" then Color else Plain)
  where
    isSet key = (/= Nothing) <$> tryGet key

{- | The markdown subset headings, as (normalized name, line index)
pairs for ## sections. Subsection headings (###) are not sections.
-}
sections :: Text -> [(Text, Int)]
sections = go 0 . T.lines
  where
    go :: Int -> [Text] -> [(Text, Int)]
    go _ [] = []
    go i (l : ls)
        | "## " `T.isPrefixOf` l = (normalize l, i) : go (i + 1) ls
        | otherwise = go (i + 1) ls

{- | Extract one ## section (including its heading) from the manual.
An exact match wins; otherwise a unique prefix match is accepted.
-}
extractSection :: Text -> Text -> Either Text Text
extractSection name content =
    case candidates of
        [] -> Left (T.unlines ["no such section: " <> T.strip name, "available sections: " <> T.intercalate ", " secNames])
        [i] -> Right (extractAt i)
        _ -> Left ("ambiguous section: " <> T.strip name <> "; matches: " <> T.intercalate ", " [n | (n, j) <- secs, j `elem` candidates])
  where
    wanted = normalize name
    secs = sections content
    secNames = map fst secs
    exact = [i | (h, i) <- secs, h == wanted]
    prefix = [i | (h, i) <- secs, wanted `T.isPrefixOf` h]
    candidates = if null exact then prefix else exact
    extractAt :: Int -> Text
    extractAt i =
        let allLines = T.lines content
            stop = case [j | (_, j) <- secs, j > i] of
                (j : _) -> j
                [] -> length allLines
         in T.unlines (take (stop - i) (drop i allLines))

normalize :: Text -> Text
normalize = T.toLower . T.strip . T.dropWhile (== '#')

{- | Render the manual subset to terminal output. `Color` embeds ANSI
styling; `Plain` strips all markup. Line widths are preserved (the
source is wrapped at 80 columns).
-}
render :: OutputStyle -> Text -> Text
render style = T.unlines . go Normal . T.lines
  where
    go :: BlockState -> [Text] -> [Text]
    go _ [] = []
    go Normal (l : ls)
        | "```" `T.isPrefixOf` l = go Fence ls
        | "<!--" `T.isPrefixOf` l = skipComment ls
        | otherwise = renderLine l : go Normal ls
    go Fence (l : ls)
        | "```" `T.isPrefixOf` l = go Normal ls
        | otherwise = renderCode l : go Fence ls

    skipComment :: [Text] -> [Text]
    skipComment [] = []
    skipComment (l : ls)
        | "-->" `T.isInfixOf` l = go Normal ls
        | otherwise = skipComment ls

    renderLine :: Text -> Text
    renderLine l
        | T.null l = ""
        | isHeading l = renderHeading l
        | "- " `T.isPrefixOf` l = styleSegment "- " <> renderSegments (T.drop 2 l)
        | otherwise = renderSegments l
      where
        styleSegment t = case style of
            Color -> "\ESC[1m" <> t <> "\ESC[0m"
            Plain -> t

    isHeading :: Text -> Bool
    isHeading l = case T.uncons l of
        Just ('#', rest) -> headingTail (T.dropWhile (== '#') rest)
        _ -> False
      where
        headingTail rest = case T.uncons rest of
            Just (' ', _) -> True
            _ -> False

    renderHeading :: Text -> Text
    renderHeading l =
        let section = "## " `T.isPrefixOf` l
            text = T.strip (T.dropWhile (== '#') l)
         in case style of
                Color -> if section then "\ESC[1m\ESC[4m" <> text <> "\ESC[0m" else "\ESC[1m" <> text <> "\ESC[0m"
                Plain -> if section then T.toUpper text else text

    renderCode :: Text -> Text
    renderCode l = case style of
        Color -> "\ESC[2m" <> l <> "\ESC[0m"
        Plain -> "    " <> l

    renderSegments :: Text -> Text
    renderSegments t = mconcat (map renderSegment (tokenize t))

    renderSegment :: Segment -> Text
    renderSegment (SegText t) = t
    renderSegment (SegBold t) = case style of
        Color -> "\ESC[1m" <> t <> "\ESC[0m"
        Plain -> t
    renderSegment (SegCode t) = case style of
        Color -> "\ESC[36m" <> t <> "\ESC[0m"
        Plain -> t
    renderSegment (SegLink label url) = case style of
        Color -> "\ESC[4m" <> label <> "\ESC[0m\ESC[2m (" <> url <> ")\ESC[0m"
        Plain -> label <> " (" <> url <> ")"

{- | Split a line into inline segments, honoring the subset grammar:
**bold**, `code`, and [label](url). Unclosed markers are left as text.
-}
tokenize :: Text -> [Segment]
tokenize t = case firstMarker t of
    Nothing -> [SegText t]
    Just (pre, after, segment) -> SegText pre : segment : tokenize after
  where
    firstMarker :: Text -> Maybe (Text, Text, Segment)
    firstMarker s =
        let hits = [hit | Just hit <- [tryBold s, tryCode s, tryLink s]]
         in if null hits
                then Nothing
                else Just (minimumBy (comparing (T.length . (\(pre, _, _) -> pre))) hits)

    tryBold :: Text -> Maybe (Text, Text, Segment)
    tryBold s = case T.breakOn "**" s of
        (pre, rest)
            | T.null rest -> Nothing
            | otherwise -> case T.breakOn "**" (T.drop 2 rest) of
                (bold, rest')
                    | T.null rest' -> Nothing
                    | otherwise -> Just (pre, T.drop 2 rest', SegBold bold)

    tryCode :: Text -> Maybe (Text, Text, Segment)
    tryCode s = case T.breakOn "`" s of
        (pre, rest)
            | T.null rest -> Nothing
            | otherwise -> case T.breakOn "`" (T.drop 1 rest) of
                (code, rest')
                    | T.null rest' -> Nothing
                    | otherwise -> Just (pre, T.drop 1 rest', SegCode code)

    tryLink :: Text -> Maybe (Text, Text, Segment)
    tryLink s = case T.breakOn "[" s of
        (pre, rest)
            | T.null rest -> Nothing
            | otherwise -> case T.breakOn "](" (T.drop 1 rest) of
                (label, rest')
                    | T.null rest' -> Nothing
                    | otherwise -> case T.breakOn ")" (T.drop 2 rest') of
                        (url, rest'')
                            | T.null rest'' -> Nothing
                            | otherwise -> Just (pre, T.drop 1 rest'', SegLink label url)

die :: String -> IO a
die msg = do
    hPutStrLn stderr ("burogu doc: " <> msg)
    exitFailure
