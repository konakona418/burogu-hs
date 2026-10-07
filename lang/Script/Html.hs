{- | Structured HTML. Elements are ordinary values built by the tag
constructors (@div@, @h1@, @a@, ...); the renderer escapes text and
attribute values and emits well-formed markup. @raw@ injects an
already-rendered fragment (an embedded script, or a page body produced
by the host) without escaping.
-}
module Script.Html (
    htmlBuiltins,
    renderHtml,
    renderValue,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Script.Value (Html (..), LangError (..), Value (..), numericToText, showValue)

-- | The tag constructors, @el@ and @raw@, as bindings for the initial
-- environment.
htmlBuiltins :: [(Text, Value)]
htmlBuiltins = [("raw", VNative "raw" rawFn), ("el", VNative "el" elFn)] <> [(name, VNative name (elemFn name)) | name <- tags]

rawFn :: [Value] -> Either LangError (Value, [Text])
rawFn [VStr s] = Right (VHtml (HRaw s), [])
rawFn args = Left (arityErr "raw" 1 args)

-- | @(el name attrs child ...)@.
elFn :: [Value] -> Either LangError (Value, [Text])
elFn (VStr name : rest) = Right (VHtml (HElem name attrs children), [])
  where
    (attrs, children) = splitAttrs rest
elFn args = Left (typeErr "el" "a tag name" args)

-- | @(tag attrs child ...)@. The first argument is the attribute map
-- when it is a map or nil, otherwise there are no attributes and every
-- argument is a child.
elemFn :: Text -> [Value] -> Either LangError (Value, [Text])
elemFn name args = Right (VHtml (HElem name attrs children), [])
  where
    (attrs, children) = splitAttrs args

splitAttrs :: [Value] -> ([(Text, Value)], [Value])
splitAttrs (VMap m : rest) = (m, rest)
splitAttrs (VNil : rest) = ([], rest)
splitAttrs args = ([], args)

tags :: [Text]
tags =
    [ "html", "head", "body", "title"
    , "meta", "link", "base"
    , "header", "footer", "nav", "main", "article", "section", "aside", "div", "span"
    , "h1", "h2", "h3", "h4", "h5", "h6"
    , "p", "a", "img", "br", "hr", "button", "form", "input", "label"
    , "ul", "ol", "li", "dl", "dt", "dd"
    , "table", "thead", "tbody", "tr", "th", "td"
    , "strong", "em", "b", "i", "u", "s", "small", "mark", "code", "pre", "kbd", "samp"
    , "blockquote", "q", "cite", "abbr", "time", "address"
    , "figure", "figcaption", "picture", "source", "video", "audio", "canvas"
    , "script", "style", "noscript", "template", "iframe"
    ]

arityErr :: Text -> Int -> [Value] -> LangError
arityErr name n args =
    LangError (name <> ": expected " <> T.pack (show n) <> " argument(s), got " <> T.pack (show (length args))) [] Nothing

typeErr :: Text -> Text -> [Value] -> LangError
typeErr name want args =
    LangError (name <> ": expected " <> want <> ", got " <> T.intercalate ", " (map showValue args)) [] Nothing

{- | Render a program's result. At the top level a string is passed
through raw (it is the page body the script built); inside an element,
string children are escaped. Nil becomes empty and arrays are
concatenated fragments.
-}
renderValue :: Value -> Either Text Text
renderValue v = case v of
    VHtml h -> renderHtml h
    VStr t -> pure t
    VNum n -> pure (numericToText n)
    VBool b -> pure (if b then "true" else "false")
    VNil -> pure ""
    VArr vs -> T.concat <$> mapM renderValue (V.toList vs)
    VSym s -> Left ("cannot render symbol " <> s)
    VMap _ -> Left "cannot render a map"
    VFun _ -> Left "cannot render a function"
    VNative _ _ -> Left "cannot render a function"
    VMacro _ -> Left "cannot render a macro"

renderHtml :: Html -> Either Text Text
renderHtml h = T.concat <$> goHtml h

goValue :: Value -> Either Text [Text]
goValue v = case v of
    VHtml h -> goHtml h
    VStr t -> pure [escapeHtml t]
    VNum n -> pure [numericToText n]
    VBool b -> pure [if b then "true" else "false"]
    VNil -> pure []
    VArr vs -> concat <$> mapM goValue (V.toList vs)
    VSym s -> Left ("cannot render symbol " <> s)
    VMap _ -> Left "cannot render a map"
    VFun _ -> Left "cannot render a function"
    VNative _ _ -> Left "cannot render a function"
    VMacro _ -> Left "cannot render a macro"

goHtml :: Html -> Either Text [Text]
goHtml h = case h of
    HRaw t -> pure [t]
    HElem name attrs children -> do
        attrsHtml <- T.concat <$> mapM renderAttr attrs
        childrenHtml <- T.concat . concat <$> mapM goValue children
        if name `elem` voidTags
            then pure ["<" <> name <> attrsHtml <> ">"]
            else pure ["<" <> name <> attrsHtml <> ">" <> childrenHtml <> "</" <> name <> ">"]

renderAttr :: (Text, Value) -> Either Text Text
renderAttr (k, v) = case v of
    VBool True -> pure (" " <> k)
    VBool False -> pure ""
    VNil -> pure ""
    VStr s -> pure (" " <> k <> "=\"" <> escapeHtml s <> "\"")
    VNum n -> pure (" " <> k <> "=\"" <> numericToText n <> "\"")
    other -> Left ("attribute " <> k <> ": cannot render " <> showValue other)

voidTags :: [Text]
voidTags = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

-- | Escape @& < > " '@.
escapeHtml :: Text -> Text
escapeHtml = T.concatMap escape
  where
    escape c = case c of
        '&' -> "&amp;"
        '<' -> "&lt;"
        '>' -> "&gt;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> T.singleton c
