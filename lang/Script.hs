{- | The script language: a small dynamic Lisp used to generate HTML. The
host compiles source once and evaluates it against an environment it
populates with domain bindings; the result is rendered to escaped
HTML. Built-in Lisp is provided by "Script.Builtins"; the host adds its
own natives on top.
-}
module Script (
    Env,
    LangError (..),
    Program,
    Value (..),
    compile,
    evalText,
    formatError,
    initialEnv,
    renderResult,
    resetGensym,
    runProgram,
    truthy,
) where

import Data.Text (Text)
import Data.Text qualified as T

import Script.Builtins (initialEnv, resetGensym)
import Script.Eval (evalProgram, truthy)
import Script.Form (Form)
import Script.Html (renderValue)
import Script.Reader (readProgram)
import Script.Value (Env, LangError (..), Value (..))

type Program = [Form]

-- | Parse source into a program, or fail with reader errors.
compile :: Text -> Either [Text] Program
compile src = case readProgram src of
    Left err -> Left [err]
    Right forms -> Right forms

-- | Evaluate a compiled program against an environment.
runProgram :: Env -> Program -> Either LangError (Value, [Text])
runProgram = evalProgram

-- | Compile and evaluate source in one step.
evalText :: Env -> Text -> Either LangError (Value, [Text])
evalText env src = case compile src of
    Left errs -> Left (LangError (T.intercalate "\n" errs) [] Nothing)
    Right forms -> runProgram env forms

-- | Render a program result to HTML.
renderResult :: Value -> Either Text Text
renderResult = renderValue

-- | A one-line rendering of a runtime error, with position and a capped
-- call stack.
formatError :: LangError -> Text
formatError e =
    posPart <> leMsg e <> stackPart
  where
    posPart = maybe "" (\(l, c) -> "line " <> T.pack (show l) <> ", column " <> T.pack (show c) <> ": ") (lePos e)
    stackPart = case leStack e of
        [] -> ""
        frames -> " [" <> T.intercalate ", " (shorten frames) <> "]"

    -- Deep recursion can make the stack huge; show the top and the root.
    shorten frames
        | length frames <= 12 = frames
        | otherwise =
            take 10 frames
                <> ["… " <> T.pack (show (length frames - 11)) <> " more"]
                <> [last frames]
