{- | Tests for the script language: reader, evaluator, builtins and the
structured HTML renderer.
-}
module ScriptSpec (scriptSpec) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (isJust)
import Script (LangError (..), evalText, formatError, initialEnv, renderResult)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

scriptSpec :: TestTree
scriptSpec =
    testGroup
        "Lisp"
        [ testCase "literals and arithmetic" $ do
            eval "42" @?= Right "42"
            eval "(- 10 4)" @?= Right "6"
            eval "(* 6 7)" @?= Right "42"
            eval "(+ 1.5 2.5)" @?= Right "4"
        , testCase "strings and escaping" $ do
            eval "(str \"a\" \"b\")" @?= Right "ab"
            eval "(p \"a < b & c\")" @?= Right "<p>a &lt; b &amp; c</p>"
        , testCase "elements and attributes" $ do
            eval "(div {:class \"x\"} \"hi\")" @?= Right "<div class=\"x\">hi</div>"
            eval "(link {:rel \"stylesheet\" :href \"/s.css\"})" @?= Right "<link rel=\"stylesheet\" href=\"/s.css\">"
            eval "(div {:z \"1\" :a \"2\"})" @?= Right "<div z=\"1\" a=\"2\"></div>"
            eval "(input {:disabled true :value \"x\"})" @?= Right "<input disabled value=\"x\">"
            eval "(input {:disabled false :value \"x\"})" @?= Right "<input value=\"x\">"
            eval "(br)" @?= Right "<br>"
        , testCase "raw inserts unescaped" $
            eval "(raw \"<b>x</b>\")" @?= Right "<b>x</b>"
        , testCase "defn supports recursion and mutual recursion" $ do
            eval "(defn fact (n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 5)" @?= Right "120"
            eval "(defn even? (n) (if (== n 0) true (odd? (- n 1)))) (defn odd? (n) (if (== n 0) false (even? (- n 1)))) (even? 10)" @?= Right "true"
        , testCase "let binds sequentially" $
            eval "(let (x 2 y (* x 3)) (+ x y))" @?= Right "8"
        , testCase "def is visible to later top-level forms" $
            eval "(def x 5) (+ x 1)" @?= Right "6"
        , testCase "map, filter, reduce and lambdas" $ do
            eval "(join (map [1 2 3] (fn (x) (* x 2))) \",\")" @?= Right "2,4,6"
            eval "(join (filter [1 2 3 4] (fn (x) (== (% x 2) 0))) \",\")" @?= Right "2,4"
            eval "(reduce [1 2 3 4] (fn (acc x) (+ acc x)) 0)" @?= Right "10"
        , testCase "conditionals and boolean operators" $ do
            eval "(if true \"y\" \"n\")" @?= Right "y"
            eval "(and true 3)" @?= Right "3"
            eval "(or false \"x\")" @?= Right "x"
        , testCase "string builtins" $
            eval "(upper (trim \"  hi  \"))" @?= Right "HI"
        , testGroup
            "Macros"
            [ testCase "defmacro expands at the call site" $ do
                eval "(defmacro unless (c body) `(if ,c nil ,body)) (unless false 7)" @?= Right "7"
                eval "(defmacro unless (c body) `(if ,c nil ,body)) (unless true 7)" @?= Right ""
            , testCase "quasiquote, unquote and splicing" $
                eval "(to-json `(1 ,(+ 1 1) ,@[3 4]))" @?= Right "[\n  1,\n  2,\n  3,\n  4\n]"
            , testCase "gensym avoids capture" $
                eval "(defmacro double (x) (let (g (gensym)) `(let (,g ,x) (+ ,g ,g)))) (double 5)" @?= Right "10"
            , testCase "macros are visible inside defn bodies" $
                eval "(defmacro twice (x) `(+ ,x ,x)) (defn f (y) (twice y)) (f 3)" @?= Right "6"
            , testCase "gensym returns distinct symbols" $
                eval "(== (gensym) (gensym))" @?= Right "false"
            , testCase "runaway recursion fails cleanly" $
                case evalText initialEnv "(defn loop (n) (loop (+ n 1))) (loop 0)" of
                    Left e -> assertBool "limit" (T.isInfixOf "recursion limit exceeded" (formatError e))
                    Right _ -> assertEqual "expected a failure" True False
            ]
        , testCase "runtime errors carry a source position" $
            case evalText initialEnv "(defn f (x) (g x)) (f 1)" of
                Left e -> assertBool "has a position" (isJust (lePos e))
                Right _ -> assertEqual "expected a failure" True False
        ]

eval :: Text -> Either Text Text
eval src = case evalText initialEnv src of
    Left e -> Left (formatError e)
    Right (v, _) -> renderResult v
