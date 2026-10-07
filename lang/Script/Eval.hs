{- | The evaluator. A program is a sequence of top-level forms; `defn`
forms are pre-bound so functions can call each other in any order, then
the remaining forms are evaluated in order and the value of the last
one is the program's result. `puts` output is collected in order.

Special forms: @quote@, @fn@ (alias @lambda@), @if@, @let@, @do@,
@and@, @or@. Definitions (@def@, @defn@) are only allowed at the top
level; inside bodies use @let@ and @fn@.

A top-level @defn@ closure captures the environment of all top-level
@defn@ bindings (so recursion and mutual recursion work), plus the
host globals. A top-level @def@ value is visible to later top-level
forms but not inside earlier @defn@ bodies; pass values as parameters
when a function needs them.
-}
module Script.Eval (
    applyValue,
    evalForm,
    evalProgram,
    quoteForm,
    truthy,
) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Script.Form (Form (..), Node (..), Pos)
import Script.Value (Env, Fun (..), LangError (..), Macro (..), Value (..), showValue)

-- | Evaluate a whole program with the given global environment.
evalProgram :: Env -> [Form] -> Either LangError (Value, [Text])
evalProgram env0 forms = go env1 forms
  where
    defns =
        [ (name, params, body)
        | Form _ (NList (Form _ (NSym "defn") : Form _ (NSym name) : Form _ (NList params') : body)) <- forms
        , let params = map symName params'
        ]
    macros =
        [ (name, params, body)
        | Form _ (NList (Form _ (NSym "defmacro") : Form _ (NSym name) : Form _ (NList params') : body)) <- forms
        , let params = map symName params'
        ]
    env1 = foldl insertFun (foldl insertMacro env0 macros) defns
    insertFun env (name, params, body) = Map.insert name (VFun (Fun (Just name) params body env1)) env
    insertMacro env (name, params, body) = Map.insert name (VMacro (Macro (Just name) params body env1)) env

    go _ [] = Right (VNil, [])
    go env [f] = do
        (_, v, out) <- evalTop env f
        pure (v, out)
    go env (f : rest) = do
        (env', _, out) <- evalTop env f
        (v, out') <- go env' rest
        pure (v, out <> out')

-- | Evaluate one top-level form, returning the (possibly extended)
-- environment, the form's value and its `puts` output.
evalTop :: Env -> Form -> Either LangError (Env, Value, [Text])
evalTop env (Form _ (NList (Form _ (NSym "defn") : _))) = pure (env, VNil, [])
evalTop env (Form _ (NList (Form _ (NSym "defmacro") : _))) = pure (env, VNil, [])
evalTop env (Form _ (NList [Form _ (NSym "def"), Form _ (NSym name), value])) = do
    (v, out) <- evalForm env value
    pure (Map.insert name v env, VNil, out)
evalTop _ (Form pos (NList (Form _ (NSym "def") : _))) =
    Left (errAt (Just pos) "def takes a name and a value: (def name value)")
evalTop env f = do
    (v, out) <- evalForm env f
    pure (env, v, out)

{- | Evaluate one form. Returns its value and the `puts` output produced
while evaluating it.
-}
evalForm :: Env -> Form -> Either LangError (Value, [Text])
evalForm env (Form pos node) = case node of
    NNum s -> pure (VNum s, [])
    NStr t -> pure (VStr t, [])
    NBool b -> pure (VBool b, [])
    NNil -> pure (VNil, [])
    NSym name -> case Map.lookup name env of
        Just v -> pure (v, [])
        Nothing -> Left (errAt (Just pos) ("undefined variable '" <> name <> "'"))
    NArray fs -> do
        (vs, out) <- evalMany env fs
        pure (VArr (V.fromList vs), out)
    NMap kvs -> evalMap env kvs
    NList [] -> pure (VNil, [])
    NList (h : args) -> evalList env pos h args

evalList :: Env -> Pos -> Form -> [Form] -> Either LangError (Value, [Text])
evalList env pos h args = case h of
    Form _ (NSym "quote") -> case args of
        [f] -> pure (quoteForm f, [])
        _ -> Left (errAt (Just pos) "quote takes exactly one form")
    Form _ (NSym "quasiquote") -> case args of
        [f] -> evalQuasi env 1 f
        _ -> Left (errAt (Just pos) "quasiquote takes exactly one form")
    Form _ (NSym "fn") -> makeFn env Nothing pos args
    Form _ (NSym "lambda") -> makeFn env Nothing pos args
    Form _ (NSym "if") -> evalIf env pos args
    Form _ (NSym "let") -> evalLet env pos args
    Form _ (NSym "do") -> evalBody env args
    Form _ (NSym "and") -> evalAnd env args
    Form _ (NSym "or") -> evalOr env args
    Form _ (NSym "def") -> Left (errAt (Just pos) "def is only allowed at the top level")
    Form _ (NSym "defn") -> Left (errAt (Just pos) "defn is only allowed at the top level")
    Form _ (NSym "defmacro") -> Left (errAt (Just pos) "defmacro is only allowed at the top level")
    Form _ (NSym name) -> case Map.lookup name env of
        Just (VMacro m) -> expandMacro env pos m args
        _ -> evalCall env pos h args
    _ -> evalCall env pos h args

makeFn :: Env -> Maybe Text -> Pos -> [Form] -> Either LangError (Value, [Text])
makeFn env name pos args = case args of
    (Form _ (NList params) : body) -> do
        ps <- mapM paramName params
        pure (VFun (Fun name ps body env), [])
    (Form _ (NArray params) : body) -> do
        ps <- mapM paramName params
        pure (VFun (Fun name ps body env), [])
    (bad : _) -> Left (errAt (Just (formPos bad)) "fn expects a parameter list")
    [] -> Left (errAt (Just pos) "fn expects a parameter list")

paramName :: Form -> Either LangError Text
paramName (Form _ (NSym n)) = pure n
paramName (Form pos _) = Left (errAt (Just pos) "parameter must be a symbol")

symName :: Form -> Text
symName (Form _ (NSym n)) = n
symName _ = "?"

evalIf :: Env -> Pos -> [Form] -> Either LangError (Value, [Text])
evalIf env pos args = case args of
    [c, t] -> do
        (cv, out) <- evalForm env c
        if truthy cv then addOut out <$> evalForm env t else pure (VNil, out)
    [c, t, e] -> do
        (cv, out) <- evalForm env c
        (v, out') <- if truthy cv then evalForm env t else evalForm env e
        pure (v, out <> out')
    _ -> Left (errAt (Just pos) "if expects a condition, a then branch and an optional else branch")

addOut :: [Text] -> (Value, [Text]) -> (Value, [Text])
addOut out (v, o) = (v, out <> o)

evalLet :: Env -> Pos -> [Form] -> Either LangError (Value, [Text])
evalLet env pos args = case args of
    (bindings : body) -> do
        kvs <- bindingPairs pos bindings
        (env', out) <- bindAll env kvs
        (v, out') <- evalBody env' body
        pure (v, out <> out')
    [] -> Left (errAt (Just pos) "let expects a binding list and a body")
  where
    bindingPairs p (Form _ (NList fs)) = pairUp p fs
    bindingPairs p (Form _ (NArray fs)) = pairUp p fs
    bindingPairs p _ = Left (errAt (Just p) "let bindings must be a list of name/value pairs")

    bindAll e [] = pure (e, [])
    bindAll e ((nameForm, valueForm) : rest) = do
        name <- paramName nameForm
        (v, out) <- evalForm e valueForm
        (e', out') <- bindAll (Map.insert name v e) rest
        pure (e', out <> out')

pairUp :: Pos -> [Form] -> Either LangError [(Form, Form)]
pairUp pos = go
  where
    go [] = pure []
    go (k : v : rest) = ((k, v) :) <$> go rest
    go [_] = Left (errAt (Just pos) "expected an even number of forms in bindings")

evalBody :: Env -> [Form] -> Either LangError (Value, [Text])
evalBody _ [] = pure (VNil, [])
evalBody env [f] = evalForm env f
evalBody env (f : rest) = do
    (_, out) <- evalForm env f
    (v, out') <- evalBody env rest
    pure (v, out <> out')

evalAnd :: Env -> [Form] -> Either LangError (Value, [Text])
evalAnd _ [] = pure (VBool True, [])
evalAnd env [f] = evalForm env f
evalAnd env (f : rest) = do
    (v, out) <- evalForm env f
    if truthy v then addOut out <$> evalAnd env rest else pure (v, out)

evalOr :: Env -> [Form] -> Either LangError (Value, [Text])
evalOr _ [] = pure (VNil, [])
evalOr env [f] = evalForm env f
evalOr env (f : rest) = do
    (v, out) <- evalForm env f
    if truthy v then pure (v, out) else addOut out <$> evalOr env rest

evalCall :: Env -> Pos -> Form -> [Form] -> Either LangError (Value, [Text])
evalCall env pos f args = do
    (fv, out) <- evalForm env f
    (avs, out') <- evalMany env args
    case applyValue env fv avs of
        Left err -> Left (withPos pos err)
        Right (v, out'') -> pure (v, out <> out' <> out'')

evalMany :: Env -> [Form] -> Either LangError ([Value], [Text])
evalMany env = go [] []
  where
    go vs outs [] = pure (reverse vs, outs)
    go vs outs (f : rest) = do
        (v, o) <- evalForm env f
        go (v : vs) (outs <> o) rest

evalMap :: Env -> [(Form, Form)] -> Either LangError (Value, [Text])
evalMap env = go [] []
  where
    go acc outs [] = pure (VMap (reverse acc), outs)
    go acc outs ((k, v) : rest) = do
        (kv, o1) <- evalForm env k
        key <- case kv of
            VStr t -> pure t
            VSym s -> pure s
            other -> Left (errAt (Just (formPos k)) ("map key: " <> showValue other))
        (vv, o2) <- evalForm env v
        let acc' = (key, vv) : filter ((/= key) . fst) acc
        go acc' (outs <> o1 <> o2) rest

{- | Apply a callable value (closure or native) to arguments, propagating
the call stack and collected `puts` output. The caller environment
carries the current call depth, so runaway recursion fails cleanly
instead of overflowing the Haskell stack. Exposed for collection
builtins such as `map` and `filter`.
-}
applyValue :: Env -> Value -> [Value] -> Either LangError (Value, [Text])
applyValue _ (VNative name fn) args = case fn args of
    Left err -> Left err{leStack = leStack err <> [name]}
    Right (v, o) -> Right (v, o)
applyValue env (VFun f) args = applyFun env f args
applyValue _ v _ = Left (errAt Nothing ("not callable: " <> showValue v))

-- | The maximum function-call nesting depth.
maxCallDepth :: Int
maxCallDepth = 10000

-- | Reserved binding used to thread the call depth through the caller
-- environment. Users never see the environment map directly.
depthKey :: Text
depthKey = "\0depth"

applyFun :: Env -> Fun -> [Value] -> Either LangError (Value, [Text])
applyFun caller f args
    | length args /= length (funParams f) =
        Left (errAt Nothing ("expected " <> T.pack (show (length (funParams f))) <> " argument(s), got " <> T.pack (show (length args))))
    | depth >= maxCallDepth =
        Left (errAt Nothing ("recursion limit exceeded (" <> T.pack (show maxCallDepth) <> " nested calls)"))
    | otherwise =
        let params = Map.fromList (zip (funParams f) args)
            env1 = Map.insert depthKey (VNum (fromIntegral (depth + 1))) (Map.union params (funEnv f))
         in case evalBody env1 (funBody f) of
                Left e -> Left e{leStack = leStack e <> [callName f]}
                Right r -> Right r
  where
    depth = case Map.lookup depthKey caller of
        Just (VNum n) -> fromInteger (truncate n)
        _ -> 0
    callName fn = maybe "<lambda>" id (funName fn)

truthy :: Value -> Bool
truthy (VBool b) = b
truthy VNil = False
truthy _ = True

{- | Expand and evaluate a macro call: the macro receives the unevaluated
argument forms as data and returns the expansion, which is then
evaluated in the caller's environment.
-}
expandMacro :: Env -> Pos -> Macro -> [Form] -> Either LangError (Value, [Text])
expandMacro env pos m args = do
    (expansion, out) <- applyMacro m (map quoteForm args)
    form <- either (Left . errAt (Just pos)) Right (valueToForm expansion)
    (v, out') <- evalForm env form
    pure (v, out <> out')

applyMacro :: Macro -> [Value] -> Either LangError (Value, [Text])
applyMacro m args
    | length args /= length (macroParams m) =
        Left (errAt Nothing (name <> ": expected " <> T.pack (show (length (macroParams m))) <> " argument(s), got " <> T.pack (show (length args))))
    | otherwise =
        let env1 = Map.union (Map.fromList (zip (macroParams m) args)) (macroEnv m)
         in evalBody env1 (macroBody m)
  where
    name = maybe "<macro>" id (macroName m)

{- | Quasiquote: like quote, but `(unquote e)` evaluates e and
`(unquote-splicing e)` splices the elements of the array e into the
surrounding list. A nested quasiquote raises the depth, so only
depth-1 unquotes fire.
-}
evalQuasi :: Env -> Int -> Form -> Either LangError (Value, [Text])
evalQuasi env depth form = case formNode form of
    NList [Form _ (NSym "unquote"), e]
        | depth == 1 -> evalForm env e
        | otherwise -> do
            (v, out) <- evalQuasi env (depth - 1) e
            pure (VArr (V.fromList [VSym "unquote", v]), out)
    NList (Form _ (NSym "quasiquote") : rest) -> do
        (v, out) <- evalQuasi env (depth + 1) (Form (formPos form) (NList rest))
        pure (VArr (V.fromList [VSym "quasiquote", v]), out)
    NList fs -> do
        (vs, out) <- collect env depth fs
        pure (VArr (V.fromList vs), out)
    NArray fs -> do
        (vs, out) <- collect env depth fs
        pure (VArr (V.fromList vs), out)
    NMap kvs -> do
        (pairs, out) <- mapPairs kvs
        pure (VMap pairs, out)
    _ -> pure (quoteForm form, [])
  where
    collect _ _ [] = pure ([], [])
    collect e d (f : rest) = case formNode f of
        NList [Form _ (NSym "unquote-splicing"), inner] | d == 1 -> do
            (v, o) <- evalForm e inner
            vs <- case v of
                VArr xs -> pure (V.toList xs)
                VNil -> pure []
                other -> Left (errAt Nothing ("unquote-splicing expects an array, got " <> showValue other))
            (more, o') <- collect e d rest
            pure (vs <> more, o <> o')
        _ -> do
            (v, o) <- evalQuasi e d f
            (more, o') <- collect e d rest
            pure (v : more, o <> o')

    mapPairs [] = pure ([], [])
    mapPairs ((k, v) : rest) = do
        (kv, o1) <- evalQuasi env depth k
        key <- case kv of
            VStr t -> pure t
            VSym s -> pure s
            other -> Left (errAt (Just (formPos k)) ("map key: " <> showValue other))
        (vv, o2) <- evalQuasi env depth v
        (more, o3) <- mapPairs rest
        pure ((key, vv) : more, o1 <> o2 <> o3)

-- | Convert macro-produced data back into a form to evaluate.
valueToForm :: Value -> Either Text Form
valueToForm v = case v of
    VNum s -> pure (Form (0, 0) (NNum s))
    VStr t -> pure (Form (0, 0) (NStr t))
    VBool b -> pure (Form (0, 0) (NBool b))
    VNil -> pure (Form (0, 0) NNil)
    VSym s -> pure (Form (0, 0) (NSym s))
    VArr vs -> Form (0, 0) . NList <$> mapM valueToForm (V.toList vs)
    VMap m -> Form (0, 0) . NMap <$> mapM kv m
    VHtml _ -> Left "a macro expansion cannot contain html"
    VFun _ -> Left "a macro expansion cannot contain a function"
    VNative _ _ -> Left "a macro expansion cannot contain a function"
    VMacro _ -> Left "a macro expansion cannot contain a macro"
  where
    kv (k, val) = (,) (Form (0, 0) (NStr k)) <$> valueToForm val

-- | Convert a form into data (used by `quote` and, later, macros).
quoteForm :: Form -> Value
quoteForm (Form _ node) = case node of
    NNum s -> VNum s
    NStr t -> VStr t
    NBool b -> VBool b
    NNil -> VNil
    NSym s -> VSym s
    NList fs -> VArr (V.fromList (map quoteForm fs))
    NArray fs -> VArr (V.fromList (map quoteForm fs))
    NMap kvs -> VMap [(key, quoteForm v) | (k, v) <- kvs, Right key <- [keyOf (quoteForm k)]]
  where
    keyOf (VStr t) = Right t
    keyOf (VSym s) = Right s
    keyOf other = Left other

withPos :: Pos -> LangError -> LangError
withPos pos e = case lePos e of
    Just _ -> e
    Nothing -> e{lePos = Just pos}

errAt :: Maybe Pos -> Text -> LangError
errAt pos msg = LangError msg [] pos
