module Eval (LangError (..), applyValueOut, evalExpr, runScript, truthy) where

import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator)
import Data.Scientific (Scientific)
import Data.Scientific qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Expr (Chunk (..), Expr (..), Op (..))
import Value (Env, FunValue (..), LangError (..), Value (..), showValue, strOf)

{- | Evaluate a whole program: every expression in order; the value of
the last one is the result. All top-level `def`s are bound up front so
functions can call each other regardless of definition order.
Collected `puts` output is returned in order. An empty program yields
nil.
-}
runScript :: Env -> [Expr] -> Either LangError (Value, [Text])
runScript env0 exprs = evalSeq env1 body
  where
    defs = [(n, ps, b) | EDef n ps b <- exprs]
    env1 = foldl bind env0 defs
    bind env (n, ps, b) = Map.insert n (VFun (FunValue (Just n) ps b env1)) env
    body = [e | e <- exprs, not (isDef e)]
    isDef (EDef _ _ _) = True
    isDef _ = False

-- | Evaluate a sequence of expressions; the last one is the value.
evalSeq :: Env -> [Expr] -> Either LangError (Value, [Text])
evalSeq _ [] = Right (VNil, [])
evalSeq env [e] = evalExpr env [] e
evalSeq env (e : rest) = do
    (_, out) <- evalExpr env [] e
    (v, out') <- evalSeq env rest
    pure (v, out <> out')

evalExpr :: Env -> [Text] -> Expr -> Either LangError (Value, [Text])
evalExpr env out e = case e of
    ENumber n -> case reads (T.unpack n) :: [(Scientific, String)] of
        [(s, "")] -> pure (VNum s, out)
        _ -> Left (errAt ("invalid number literal " <> n))
    EString chunks -> do
        (parts, outs) <- evalChunks env chunks
        pure (VStr (T.concat parts), out <> outs)
    EBool b -> pure (VBool b, out)
    ENil -> pure (VNil, out)
    EIdent name -> case Map.lookup name env of
        Just v -> pure (v, out)
        Nothing -> Left (errAt ("undefined variable '" <> name <> "'"))
    EArray es -> do
        (vs, out') <- evalMany env out es
        pure (VArr (V.fromList vs), out')
    EMap kvs -> evalMap env out kvs
    ECall f args -> evalCall env out f args
    ELambda ps body -> pure (VFun (FunValue Nothing ps body env), out)
    EDef name ps body ->
        let fun = VFun (FunValue (Just name) ps body env)
         in pure (fun, out)
    EIf c t mf -> do
        (cv, out') <- evalExpr env out c
        if truthy cv
            then evalExpr env out' t
            else case mf of
                Just f -> evalExpr env out' f
                Nothing -> pure (VNil, out')
    EAnd a b -> do
        (av, out') <- evalExpr env out a
        if truthy av then evalExpr env out' b else pure (av, out')
    EOr a b -> do
        (av, out') <- evalExpr env out a
        if truthy av then pure (av, out') else evalExpr env out' b
    ENot a -> do
        (av, out') <- evalExpr env out a
        pure (VBool (not (truthy av)), out')
    EBinOp op a b -> evalBinOp env out op a b

truthy :: Value -> Bool
truthy v = case v of
    VBool b -> b
    VNil -> False
    _ -> True

evalMany :: Env -> [Text] -> [Expr] -> Either LangError ([Value], [Text])
evalMany env out = go out
  where
    go o [] = pure ([], o)
    go o (x : xs) = do
        (v, o') <- evalExpr env o x
        (vs, o'') <- go o' xs
        pure (v : vs, o'')

evalMap :: Env -> [Text] -> [(Expr, Expr)] -> Either LangError (Value, [Text])
evalMap env out kvs = go Map.empty out kvs
  where
    go acc o [] = pure (VMap acc, o)
    go acc o ((k, v) : rest) = do
        (kv, o') <- evalExpr env o k
        key <- case strOf kv of
            Right k' -> pure k'
            Left err -> Left (errAt ("map key: " <> err))
        (vv, o'') <- evalExpr env o' v
        go (Map.insert key vv acc) o'' rest

evalCall :: Env -> [Text] -> Expr -> [Expr] -> Either LangError (Value, [Text])
evalCall env out f args = do
    (fv, out') <- evalExpr env out f
    (avs, out'') <- evalMany env out' args
    apply out'' fv avs

apply :: [Text] -> Value -> [Value] -> Either LangError (Value, [Text])
apply out (VNative name fn) args = case fn args of
    Left err -> Left err{leStack = leStack err <> [name]}
    Right (v, o) -> pure (v, out <> o)
apply out (VFun (FunValue nm ps body env0)) args =
    case applyFun nm ps body env0 args of
        Left e -> Left e
        Right (v, o) -> pure (v, out <> o)
apply _ v _ = Left (errAt ("not callable: " <> showValue v))

{- | Apply any callable value (closure or builtin) to arguments,
propagating call-stack entries and collected `puts` output.
-}
applyValueOut :: Env -> Value -> [Value] -> Either LangError (Value, [Text])
applyValueOut _ fn args = apply [] fn args

applyFun :: Maybe Text -> [Text] -> [Expr] -> Env -> [Value] -> Either LangError (Value, [Text])
applyFun nm ps body env0 args
    | length args /= length ps =
        Left (errAt ("expected " <> T.pack (show (length ps)) <> " argument(s), got " <> T.pack (show (length args))))
    | otherwise =
        let params = Map.fromList (zip ps args)
            env1 = Map.union params env0
            env2 = maybe env1 (\n -> Map.insert n (VFun (FunValue nm ps body env0)) env1) nm
         in case evalSeq env2 body of
                Left e -> Left e{leStack = leStack e <> [callName nm]}
                Right r -> Right r
  where
    callName = fromMaybe "<lambda>"

evalBinOp :: Env -> [Text] -> Op -> Expr -> Expr -> Either LangError (Value, [Text])
evalBinOp env out op a b = do
    (av, out') <- evalExpr env out a
    (bv, out'') <- evalExpr env out' b
    evalOp av bv out''
  where
    evalOp x y o = case op of
        Add -> case (x, y) of
            (VNum m, VNum n) -> pure (VNum (m + n), o)
            (VStr s, VStr t) -> pure (VStr (s <> t), o)
            _ -> Left (typeErr "add" "number or string" x y)
        Sub -> numBin (-) x y o
        Mul -> numBin (*) x y o
        Div -> case (x, y) of
            (VNum m, VNum n)
                | n == 0 -> Left (errAt "division by zero")
                | otherwise -> pure (VNum (divSci m n), o)
            _ -> Left (typeErr "divide" "number" x y)
        Mod -> case (x, y) of
            (VNum m, VNum n)
                | n == 0 -> Left (errAt "division by zero")
                | otherwise -> pure (VNum (modSci m n), o)
            _ -> Left (typeErr "modulo" "number" x y)
        Eq -> pure (VBool (x == y), o)
        Ne -> pure (VBool (x /= y), o)
        Lt -> cmp (<) x y o
        Gt -> cmp (>) x y o
        Le -> cmp (<=) x y o
        Ge -> cmp (>=) x y o
        And -> Left (errAt "internal: && is a short-circuit node")
        Or -> Left (errAt "internal: || is a short-circuit node")

    numBin :: (Scientific -> Scientific -> Scientific) -> Value -> Value -> [Text] -> Either LangError (Value, [Text])
    numBin f (VNum m) (VNum n) o = pure (VNum (f m n), o)
    numBin _ _ _ _ = Left (typeErr "operate" "number" (VNum 0) (VNum 0))

    cmp :: (Scientific -> Scientific -> Bool) -> Value -> Value -> [Text] -> Either LangError (Value, [Text])
    cmp f (VNum m) (VNum n) o = pure (VBool (f m n), o)
    cmp _ _ _ _ = Left (typeErr "compare" "number" (VNum 0) (VNum 0))

    typeErr what want x y = errAt ("cannot " <> what <> ": expected " <> want <> ", got " <> showValue x <> " and " <> showValue y)

{- | Division that stays exact for whole results and falls back to a
Double (finite) otherwise; Scientific's own `fromRational` rejects
repeating decimals.
-}
divSci :: Scientific -> Scientific -> Scientific
divSci m n =
    let r = toRational m / toRational n
     in if denominator r == 1
            then fromRational r
            else fromRational (toRational ((S.toRealFloat m / S.toRealFloat n) :: Double))

modSci :: Scientific -> Scientific -> Scientific
modSci m n =
    let q = truncate (toRational m / toRational n) :: Integer
     in m - fromRational (toRational q) * n

evalChunks :: Env -> [Chunk] -> Either LangError ([Text], [Text])
evalChunks env = go []
  where
    go acc [] = pure (reverse acc, [])
    go acc (c : rest) = case c of
        SLit t -> do
            (parts, outs) <- go (t : acc) rest
            pure (parts, outs)
        SInterp e -> do
            (v, outs1) <- evalExpr env [] e
            t <- either (Left . errAt) Right (strOf v)
            (parts, outs2) <- go (t : acc) rest
            pure (parts, outs1 <> outs2)

errAt :: Text -> LangError
errAt msg = LangError msg []
