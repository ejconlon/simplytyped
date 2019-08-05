module SimplyTyped.Type where

import qualified Data.Sequence as Seq
import SimplyTyped.Back
import SimplyTyped.Blanks
import SimplyTyped.Parts
import SimplyTyped.Prelude

data InferEnv m i e =
  InferEnv
    { inferEnvRead :: i -> m e
    , inferEnvPush :: e -> InferEnv m i e
    , inferEnvPeek :: Int -> m e
    }

mkInferEnv :: Applicative m => (i -> m e) -> (Int -> m e) -> InferEnv m i e
mkInferEnv onRead onMissing = go Seq.empty where
  go s = InferEnv onRead (\e -> go (e :<| s)) (\i -> maybe (onMissing i) pure (Seq.lookup i s))

tyTy, unitTy :: ExpScope
tyTy = wrapScoped (ExpTyTy TyTy)

unitTy = wrapScoped (ExpUnitTy UnitTy)

infer :: Applicative m => InferEnv m Identifier ExpScope -> ScopedFold ExpScope (m ExpScope)
infer env = ScopeFold bound free binder embed
  where
    bound (BoundScope b) = inferEnvPeek env b
    free (FreeScope a) = inferEnvRead env a
    binder (BinderScope i (BindInfo c n t) s) =
      case c of
        BindConPi ->
          let env' = inferEnvPush env t
              msty = foldScoped (infer env') s
           in reviewBinderScoped . BinderScope i (BindInfo BindConPiTy n t) <$> msty
        _ -> pure tyTy
    embed (EmbedScope fe) =
      case fe of
        ExpTyTy _ -> pure tyTy
        ExpUnitTm _ -> pure unitTy
        ExpUnitTy _ -> pure tyTy
        ExpProdTm (ProdTm e1 e2) ->
          let mt1 = foldScoped (infer env) e1
              mt2 = foldScoped (infer env) e2
           in (\t1 t2 -> wrapScoped (ExpProdTy (ProdTy t1 t2))) <$> mt1 <*> mt2
        ExpProdTy _ -> pure tyTy

data EnvFreeError a = EnvFreeError a
  deriving (Generic, Eq, Show, Typeable)

instance (Show a, Typeable a) => Exception (EnvFreeError a)

data EnvBoundError = EnvBoundError Int
  deriving (Generic, Eq, Show, Typeable)

instance Exception EnvBoundError

closedInferEnv :: MonadThrow m => InferEnv m Identifier ExpScope
closedInferEnv = mkInferEnv (throwM . EnvFreeError) (throwM . EnvBoundError)

closedInfer :: MonadThrow m => ScopedFold ExpScope (m ExpScope)
closedInfer = infer closedInferEnv

data CheckEnv m i e = CheckEnv
  { checkEnvRead :: i -> m (e, CheckEnv m i e)
  , checkEnvPush :: e -> CheckEnv m i e
  , checkEnvPeek :: Int -> m (e, CheckEnv m i e)
  }

-- check :: Monad m => CheckEnv m Identifier ExpScope -> ExpScope -> ScopedFold ExpScope (m Bool)
-- check env ty = ScopeFold bound free binder embed where
--   bound (BoundScope b) = do
--     (t, env') <- checkEnvPeek env b
--     foldScoped (check env' ty) t
--   free (FreeScope a) = do
--     (t, env') <- checkEnvRead env a
--     foldScoped (check env' ty) t
--   binder (BinderScope i (BindInfo c n t) s) =
--     case c of
--       BindConLam -> undefined
--       _ -> pure (ty == tyTy)
--   embed (EmbedScope fe) =
--     case fe of
--       ExpTyTy _ -> pure (ty == tyTy)
--       ExpUnitTm _ -> pure (ty == unitTy)
--       ExpUnitTy _ -> pure (ty == tyTy)
--       ExpProdTm (ProdTm e1 e2) -> do
--         t <- foldScoped (check env) e1
--         t2 = foldScoped (eheck env) e2
--            (\t1 t2 -> wrapScoped (ExpProdTy (ProdTy t1 t2))) <$> mt1 <*> mt2
--       ExpProdTy _ -> pure (ty == tyTy)
