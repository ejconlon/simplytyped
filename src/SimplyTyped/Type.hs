module SimplyTyped.Type where

import SimplyTyped.Back
import SimplyTyped.Blanks
import SimplyTyped.Parts
import SimplyTyped.Prelude

data Env m i e =
  Env
    { envRead :: i -> m e
    }

-- Idea: don't rewrite the term, instead allow lookup of bound vars and support shifting.
-- Will require a local-equivalent to increment bound refs
tyTy, unitTy :: ExpScope
tyTy = wrapScoped (ExpTyTy TyTy)

unitTy = wrapScoped (ExpUnitTy UnitTy)

infer :: Applicative m => (Identifier -> m ExpScope) -> ScopedFold ExpScope (m ExpScope)
infer readEnv = ScopeFold bound free binder embed
  where
    bound (BoundScope b) = pure (reviewBoundScoped b)
    free (FreeScope a) = readEnv a
    binder (BinderScope i (BindInfo c n t) s) =
      case c of
        BindConLam ->
          let msty = foldScoped (infer readEnv) s
           in reviewBinderScoped . BinderScope i (BindInfo BindConPiTy n t) <$> msty
        _ -> pure tyTy
    embed (EmbedScope fe) =
      case fe of
        ExpTyTy _ -> pure tyTy
        ExpUnitTm _ -> pure unitTy
        ExpUnitTy _ -> pure tyTy
        ExpProdTm (ProdTm e1 e2) ->
          let mt1 = foldScoped (infer readEnv) e1
              mt2 = foldScoped (infer readEnv) e2
           in (\t1 t2 -> wrapScoped (ExpProdTy (ProdTy t1 t2))) <$> mt1 <*> mt2
        ExpProdTy _ -> pure tyTy

data UnfreeError a =
  UnfreeError a
  deriving (Generic, Eq, Show, Typeable)

instance (Show a, Typeable a) => Exception (UnfreeError a)

closedInfer :: MonadThrow m => ScopedFold ExpScope (m ExpScope)
closedInfer = infer (throwM . UnfreeError)
