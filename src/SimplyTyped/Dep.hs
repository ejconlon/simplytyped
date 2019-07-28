{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Dep where

import Control.Newtype.Generics (Newtype)
import SimplyTyped.Deriving.Enum
import SimplyTyped.Deriving.Sum
import SimplyTyped.Deriving.Wrapper
import SimplyTyped.Lenses
import SimplyTyped.Parts
import SimplyTyped.Prelude
import SimplyTyped.Sub
import SimplyTyped.Tree

data BindCon =
    BindConPi
  | BindConSigma
  deriving (Generic, Eq, Show, Ord, Bounded, Enum)
  deriving (Treeable) via (EnumWrapperTreeable BindCon)

instance EnumWrapper BindCon where
  enumRefTree _ = "bindCon"
  enumToValueKeyword v =
    case v of
      BindConPi -> "pi"
      BindConSigma -> "sigma"
  enumFromValueKeyword _ k =
    case k of
      "pi" -> Just BindConPi
      "sigma" -> Just BindConSigma
      _ -> Nothing

data BindInfo a = BindInfo { bindInfoCon :: BindCon, bindInfoType :: a } deriving (Generic, Eq, Show)

instance Treeable a => Treeable (BindInfo a) where
  refTree _ = "bindInfo"
  defineTree _ = BranchDef (BranchFixed [RefDef (refTree (Proxy :: Proxy BindCon)), RefDef (refTree (Proxy :: Proxy a))])
  depsTree _ =
    mergeDepTrees
      [ selfDepsTree (Proxy :: Proxy BindCon)
      , selfDepsTree (Proxy :: Proxy a)
      ]
  parseTree _ t =
    case t of
      Branch [tx, ty] -> do
        x <- parseTree (Proxy :: Proxy BindCon) tx
        y <- parseTree (Proxy :: Proxy a) ty
        pure (BindInfo x y)
      _ -> parseFail
  renderTree (BindInfo x y) =
    Branch [renderTree x, renderTree y]

data Exp a =
      ExpUnit UnitExp
    | ExpUnitTy UnitTy
    | ExpProd (ProdExp a)
    | ExpProdTy (ProdTy a)
    -- | ExpPiTy (PiTy a)
    -- | ExpSigmaTy (SigmaTy a)
    -- | ExpRewrite (RewriteExp a)
    -- | ExpRefl (ReflExp a)
    -- | ExpEqTy (EqTy a)
    deriving (Generic, Eq, Show)

$(makePrisms ''Exp)

deriving via (SumWrapperTreeable (Exp a)) instance Treeable a => Treeable (Exp a)

instance Treeable a => SumWrapper (Exp a) where
  sumRefTree _ = "exp"
  sumTreeInjs _ =
    [ TreeInj (Proxy :: Proxy UnitExp) _ExpUnit
    , TreeInj (Proxy :: Proxy UnitTy) _ExpUnitTy
    , TreeInj (Proxy :: Proxy (ProdExp a)) _ExpProd
    , TreeInj (Proxy :: Proxy (ProdTy a)) _ExpProdTy
    ]

newtype ExpScope = ExpScope { unExpScope :: Scope (BindInfo ExpScope) Exp Identifier }
  deriving (Generic, Eq, Show)
  deriving (Treeable) via TreeWrapperTreeable ExpScope

instance Newtype ExpScope

instance TreeWrapper ExpScope where
  wrapRefTree _ = "expScope"
  wrapConTree _ = "expScope"

data OfTyJudge a = OfTyJudge a a deriving (Generic, Eq, Show)

data IsTyJudge a = IsTyJudge a deriving (Generic, Eq, Show)

data Judge a =
      JudgeOfTy (OfTyJudge a)
    | JudgeIsTy (IsTyJudge a)
    deriving (Generic, Eq, Show)

-- data Term a =
--       TermExp (Exp a)
--     | TermJudge (Judge a)
--     deriving (Generic, Eq, Show)

-- newtype TermScope = TermScope (Scope (Bind TermScope) Term Identifier) deriving (Eq, Show)
