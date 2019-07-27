{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Dep where

import Control.Newtype.Generics (Newtype)
import qualified Data.Map as Map
import SimplyTyped.Deriving.Sum
import SimplyTyped.Deriving.Wrapper
import SimplyTyped.Prelude
import SimplyTyped.Sub
import SimplyTyped.Tree

type Identifier = Text

data BindCon =
    BindConPi
  | BindConSigma
  deriving (Generic, Eq, Show)

instance Treeable BindCon where
  refTree _ = "bindCon"
  defineTree _ =
    ChoiceDef
      [ LeafDef (LeafKeyword "pi")
      , LeafDef (LeafKeyword "sigma")
      ]
  depsTree _ = Map.empty
  parseTree _ t =
    case t of
      Leaf n ->
        case n of
          "pi" -> pure BindConPi
          "sigma" -> pure BindConSigma
          _ -> parseFail
      _ -> parseFail
  renderTree t =
    case t of
      BindConPi -> Leaf "pi"
      BindConSigma -> Leaf "sigma"

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

data UnitExp = UnitExp deriving (Generic, Eq, Show)

instance Treeable UnitExp where
  refTree _ = "unitExp"
  defineTree _ = LeafDef (LeafKeyword "unit")
  depsTree _ = Map.empty
  parseTree _ t =
    case t of
      Leaf "unit" -> pure UnitExp
      _ -> empty
  renderTree _ = Leaf "unit"

data UnitTy = UnitTy deriving (Generic, Eq, Show)

instance Treeable UnitTy where
  refTree _ = "unitTy"
  defineTree _ = LeafDef (LeafKeyword "Unit")
  depsTree _ = Map.empty
  parseTree _ t =
    case t of
      Leaf "Unit" -> pure UnitTy
      _ -> empty
  renderTree _ = Leaf "Unit"

data ProdExp a = ProdExp a a deriving (Generic, Eq, Show)

instance Treeable a => Treeable (ProdExp a) where
  refTree _ = "prodExp"
  defineTree _ =
    let nestRef = refTree (Proxy :: Proxy a)
    in BranchDef (BranchFixed [LeafDef (LeafKeyword "prod"), RefDef nestRef, RefDef nestRef])
  depsTree _ = selfDepsTree (Proxy :: Proxy a)
  parseTree _ t =
    case t of
      Branch [Leaf "prod", l, r] -> ProdExp <$> parseTree (Proxy :: Proxy a) l <*> parseTree (Proxy :: Proxy a) r
      _ -> empty
  renderTree (ProdExp l r) = Branch [Leaf "prod", renderTree l, renderTree r]

data ProdTy a = ProdTy a a deriving (Generic, Eq, Show)

instance Treeable a => Treeable (ProdTy a) where
  refTree _ = "prodTy"
  defineTree _ =
    let nestRef = refTree (Proxy :: Proxy a)
    in BranchDef (BranchFixed [LeafDef (LeafKeyword "Prod"), RefDef nestRef, RefDef nestRef])
  depsTree _ = selfDepsTree (Proxy :: Proxy a)
  parseTree _ t =
    case t of
      Branch [Leaf "Prod", l, r] -> ProdTy <$> parseTree (Proxy :: Proxy a) l <*> parseTree (Proxy :: Proxy a) r
      _ -> empty
  renderTree (ProdTy l r) = Branch [Leaf "Prod", renderTree l, renderTree r]

-- data PiTy a = PiTy a a deriving (Generic, Eq, Show)

-- data SigmaTy a = SigmaTy a a deriving (Generic, Eq, Show)

-- data RewriteExp a = RewriteExp a a deriving (Generic, Eq, Show)

-- data ReflExp a = ReflExp a deriving (Generic, Eq, Show)

-- data EqTy a = EqTy a a a deriving (Generic, Eq, Show)

-- TODO simplify with generic sum-of-products methods
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
    deriving (Treeable) via (SumWrapperTreeable (Exp a))

instance Treeable a => SumWrapper (Exp a) where
  sumRefTree _ = "exp"
  sumTreeInjs _ =
    [ TreeInj (Proxy :: Proxy UnitExp) (Inj ExpUnit (\case ExpUnit t -> Just t; _ -> Nothing))
    , TreeInj (Proxy :: Proxy UnitTy) (Inj ExpUnitTy (\case ExpUnitTy t -> Just t; _ -> Nothing))
    , TreeInj (Proxy :: Proxy (ProdExp a)) (Inj ExpProd (\case ExpProd t -> Just t; _ -> Nothing))
    , TreeInj (Proxy :: Proxy (ProdTy a)) (Inj ExpProdTy (\case ExpProdTy t -> Just t; _ -> Nothing))
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
