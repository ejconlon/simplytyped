{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Back where

import Control.Lens (Iso', iso)
import Control.Newtype.Generics (Newtype)
import SimplyTyped.Deriving.Enum
import SimplyTyped.Deriving.Sum
import SimplyTyped.Deriving.Wrapper
import SimplyTyped.Blanks.Scope
import SimplyTyped.Blanks.Scoped
import SimplyTyped.Lenses
import SimplyTyped.Parts
import SimplyTyped.Prelude
import SimplyTyped.Tree

data BindCon =
    BindConLam
  | BindConPiTy
  | BindConSigmaTy
  deriving (Generic, Eq, Show, Ord, Bounded, Enum)
  deriving (Treeable) via (EnumWrapperTreeable BindCon)

instance EnumWrapper BindCon where
  enumRefTree _ = "bindCon"
  enumToValueKeyword v =
    case v of
      BindConLam -> "lambda"
      BindConPiTy -> "Pi"
      BindConSigmaTy -> "Sigma"
  enumFromValueKeyword _ k =
    case k of
      "lambda" -> Just BindConLam
      "Pi" -> Just BindConPiTy
      "Sigma" -> Just BindConSigmaTy
      _ -> Nothing

data BindInfo a = BindInfo
  { bindInfoCon :: BindCon
  , bindInfoName :: Identifier
  , bindInfoType :: a
  } deriving (Generic, Eq, Show)

instance Treeable a => Treeable (BindInfo a) where
  refTree _ = "bindInfo"
  defineTree _ = BranchDef (BranchFixed [
    RefDef (refTree (Proxy :: Proxy BindCon)),
    RefDef (refTree (Proxy :: Proxy Identifier)),
    RefDef (refTree (Proxy :: Proxy a))
    ])
  depsTree _ =
      [ TreeProof (Proxy :: Proxy BindCon)
      , TreeProof (Proxy :: Proxy Identifier)
      , TreeProof (Proxy :: Proxy a)
      ]
  parseTree _ t =
    case t of
      Branch [tx, ti, ty] -> do
        x <- parseTree (Proxy :: Proxy BindCon) tx
        i <- parseTree (Proxy :: Proxy Identifier) ti
        y <- parseTree (Proxy :: Proxy a) ty
        pure (BindInfo x i y)
      _ -> parseFail
  renderTree (BindInfo x i y) =
    Branch [renderTree x, renderTree i, renderTree y]

data Exp a =
      ExpUnitTm UnitTm
    | ExpUnitTy UnitTy
    | ExpProdTm (ProdTm a)
    | ExpProdTy (ProdTy a)
    -- | ExpReflTm (ReflTm a)
    -- | ExpEqTy (EqTy a)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

$(makePrisms ''Exp)

deriving via (SumWrapperTreeable (Exp a)) instance Treeable a => Treeable (Exp a)

instance Treeable a => SumWrapper (Exp a) where
  sumRefTree _ = "exp"
  sumTreeInjs _ =
    [ Inj (Proxy :: Proxy UnitTm) _ExpUnitTm
    , Inj (Proxy :: Proxy UnitTy) _ExpUnitTy
    , Inj (Proxy :: Proxy (ProdTm a)) _ExpProdTm
    , Inj (Proxy :: Proxy (ProdTy a)) _ExpProdTy
    ]

type UnderExpScope = Scope (BindInfo ExpScope) Exp Identifier

newtype ExpScope = ExpScope { unExpScope :: UnderExpScope }
  deriving (Generic, Typeable, Eq, Show)
  deriving (Treeable) via TreeWrapperTreeable ExpScope

_UnderExpScope :: Iso' ExpScope UnderExpScope
_UnderExpScope = iso unExpScope ExpScope

instance Newtype ExpScope

instance TreeWrapper ExpScope where
  wrapRefTree _ = "expScope"
  wrapConTree _ = "expScope"

instance Scoped ExpScope where
  type ScopedInfo ExpScope = BindInfo ExpScope
  type ScopedFunctor ExpScope = Exp
  type ScopedIdentifier ExpScope = Identifier

  scoped = _UnderExpScope
