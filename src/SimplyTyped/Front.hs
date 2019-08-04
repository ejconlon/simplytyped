{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module SimplyTyped.Front where

import Control.Newtype.Generics
import SimplyTyped.Deriving.Sum
import SimplyTyped.Deriving.Wrapper
import SimplyTyped.Lenses
import SimplyTyped.Parts
import SimplyTyped.Prelude
import SimplyTyped.Tree

data FrontExp a =
      FrontExpTyTy TyTy
    | FrontExpVarTm VarTm
    | FrontExpUnitTm UnitTm
    | FrontExpUnitTy UnitTy
    | FrontExpProdTm (ProdTm a)
    | FrontExpProdTy (ProdTy a)
    | FrontExpLamTm (LamTm a)
    | FrontExpPiTy (PiTy a)
    -- | FrontExpSigmaTy (SigmaTy a)
    -- | FrontExpReflTm (ReflTm a)
    -- | FrontExpEqTy (EqTy a)
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

$(makePrisms ''FrontExp)

deriving via (SumWrapperTreeable (FrontExp a)) instance Treeable a => Treeable (FrontExp a)

instance Treeable a => SumWrapper (FrontExp a) where
  sumRefTree _ = "frontExp"
  sumTreeInjs _ =
    [ Inj (Proxy :: Proxy TyTy) _FrontExpTyTy
    , Inj (Proxy :: Proxy VarTm) _FrontExpVarTm
    , Inj (Proxy :: Proxy UnitTm) _FrontExpUnitTm
    , Inj (Proxy :: Proxy UnitTy) _FrontExpUnitTy
    , Inj (Proxy :: Proxy (ProdTm a)) _FrontExpProdTm
    , Inj (Proxy :: Proxy (ProdTy a)) _FrontExpProdTy
    , Inj (Proxy :: Proxy (LamTm a)) _FrontExpLamTm
    , Inj (Proxy :: Proxy (PiTy a)) _FrontExpPiTy
    ]

instance TreeWrapper FrontFix where
  wrapRefTree _ = "frontFix"
  wrapConTree _ = "frontFix"

newtype FrontFix = FrontFix { unFrontFix :: FrontExp (FrontFix) }
    deriving (Generic, Typeable, Eq, Show)
    deriving (Treeable) via TreeWrapperTreeable FrontFix

instance Newtype FrontFix
