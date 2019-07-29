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
      FrontExpUnit UnitExp
    | FrontExpUnitTy UnitTy
    | FrontExpProd (ProdExp a)
    | FrontExpProdTy (ProdTy a)
    | FrontExpPi (PiExp a)
    | FrontExpPiTy (PiTy a)
    -- | FrontExpSigma (SigmaExp a)
    -- | FrontExpSigmaTy (SigmaTy a)
    -- | FrontExpRefl (ReflExp a)
    -- | FrontExpEqTy (EqTy a)
    deriving (Generic, Eq, Show)

$(makePrisms ''FrontExp)

deriving via (SumWrapperTreeable (FrontExp a)) instance Treeable a => Treeable (FrontExp a)

instance Treeable a => SumWrapper (FrontExp a) where
  sumRefTree _ = "frontExp"
  sumTreeInjs _ =
    [ Inj (Proxy :: Proxy UnitExp) _FrontExpUnit
    , Inj (Proxy :: Proxy UnitTy) _FrontExpUnitTy
    , Inj (Proxy :: Proxy (ProdExp a)) _FrontExpProd
    , Inj (Proxy :: Proxy (ProdTy a)) _FrontExpProdTy
    , Inj (Proxy :: Proxy (PiExp a)) _FrontExpPi
    , Inj (Proxy :: Proxy (PiTy a)) _FrontExpPiTy
    ]

instance TreeWrapper FrontFix where
  wrapRefTree _ = "frontFix"
  wrapConTree _ = "frontFix"

newtype FrontFix = FrontFix { unFrontFix :: FrontExp (FrontFix) }
    deriving (Generic, Typeable, Eq, Show)
    deriving (Treeable) via TreeWrapperTreeable FrontFix

instance Newtype FrontFix
