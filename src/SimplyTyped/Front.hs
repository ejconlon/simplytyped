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

-- data PiTy a = PiTy a a deriving (Generic, Eq, Show)
-- data SigmaTy a = SigmaTy a a deriving (Generic, Eq, Show)

data FrontExp a =
      FrontExpUnit UnitExp
    | FrontExpUnitTy UnitTy
    | FrontExpProd (ProdExp a)
    | FrontExpProdTy (ProdTy a)
    -- | ExpPiTy (PiTy a)
    -- | ExpSigmaTy (SigmaTy a)
    -- | ExpRewrite (RewriteExp a)
    -- | ExpRefl (ReflExp a)
    -- | ExpEqTy (EqTy a)
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
    ]

instance TreeWrapper FrontFix where
  wrapRefTree _ = "frontFix"
  wrapConTree _ = "frontFix"

newtype FrontFix = FrontFix { unFrontFix :: FrontExp (FrontFix) }
    deriving (Generic, Eq, Show)
    deriving (Treeable) via TreeWrapperTreeable FrontFix

instance Newtype FrontFix
