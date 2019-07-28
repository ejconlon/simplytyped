{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module SimplyTyped.Front where

import SimplyTyped.Deriving.Sum
import SimplyTyped.Lenses
import SimplyTyped.Parts
import SimplyTyped.Prelude
import SimplyTyped.Tree

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
    [ TreeInj (Proxy :: Proxy UnitExp) _FrontExpUnit
    , TreeInj (Proxy :: Proxy UnitTy) _FrontExpUnitTy
    , TreeInj (Proxy :: Proxy (ProdExp a)) _FrontExpProd
    , TreeInj (Proxy :: Proxy (ProdTy a)) _FrontExpProdTy
    ]
