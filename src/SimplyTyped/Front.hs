{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module SimplyTyped.Front where

import SimplyTyped.Deriving.Sum
import SimplyTyped.Lenses
import SimplyTyped.Parts
import SimplyTyped.Prelude
import SimplyTyped.Tree

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
    [ TreeInj (Proxy :: Proxy UnitExp) (Inj ExpUnit (\case ExpUnit t -> Just t; _ -> Nothing))
    , TreeInj (Proxy :: Proxy UnitTy) (Inj ExpUnitTy (\case ExpUnitTy t -> Just t; _ -> Nothing))
    , TreeInj (Proxy :: Proxy (ProdExp a)) (Inj ExpProd (\case ExpProd t -> Just t; _ -> Nothing))
    , TreeInj (Proxy :: Proxy (ProdTy a)) (Inj ExpProdTy (\case ExpProdTy t -> Just t; _ -> Nothing))
    ]
