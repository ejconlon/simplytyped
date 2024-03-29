module SimplyTyped.Convert where

import SimplyTyped.Back
import SimplyTyped.Blanks.Scoped
import SimplyTyped.Front
import SimplyTyped.Parts
import SimplyTyped.Prelude

class Convert a where
  convert :: a -> ExpScope

instance Convert VarTm where
  convert (VarTm i) = reviewFreeScoped i

instance Convert TyTy where
  convert = liftScoped . ExpTyTy

instance Convert UnitTm where
  convert = liftScoped . ExpUnitTm

instance Convert UnitTy where
  convert = liftScoped . ExpUnitTy

instance Convert a => Convert (ProdTm a) where
  convert (ProdTm x y) = wrapScoped (ExpProdTm (ProdTm (convert x) (convert y)))

instance Convert a => Convert (ProdTy a) where
  convert (ProdTy x y) = wrapScoped (ExpProdTy (ProdTy (convert x) (convert y)))

instance Convert a => Convert (PiTm a) where
  convert (PiTm i x y) =
    let bi = BindInfo BindConPi i (convert x)
     in abstract1Scoped bi i (convert y)

instance Convert a => Convert (PiTy a) where
  convert (PiTy i x y) =
    let bi = BindInfo BindConPiTy i (convert x)
     in abstract1Scoped bi i (convert y)

instance Convert FrontFix where
  convert (FrontFix t) =
    case t of
      FrontExpTyTy u -> convert u
      FrontExpVarTm u -> convert u
      FrontExpUnitTm u -> convert u
      FrontExpUnitTy u -> convert u
      FrontExpProdTm u -> convert u
      FrontExpProdTy u -> convert u
      FrontExpPiTm u -> convert u
      FrontExpPiTy u -> convert u
