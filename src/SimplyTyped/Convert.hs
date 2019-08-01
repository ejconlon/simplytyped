module SimplyTyped.Convert where

import qualified Data.Sequence as Seq
import SimplyTyped.Back
import SimplyTyped.Front
import SimplyTyped.Parts
import SimplyTyped.Prelude
import SimplyTyped.Sub

class Convert a where
  convert :: a -> ExpScope

instance Convert VarTm where
  convert (VarTm i) = freeVarScoped i

instance Convert UnitTm where
  convert = embedScoped . ExpUnitTm

instance Convert UnitTy where
  convert = embedScoped . ExpUnitTy

instance Convert a => Convert (ProdTm a) where
  convert (ProdTm x y) = embedScoped (ExpProdTm (ProdTm (convert x) (convert y)))

instance Convert a => Convert (ProdTy a) where
  convert (ProdTy x y) = embedScoped (ExpProdTy (ProdTy (convert x) (convert y)))

instance Convert a => Convert (LamTm a) where
  convert (LamTm i x y) =
    let bi = BindInfo BindConLam i (convert x)
     in abstractScoped bi (Seq.singleton i) (convert y)

instance Convert a => Convert (PiTy a) where
  convert (PiTy i x y) =
    let bi = BindInfo BindConPiTy i (convert x)
     in abstractScoped bi (Seq.singleton i) (convert y)

instance Convert FrontFix where
  convert (FrontFix t) =
    case t of
      FrontExpVarTm u -> convert u
      FrontExpUnitTm u -> convert u
      FrontExpUnitTy u -> convert u
      FrontExpProdTm u -> convert u
      FrontExpProdTy u -> convert u
      FrontExpLamTm u -> convert u
      FrontExpPiTy u -> convert u
