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
  convert (VarTm i) = ExpScope (freeVarScope i)

instance Convert UnitTm where
  convert = ExpScope . wrapScope . ExpUnitTm

instance Convert UnitTy where
  convert = ExpScope . wrapScope . ExpUnitTy

instance Convert a => Convert (ProdTm a) where
  convert (ProdTm x y) = ExpScope (wrapScope (ExpProdTm (ProdTm (unExpScope (convert x)) (unExpScope (convert y)))))

instance Convert a => Convert (ProdTy a) where
  convert (ProdTy x y) = ExpScope (wrapScope (ExpProdTy (ProdTy (unExpScope (convert x)) (unExpScope (convert y)))))

instance Convert a => Convert (LamTm a) where
  convert (LamTm i x y) =
    let bi = BindInfo BindConLam i (convert x)
     in ExpScope (binderScope (abstract bi (Seq.singleton i) (unExpScope (convert y))))

instance Convert a => Convert (PiTy a) where
  convert (PiTy i x y) =
    let bi = BindInfo BindConPiTy i (convert x)
     in ExpScope (binderScope (abstract bi (Seq.singleton i) (unExpScope (convert y))))

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
