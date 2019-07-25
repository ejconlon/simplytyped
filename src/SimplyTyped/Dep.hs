module SimplyTyped.Dep where

import SimplyTyped.Prelude

data UnitExp = UnitExp deriving (Eq, Show)

data UnitTy = UnitTy deriving (Eq, Show)

data ProdExp = ProdExp Exp Exp deriving (Eq, Show)

data ProdTy = ProdTy Exp Exp deriving (Eq, Show)

data Exp =
      ExpUnit UnitExp
    | ExpUnitTy UnitTy
    | ExpProd ProdExp
    | ExpProdTy ProdTy
    deriving (Eq, Show)

data EqualJudge = EqualJudge Exp Exp Exp deriving (Eq, Show)

data OfTyJudge = OfTyJudge Exp Exp deriving (Eq, Show)

data IsTyJudge = IsTyJudge Exp deriving (Eq, Show)

data Judgement =
      JudgeEqual EqualJudge
    | JudgeOfTy OfTyJudge
    | JudgeIsTy IsTyJudge
    deriving (Eq, Show)
