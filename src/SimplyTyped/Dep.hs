module SimplyTyped.Dep where

import Data.Text (Text)
import SimplyTyped.Prelude
import SimplyTyped.Sub

type Identifier = Text

data UnitExp = UnitExp deriving (Eq, Show)

data UnitTy = UnitTy deriving (Eq, Show)

data ProdExp a = ProdExp a a deriving (Eq, Show)

data ProdTy a = ProdTy a a deriving (Eq, Show)

data BindCon = BindConPi deriving (Eq, Show)

data Bind a = Bind { bindCon :: BindCon, bindType :: a } deriving (Eq, Show)

data Exp a =
      ExpUnit UnitExp
    | ExpUnitTy UnitTy
    | ExpProd (ProdExp a)
    | ExpProdTy (ProdTy a)
    deriving (Eq, Show)

newtype ExpScope = ExpScope (Scope (Bind ExpScope) Exp Identifier) deriving (Eq, Show)

data EqualJudge a = EqualJudge a a a deriving (Eq, Show)

data OfTyJudge a = OfTyJudge a a deriving (Eq, Show)

data IsTyJudge a = IsTyJudge a deriving (Eq, Show)

data Judgement a =
      JudgeEqual (EqualJudge a)
    | JudgeOfTy (OfTyJudge a)
    | JudgeIsTy (IsTyJudge a)
    deriving (Eq, Show)
