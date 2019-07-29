module SimplyTyped.Judge where

import SimplyTyped.Prelude

data OfTyJudge a =
  OfTyJudge a a
  deriving (Generic, Eq, Show)

data IsTyJudge a =
  IsTyJudge a
  deriving (Generic, Eq, Show)

data Judge a
  = JudgeOfTy (OfTyJudge a)
  | JudgeIsTy (IsTyJudge a)
  deriving (Generic, Eq, Show)
