{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module SimplyTyped.Fix where

import SimplyTyped.Prelude

newtype Fix f =
  Fix
    { unFix :: f (Fix f)
    }
  deriving (Generic)

deriving instance Eq (f (Fix f)) => Eq (Fix f)

deriving instance Show (f (Fix f)) => Show (Fix f)
