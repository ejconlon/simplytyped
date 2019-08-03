module SimplyTyped.Blanks.Sub where

import Control.Monad.Except (Except, MonadError(..), runExcept)
import GHC.Generics (Generic)
import Prelude

data SubError
  = ApplyError Int Int
  | UnboundError Int
  deriving (Generic, Eq, Show)

class ThrowSub m where
  throwSub :: SubError -> m a

newtype Sub a =
  Sub
    { unSub :: Except SubError a
    }
  deriving (Generic, Functor, Applicative, Monad)

instance ThrowSub Sub where
  throwSub = Sub . throwError

runSub :: Sub a -> Either SubError a
runSub = runExcept . unSub
