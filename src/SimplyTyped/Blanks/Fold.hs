module SimplyTyped.Blanks.Fold where

import GHC.Generics (Generic)
import Prelude
import SimplyTyped.Blanks.Scope

data ScopeFold n f a r =
  ScopeFold
    { sfBound :: BoundScope -> r
    , sfFree :: FreeScope a -> r
    , sfBinder :: BinderScope n (Scope n f a) -> r
    , sfFunctor :: EmbedScope f (Scope n f a) -> r
    }
  deriving (Generic, Functor)

foldScope :: ScopeFold n f a r -> Scope n f a -> r
foldScope (ScopeFold bound free binder functor) (Scope us) =
  case us of
    UnderBoundScope u -> bound u
    UnderFreeScope u -> free u
    UnderBinderScope u -> binder u
    UnderEmbedScope u -> functor u
