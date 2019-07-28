{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Deriving.Sum where

import Control.Lens (Prism', preview, review)
import Data.Foldable (asum)
import SimplyTyped.Prelude
import SimplyTyped.Tree

data TreeInj a where
  TreeInj :: Treeable b => Proxy b -> Prism' a b -> TreeInj a

class SumWrapper a where
  sumRefTree :: Proxy a -> TreeIdent
  sumTreeInjs :: Proxy a -> Seq (TreeInj a)

newtype SumWrapperTreeable a =
  SumWrapperTreeable
    { unSumWrapperTreeable :: a
    }

instance SumWrapper a => Treeable (SumWrapperTreeable a) where
  refTree _ = sumRefTree (Proxy :: Proxy a)
  defineTree _ = ChoiceDef ((\(TreeInj p _) -> defineTree p) <$> sumTreeInjs (Proxy :: Proxy a))
  depsTree _ = mergeDepTrees ((\(TreeInj p _) -> depsTree p) <$> sumTreeInjs (Proxy :: Proxy a))
  parseTree _ t =
    SumWrapperTreeable <$> asum ((\(TreeInj p i) -> review i <$> parseTree p t) <$> sumTreeInjs (Proxy :: Proxy a))
  renderTree (SumWrapperTreeable t) = go (sumTreeInjs (Proxy :: Proxy a))
    where
      go Empty = error "unmatched sum branch"
      go (TreeInj _ i :<| injs) = maybe (go injs) renderTree (preview i t)
