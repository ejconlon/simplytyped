{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Deriving.Sum where

import Control.Lens (Prism', preview, review)
import Data.Foldable (asum)
import SimplyTyped.Prelude
import SimplyTyped.Tree

data Inj c a where
  Inj :: c b => Proxy b -> Prism' a b -> Inj c a

class SumWrapper a where
  sumRefTree :: Proxy a -> TreeIdent
  sumTreeInjs :: Proxy a -> Seq (Inj Treeable a)

newtype SumWrapperTreeable a =
  SumWrapperTreeable
    { unSumWrapperTreeable :: a
    }

instance SumWrapper a => Treeable (SumWrapperTreeable a) where
  refTree _ = sumRefTree (Proxy :: Proxy a)
  defineTree _ = ChoiceDef ((\(Inj p _) -> RefDef (refTree p)) <$> sumTreeInjs (Proxy :: Proxy a))
  depsTree _ = (\(Inj p _) -> TreeProof p) <$> sumTreeInjs (Proxy :: Proxy a)
  parseTree _ t =
    SumWrapperTreeable <$> asum ((\(Inj p i) -> review i <$> parseTree p t) <$> sumTreeInjs (Proxy :: Proxy a))
  renderTree (SumWrapperTreeable t) = go (sumTreeInjs (Proxy :: Proxy a))
    where
      go Empty = error "unmatched sum branch"
      go (Inj _ i :<| injs) = maybe (go injs) renderTree (preview i t)
