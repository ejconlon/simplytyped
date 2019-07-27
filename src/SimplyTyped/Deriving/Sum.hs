{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Deriving.Sum where

import Data.Foldable (asum)
import SimplyTyped.Prelude
import SimplyTyped.Tree

data Inj a b =
    Inj
        { injTo :: b -> a
        , injFrom :: a -> Maybe b
        }

data TreeInj a where
    TreeInj :: Treeable b => Proxy b -> Inj a b -> TreeInj a

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
        SumWrapperTreeable <$> asum ((\(TreeInj p i) -> injTo i <$> parseTree p t) <$> sumTreeInjs (Proxy :: Proxy a))
    renderTree (SumWrapperTreeable t) = go (sumTreeInjs (Proxy :: Proxy a))
      where
        go Empty = error "unmatched sum branch"
        go (TreeInj _ i :<| injs) = maybe (go injs) renderTree (injFrom i t)
