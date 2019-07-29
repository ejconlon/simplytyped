{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module SimplyTyped.Deriving.Wrapper where

import Control.Newtype.Generics (Newtype(..))
import SimplyTyped.Prelude
import SimplyTyped.Tree

class TreeWrapper a where
  wrapRefTree :: Proxy a -> TreeIdent
  wrapConTree :: Proxy a -> Atom

newtype TreeWrapperTreeable a =
  TreeWrapperTreeable
    { unTreeWrapperTreeable :: a
    }

instance (TreeWrapper a, Newtype a, Treeable (O a)) => Treeable (TreeWrapperTreeable a) where
  refTree _ = wrapRefTree (Proxy :: Proxy a)
  defineTree _ =
    let nestRef = refTree (Proxy :: Proxy (O a))
        con = wrapConTree (Proxy :: Proxy a)
     in FixDef con nestRef
  depsTree _ = [TreeProof (Proxy :: Proxy (O a))]
  parseTree _ t = TreeWrapperTreeable . pack <$> parseTree (Proxy :: Proxy (O a)) t
  renderTree = renderTree . unpack . unTreeWrapperTreeable
