{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SimplyTyped.Wrapper where

import Control.Newtype.Generics (Newtype (..))
import SimplyTyped.Tree
import SimplyTyped.Prelude

class TreeWrapper a where
    wrapRefTree :: Proxy a -> TreeIdent
    wrapConTree :: Proxy a -> Atom

newtype TreeWrapperTreeable a = TreeWrapperTreeable { unTreeWrapperTreeable :: a }

instance (TreeWrapper a, Newtype a, Treeable (O a)) => Treeable (TreeWrapperTreeable a) where
    refTree _ = wrapRefTree (Proxy :: Proxy a)
    defineTree _ =
        let nestRef = refTree (Proxy :: Proxy (O a))
            con = wrapConTree (Proxy :: Proxy a)
        in BranchDef (BranchFixed [LeafDef (LeafKeyword con), RefDef nestRef])
    depsTree _ = depsTree (Proxy :: Proxy (O a))
    parseTree _ t =
        let con = wrapConTree  (Proxy :: Proxy a)
        in case t of
            Branch [Leaf atom, tt] ->
                if atom == con
                    then TreeWrapperTreeable . pack <$> parseTree (Proxy :: Proxy (O a)) tt
                    else parseFail
            _ -> parseFail
    renderTree ta =
        let con = wrapConTree (Proxy :: Proxy a)
        in Branch [Leaf con, renderTree (unpack (unTreeWrapperTreeable ta))]
