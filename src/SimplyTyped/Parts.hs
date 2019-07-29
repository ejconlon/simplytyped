{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Parts where

import qualified Data.Sequence as Seq
import SimplyTyped.Prelude
import SimplyTyped.Tree

type Identifier = Text

data UnitExp =
  UnitExp
  deriving (Generic, Eq, Show)

instance Treeable UnitExp where
  refTree _ = "unitExp"
  defineTree _ = LeafDef (LeafKeyword "unit")
  depsTree _ = Seq.empty
  parseTree _ t =
    case t of
      Leaf "unit" -> pure UnitExp
      _ -> empty
  renderTree _ = Leaf "unit"

data UnitTy =
  UnitTy
  deriving (Generic, Eq, Show)

instance Treeable UnitTy where
  refTree _ = "unitTy"
  defineTree _ = LeafDef (LeafKeyword "Unit")
  depsTree _ = Seq.empty
  parseTree _ t =
    case t of
      Leaf "Unit" -> pure UnitTy
      _ -> empty
  renderTree _ = Leaf "Unit"

data ProdExp a =
  ProdExp a a
  deriving (Generic, Eq, Show)

instance Treeable a => Treeable (ProdExp a) where
  refTree _ = "prodExp"
  defineTree _ =
    let nestRef = refTree (Proxy :: Proxy a)
     in BranchDef (BranchFixed [LeafDef (LeafKeyword "prod"), RefDef nestRef, RefDef nestRef])
  depsTree _ = Seq.empty
  parseTree _ t =
    case t of
      Branch [Leaf "prod", l, r] -> ProdExp <$> parseTree (Proxy :: Proxy a) l <*> parseTree (Proxy :: Proxy a) r
      _ -> empty
  renderTree (ProdExp l r) = Branch [Leaf "prod", renderTree l, renderTree r]

data ProdTy a =
  ProdTy a a
  deriving (Generic, Eq, Show)

instance Treeable a => Treeable (ProdTy a) where
  refTree _ = "prodTy"
  defineTree _ =
    let nestRef = refTree (Proxy :: Proxy a)
     in BranchDef (BranchFixed [LeafDef (LeafKeyword "Prod"), RefDef nestRef, RefDef nestRef])
  depsTree _ = Seq.empty
  parseTree _ t =
    case t of
      Branch [Leaf "Prod", l, r] -> ProdTy <$> parseTree (Proxy :: Proxy a) l <*> parseTree (Proxy :: Proxy a) r
      _ -> empty
  renderTree (ProdTy l r) = Branch [Leaf "Prod", renderTree l, renderTree r]
-- data PiTy a = PiTy a a deriving (Generic, Eq, Show)
-- data SigmaTy a = SigmaTy a a deriving (Generic, Eq, Show)
-- data RewriteExp a = RewriteExp a a deriving (Generic, Eq, Show)
-- data ReflExp a = ReflExp a deriving (Generic, Eq, Show)
-- data EqTy a = EqTy a a a deriving (Generic, Eq, Show)
