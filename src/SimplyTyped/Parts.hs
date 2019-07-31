{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Parts where

import qualified Data.Sequence as Seq
import SimplyTyped.Prelude
import SimplyTyped.Tree

newtype Identifier =
  Identifier
    { unIdentifier :: Text
    }
  deriving (Generic, Eq, Ord, Read, Show, IsString)

instance Treeable Identifier where
  refTree _ = "identifier"
  defineTree _ = LeafDef LeafIdent
  depsTree _ = Seq.empty
  parseTree _ t =
    case t of
      Leaf (Atom i) -> pure (Identifier i)
      _ -> empty
  renderTree (Identifier i) = Leaf (Atom i)

data VarTm =
  VarTm Identifier
  deriving (Generic, Eq, Show)

instance Treeable VarTm where
  refTree _ = "varTm"
  defineTree _ =
    let p = refTree (Proxy :: Proxy Identifier)
     in BranchDef (BranchFixed [LeafDef (LeafKeyword "var"), RefDef p])
  depsTree _ = [TreeProof (Proxy :: Proxy Identifier)]
  parseTree _ t =
    case t of
      Branch [Leaf "var", i] -> VarTm <$> parseTree (Proxy :: Proxy Identifier) i
      _ -> empty
  renderTree (VarTm i) = Branch [Leaf "var", renderTree i]

data UnitTm =
  UnitTm
  deriving (Generic, Eq, Show)

instance Treeable UnitTm where
  refTree _ = "unitTm"
  defineTree _ = LeafDef (LeafKeyword "unit")
  depsTree _ = Seq.empty
  parseTree _ t =
    case t of
      Leaf "unit" -> pure UnitTm
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

data ProdTm a =
  ProdTm a a
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

instance Treeable a => Treeable (ProdTm a) where
  refTree _ = "prodTm"
  defineTree _ =
    let nestRef = refTree (Proxy :: Proxy a)
     in BranchDef (BranchFixed [LeafDef (LeafKeyword "prod"), RefDef nestRef, RefDef nestRef])
  depsTree _ = [TreeProof (Proxy :: Proxy a)]
  parseTree _ t =
    case t of
      Branch [Leaf "prod", l, r] -> ProdTm <$> parseTree (Proxy :: Proxy a) l <*> parseTree (Proxy :: Proxy a) r
      _ -> empty
  renderTree (ProdTm l r) = Branch [Leaf "prod", renderTree l, renderTree r]

data ProdTy a =
  ProdTy a a
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

instance Treeable a => Treeable (ProdTy a) where
  refTree _ = "prodTy"
  defineTree _ =
    let nestRef = refTree (Proxy :: Proxy a)
     in BranchDef (BranchFixed [LeafDef (LeafKeyword "Prod"), RefDef nestRef, RefDef nestRef])
  depsTree _ = [TreeProof (Proxy :: Proxy a)]
  parseTree _ t =
    case t of
      Branch [Leaf "Prod", l, r] -> ProdTy <$> parseTree (Proxy :: Proxy a) l <*> parseTree (Proxy :: Proxy a) r
      _ -> empty
  renderTree (ProdTy l r) = Branch [Leaf "Prod", renderTree l, renderTree r]

data LamTm a =
  LamTm Identifier a a
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

instance Treeable a => Treeable (LamTm a) where
  refTree _ = "lamTm"
  defineTree _ =
    let identRef = refTree (Proxy :: Proxy Identifier)
        nestRef = refTree (Proxy :: Proxy a)
     in BranchDef (BranchFixed [LeafDef (LeafKeyword "lambda"), RefDef identRef, RefDef nestRef, RefDef nestRef])
  depsTree _ = [TreeProof (Proxy :: Proxy Identifier), TreeProof (Proxy :: Proxy a)]
  parseTree _ t =
    case t of
      Branch [Leaf "lambda", i, l, r] ->
        LamTm <$> parseTree (Proxy :: Proxy Identifier) i <*> parseTree (Proxy :: Proxy a) l <*>
        parseTree (Proxy :: Proxy a) r
      _ -> empty
  renderTree (LamTm i l r) = Branch [Leaf "lambda", renderTree i, renderTree l, renderTree r]

data PiTy a =
  PiTy Identifier a a
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

instance Treeable a => Treeable (PiTy a) where
  refTree _ = "piTy"
  defineTree _ =
    let identRef = refTree (Proxy :: Proxy Identifier)
        nestRef = refTree (Proxy :: Proxy a)
     in BranchDef (BranchFixed [LeafDef (LeafKeyword "Pi"), RefDef identRef, RefDef nestRef, RefDef nestRef])
  depsTree _ = [TreeProof (Proxy :: Proxy Identifier), TreeProof (Proxy :: Proxy a)]
  parseTree _ t =
    case t of
      Branch [Leaf "Pi", i, l, r] ->
        PiTy <$> parseTree (Proxy :: Proxy Identifier) i <*> parseTree (Proxy :: Proxy a) l <*>
        parseTree (Proxy :: Proxy a) r
      _ -> empty
  renderTree (PiTy i l r) = Branch [Leaf "Pi", renderTree i, renderTree l, renderTree r]
-- data SigmaTy a = SigmaTy a a deriving (Generic, Eq, Show)
-- data ReflExp a = ReflExp a deriving (Generic, Eq, Show)
-- data EqTy a = EqTy a a a deriving (Generic, Eq, Show)
