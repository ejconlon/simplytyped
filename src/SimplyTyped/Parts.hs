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
  deriving (Generic, Eq, Show)

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
  deriving (Generic, Eq, Show)

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

data PiTm a =
  PiTm Identifier a a
  deriving (Generic, Eq, Show)

instance Treeable a => Treeable (PiTm a) where
  refTree _ = "piTm"
  defineTree _ =
    let identRef = refTree (Proxy :: Proxy Identifier)
        nestRef = refTree (Proxy :: Proxy a)
     in BranchDef (BranchFixed [LeafDef (LeafKeyword "pi"), RefDef identRef, RefDef nestRef, RefDef nestRef])
  depsTree _ = [TreeProof (Proxy :: Proxy Identifier), TreeProof (Proxy :: Proxy a)]
  parseTree _ t =
    case t of
      Branch [Leaf "pi", i, l, r] ->
        PiTm <$> parseTree (Proxy :: Proxy Identifier) i <*> parseTree (Proxy :: Proxy a) l <*>
        parseTree (Proxy :: Proxy a) r
      _ -> empty
  renderTree (PiTm i l r) = Branch [Leaf "pi", renderTree i, renderTree l, renderTree r]

data PiTy a =
  PiTy Identifier a a
  deriving (Generic, Eq, Show)

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

-- data SigmaExp a = SigmaExp a a deriving (Generic, Eq, Show)
-- data SigmaTy a = SigmaTy a a deriving (Generic, Eq, Show)
-- data ReflExp a = ReflExp a deriving (Generic, Eq, Show)
-- data EqTy a = EqTy a a a deriving (Generic, Eq, Show)
