{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Generic where

import Control.Lens (iso)
import Data.Foldable (find)
import qualified Data.Sequence as Seq
import SimplyTyped.Blanks
import SimplyTyped.Prelude
import SimplyTyped.Tree
-- import SimplyTyped.Parts (Identifier (..))

newtype ConName = ConName { unConName :: Text }
  deriving (Generic, Eq, Ord, Show, Read, IsString)

newtype FieldName = FieldName { unFieldName :: Text }
  deriving (Generic, Eq, Ord, Show, Read, IsString)

newtype BindName = BindName { unBindName :: Text }
  deriving (Generic, Eq, Ord, Show, Read, IsString)

newtype Identifier = Identifier { unIdentifier :: Text }
  deriving (Generic, Eq, Ord, Show, Read, IsString)

data ConDef = ConDef ConName (Seq FieldName) deriving (Generic, Show, Eq)

data BindDef = BindDef BindName (Maybe Int) deriving (Generic, Show, Eq)

data BindInfo = BindInfo BindName Int deriving (Generic, Show, Eq)

data ConPat b = ConPat (Maybe b) (Seq (Maybe b)) deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

data ConVal a = ConVal ConName (Seq a) deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

data BindVal a = BindVal BindName (Seq Identifier) a deriving (Generic, Show, Eq, Functor, Foldable, Traversable)

renderCon :: Treeable a => ConVal a -> Tree
renderCon (ConVal (ConName n) s) =
  let ln = Leaf (Atom n)
  in case s of
    Empty -> ln
    _ -> Branch (ln :<| (renderTree <$> s))

renderBind :: Treeable a => BindVal a -> Tree
renderBind = undefined

parseCon :: Treeable a => Proxy a -> Tree -> TreeParser (ConVal a)
parseCon _ (Leaf (Atom n)) = pure (ConVal (ConName n) Empty)
parseCon p (Branch s) =
  case s of
    Leaf (Atom n) :<| s' -> ConVal (ConName n) <$> traverse (parseTree p) s'
    _ -> empty

parseBind :: Treeable a => Proxy a -> Tree -> TreeParser (BindVal a)
parseBind = undefined

instance Treeable a => Treeable (ConVal a) where
  refTree _ = "conVal"
  defineTree _ = ChoiceDef [LeafDef LeafIdent, BranchDef (BranchTagged LeafIdent BranchWild)]
  depsTree _ = [TreeProof (Proxy :: Proxy a)]
  parseTree _ = parseCon (Proxy :: Proxy a)
  renderTree = renderCon

instance Treeable a => Treeable (BindVal a) where
  refTree _ = "bindVal"
  defineTree _ = ChoiceDef [LeafDef LeafIdent, BranchDef BranchWild, BranchDef BranchWild]
  depsTree _ = [TreeProof (Proxy :: Proxy a)]
  parseTree _ = parseBind (Proxy :: Proxy a)
  renderTree = renderBind

instance Treeable BindInfo where
  refTree _ = "bindInfo"
  defineTree _ = BranchDef (BranchFixed [LeafDef LeafIdent, LeafDef LeafNat])
  depsTree _ = []
  parseTree _ t =
    case t of
      Branch [Leaf (Atom n), p] -> BindInfo (BindName n) <$> parseLeaf parseNat p
      _ -> empty
  renderTree (BindInfo (BindName n) i) = Branch [Leaf (Atom n), Leaf (showAtom i)]

data Language = Language (Seq ConDef) (Seq BindDef) deriving (Generic, Eq, Show)

-- Add `LangPat (PatVal a)` to support patterns
data LangFunc a
  = LangCon (ConVal a)
  | LangBind (BindVal a)
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

-- newtype LangScope = LangScope { unLangScope :: Scope BindInfo LangFunc Identifier } deriving (Generic)
newtype LangScope = LangScope { unLangScope :: Scope BindInfo ConVal Identifier } deriving (Generic)

instance Scoped LangScope where
  type ScopedInfo LangScope = BindInfo
  -- type ScopedFunctor LangScope = LangFunc
  type ScopedFunctor LangScope = ConVal
  type ScopedIdentifier LangScope = Identifier
  scoped = iso unLangScope LangScope

findLangCon :: Text -> Language -> Maybe ConDef
findLangCon n (Language cons _) = find (\(ConDef (ConName x) _) -> x == n) cons

findLangBind :: Text -> Language -> Maybe BindDef
findLangBind n (Language _ binds) = find (\(BindDef (BindName x) _) -> x == n) binds

conArity :: ConDef -> Int
conArity (ConDef _ xs) = Seq.length xs

bindArity :: BindDef -> Maybe Int
bindArity (BindDef _ ar) = ar

assertArity :: (MonadThrow m, Exception e) => (Int -> Int -> e) -> (d -> Maybe Int) -> d -> Seq a -> m ()
assertArity mkE mkAr cd ts =
  let mar = mkAr cd
      tr = Seq.length ts
  in case mar of
    Just ar -> if ar == tr then throwM (mkE ar tr) else pure ()
    Nothing -> pure ()

assertConArity :: MonadThrow m => ConDef -> Seq a -> m ()
assertConArity = assertArity ConArityError (Just . conArity)

assertBindArity :: MonadThrow m => BindDef -> Seq a -> m ()
assertBindArity = assertArity BindArityError bindArity

parseLangCon :: MonadThrow m => Language -> ConDef -> Seq Tree -> m LangScope
parseLangCon lang cd@(ConDef cn _) ts = do
  assertConArity cd ts
  wrapScoped . ConVal cn <$> traverse (parseLang lang) ts

parseLangBind :: MonadThrow m => Language -> BindDef -> Seq Tree -> m LangScope
parseLangBind lang bd@(BindDef bn ar) ts = do
  assertBindArity bd ts
  undefined

data UnknownElemError = UnknownElemError Text deriving (Generic, Eq, Show, Typeable)
instance Exception UnknownElemError

data ConArityError = ConArityError Int Int deriving (Generic, Eq, Show, Typeable)
instance Exception ConArityError

data BindArityError = BindArityError Int Int deriving (Generic, Eq, Show, Typeable)
instance Exception BindArityError

parseLang :: MonadThrow m => Language -> Tree -> m LangScope
parseLang lang t =
  case t of
    Leaf (Atom n) ->
      case findLangCon n lang of
        Just con -> parseLangCon lang con []
        _ -> throwM (UnknownElemError n)
    Branch (Leaf (Atom n) :<| ts) ->
      case findLangCon n lang of
        Just con -> parseLangCon lang con ts
        _ ->
          case findLangBind n lang of
            Just bind -> parseLangBind lang bind ts
            _ -> throwM (UnknownElemError n)

renderLang :: LangScope -> Tree
renderLang (LangScope s) = renderTree s

data ConPatArityError = ConPatArityError Int Int deriving (Generic, Eq, Show, Typeable)
instance Exception ConPatArityError

align :: MonadThrow m => ConVal LangScope -> ConPat i -> m (Seq (i, LangScope))
align = undefined

-- Can use constraints (Scoped h, ScopedFunctor h ~ ConVal h)
matchConPat :: MonadThrow m => ConVal LangScope -> ConPat i -> m LangScope
matchConPat c p = do
  bs <- align c p
  undefined
