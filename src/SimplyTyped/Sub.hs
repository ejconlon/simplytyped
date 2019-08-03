{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module SimplyTyped.Sub where

import Control.Lens (Iso', Prism', from, iso, over, prism, review, simple, view, withPrism)
import Control.Monad (ap)
import Control.Monad.Except (Except, MonadError(..), runExcept)
import Control.Monad.Trans (MonadTrans(..))
import Control.Newtype.Generics (Newtype)
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Bitraversable (Bitraversable(..))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import SimplyTyped.Lenses
import SimplyTyped.Prelude
import SimplyTyped.Tree

data SubError
  = ApplyError Int Int
  | UnboundError Int
  deriving (Generic, Eq, Show)

class ThrowSub m where
  throwSub :: SubError -> m a

newtype Sub a =
  Sub
    { unSub :: Except SubError a
    }
  deriving (Generic, Functor, Applicative, Monad)

instance ThrowSub Sub where
  throwSub = Sub . throwError

runSub :: Sub a -> Either SubError a
runSub = runExcept . unSub

data BoundScope = BoundScope { boundScopeIndex :: Int }
  deriving (Generic, Eq, Show)

data FreeScope a = FreeScope { freeScopeVar :: a }
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data BinderScope n e = BinderScope
  { binderScopeArity :: Int
  , binderScopeInfo :: n
  , binderScopeBody :: e
  }
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

data EmbedScope f e = EmbedScope { embedScopeBody :: f e }
  deriving (Generic, Eq, Show, Functor)

data UnderScope n f e a
  = UnderBoundScope BoundScope
  | UnderFreeScope (FreeScope a)
  | UnderBinderScope (BinderScope n e)
  | UnderEmbedScope (EmbedScope f e)
  deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

$(makePrisms ''UnderScope)

instance Functor f => Bifunctor (UnderScope n f) where
  bimap _ _ (UnderBoundScope (BoundScope b)) = UnderBoundScope (BoundScope b)
  bimap _ g (UnderFreeScope (FreeScope a)) = UnderFreeScope (FreeScope (g a))
  bimap f _ (UnderBinderScope (BinderScope i x e)) = UnderBinderScope (BinderScope i x (f e))
  bimap f _ (UnderEmbedScope (EmbedScope fe)) = UnderEmbedScope (EmbedScope (f <$> fe))

instance Foldable f => Bifoldable (UnderScope n f) where
  bifoldr _ _ z (UnderBoundScope _) = z
  bifoldr _ g z (UnderFreeScope (FreeScope a)) = g a z
  bifoldr f _ z (UnderBinderScope (BinderScope _ _ e)) = f e z
  bifoldr f _ z (UnderEmbedScope (EmbedScope fe)) = foldr f z fe

instance Traversable f => Bitraversable (UnderScope n f) where
  bitraverse _ _ (UnderBoundScope (BoundScope b)) = pure (UnderBoundScope (BoundScope b))
  bitraverse _ g (UnderFreeScope (FreeScope a)) = UnderFreeScope . FreeScope <$> g a
  bitraverse f _ (UnderBinderScope (BinderScope i x e)) = UnderBinderScope . BinderScope i x <$> f e
  bitraverse f _ (UnderEmbedScope (EmbedScope fe)) = UnderEmbedScope . EmbedScope <$> traverse f fe

newtype Scope n f a =
  Scope
    { unScope :: UnderScope n f (Scope n f a) a
    }
  deriving (Generic)

type Binder n f a = BinderScope n (Scope n f a)

_UnderScope :: Iso' (Scope n f a) (UnderScope n f (Scope n f a) a)
_UnderScope = iso unScope Scope

instance Newtype (Scope n f a)

type BottomUp n f g a = f (Scope n g a) -> g (Scope n g a)

instance (Eq (f (Scope n f a)), Eq n, Eq a) => Eq (Scope n f a) where
  Scope u == Scope v = u == v

instance (Show (f (Scope n f a)), Show n, Show a) => Show (Scope n f a) where
  showsPrec d (Scope u) = showsPrec d u

instance Functor f => Functor (Scope n f) where
  fmap f (Scope us) = Scope (bimap (fmap f) f us)

instance Foldable f => Foldable (Scope n f) where
  foldr f z (Scope us) = bifoldr (flip (foldr f)) f z us

instance Traversable f => Traversable (Scope n f) where
  traverse f (Scope us) = Scope <$> bitraverse (traverse f) f us

instance Functor f => Applicative (Scope n f) where
  pure = Scope . UnderFreeScope . FreeScope
  (<*>) = ap

instance (Read a, Show a, Treeable n, Treeable (f (Scope n f a))) => Treeable (Scope n f a) where
  refTree _ = "scope"
  defineTree _ =
    let refN = refTree (Proxy :: Proxy n)
        refE = refTree (Proxy :: Proxy (f (Scope n f a)))
     in ChoiceDef
          [ BranchDef (BranchFixed [LeafDef (LeafKeyword "bound"), LeafDef LeafNat])
          , BranchDef (BranchFixed [LeafDef (LeafKeyword "free"), LeafDef LeafIdent])
          , BranchDef (BranchFixed [LeafDef (LeafKeyword "binder"), LeafDef LeafNat, RefDef refN, RefDef "scope"])
          , BranchDef (BranchFixed [LeafDef (LeafKeyword "embed"), RefDef refE])
          ]
  depsTree _ = [TreeProof (Proxy :: Proxy n), TreeProof (Proxy :: Proxy (f (Scope n f a)))]
  parseTree p t = parseBound <|> parseFree <|> parseBinder <|> parseEmbed
    where
      parseBound =
        case t of
          Branch [Leaf "bound", Leaf tb] -> Scope . UnderBoundScope . BoundScope <$> parseNat tb
          _ -> parseFail
      parseFree =
        case t of
          Branch [Leaf "free", Leaf ta] -> Scope . UnderFreeScope . FreeScope <$> parseRead ta
          _ -> parseFail
      parseBinder =
        case t of
          Branch [Leaf "binder", Leaf ti, tx, te] -> do
            i <- parseNat ti
            x <- parseTree (Proxy :: Proxy n) tx
            e <- parseTree p te
            pure (Scope (UnderBinderScope (BinderScope i x e)))
          _ -> parseFail
      parseEmbed =
        case t of
          Branch [Leaf "embed", te] -> do
            e <- parseTree (Proxy :: Proxy (f (Scope n f a))) te
            pure (Scope (UnderEmbedScope (EmbedScope e)))
          _ -> parseFail
  renderTree (Scope us) =
    case us of
      UnderBoundScope (BoundScope b) -> Branch [Leaf "bound", Leaf (showAtom b)]
      UnderFreeScope (FreeScope a) -> Branch [Leaf "free", Leaf (showAtom a)]
      UnderBinderScope (BinderScope i x e) -> Branch [Leaf "binder", Leaf (showAtom i), renderTree x, renderTree e]
      UnderEmbedScope (EmbedScope fe) -> Branch [Leaf "embed", renderTree fe]

subScopeShift :: Functor f => Int -> Int -> Scope n f a -> Scope n f a
subScopeShift c d s@(Scope us) =
  case us of
    UnderBoundScope (BoundScope b) ->
      if b < c
        then s
        else Scope (UnderBoundScope (BoundScope (b + d)))
    UnderFreeScope _ -> s
    UnderBinderScope (BinderScope i x e) -> Scope (UnderBinderScope (BinderScope i x (subScopeShift (c + i) d e)))
    UnderEmbedScope fe -> Scope (UnderEmbedScope (subScopeShift c d <$> fe))

scopeShift :: Functor f => Int -> Scope n f a -> Scope n f a
scopeShift = subScopeShift 0

scopeBind :: Functor f => Int -> Scope n f a -> (a -> Scope n f b) -> Scope n f b
scopeBind n (Scope us) f =
  case us of
    UnderBoundScope (BoundScope b) -> Scope (UnderBoundScope (BoundScope b))
    UnderFreeScope (FreeScope a) -> scopeShift n (f a)
    UnderBinderScope (BinderScope i x e) -> Scope (UnderBinderScope (BinderScope i x (scopeBind (n + i) e f)))
    UnderEmbedScope (EmbedScope fe) -> Scope (UnderEmbedScope (EmbedScope ((\e -> scopeBind n e f) <$> fe)))

scopeBindOpt :: Functor f => Int -> Scope n f a -> (a -> Maybe (Scope n f a)) -> Scope n f a
scopeBindOpt n s@(Scope us) f =
  case us of
    UnderBoundScope _ -> s
    UnderFreeScope (FreeScope a) ->
      case f a of
        Nothing -> s
        Just s' -> scopeShift n s'
    UnderBinderScope (BinderScope i x e) -> Scope (UnderBinderScope (BinderScope i x (scopeBindOpt (n + i) e f)))
    UnderEmbedScope (EmbedScope fe) -> Scope (UnderEmbedScope (EmbedScope ((\e -> scopeBindOpt n e f) <$> fe)))

instance Functor f => Monad (Scope n f) where
  return = pure
  (>>=) = scopeBind 0

instance MonadTrans (Scope n) where
  lift = Scope . UnderEmbedScope . EmbedScope . fmap pure

scopeFreeVars :: (Foldable f, Ord a) => Scope n f a -> Set a
scopeFreeVars = Set.fromList . toList

class Scoped h where
  type ScopedInfo h :: *
  type ScopedFunctor h :: * -> *
  type ScopedIdentifier h :: *

  scoped :: Iso' h (ScopedType h)

type ScopedType h = Scope (ScopedInfo h) (ScopedFunctor h) (ScopedIdentifier h)

boundScoped :: Scoped h => Prism' h BoundScope
boundScoped = scoped . _UnderScope . _UnderBoundScope

reviewBoundScoped :: Scoped h => Int -> h
reviewBoundScoped = review boundScoped . BoundScope

freeScoped :: Scoped h => Prism' h (FreeScope (ScopedIdentifier h))
freeScoped = scoped . _UnderScope . _UnderFreeScope

reviewFreeScoped :: Scoped h => ScopedIdentifier h -> h
reviewFreeScoped = review freeScoped . FreeScope

binderScoped' :: Scoped h => Prism' h (BinderScope (ScopedInfo h) (ScopedType h))
binderScoped' = scoped . _UnderScope . _UnderBinderScope

-- Jesus...
underMap :: Functor f => Prism' a (f b) -> Iso' b c -> Prism' a (f c)
underMap v w = withPrism v $ \m n ->
  let p fc =
        let fb = fmap (review w) fc
        in m fb
      q a =
        let b = n a
        in fmap (fmap (view w)) b
  in prism p q

binderScoped :: Scoped h => Prism' h (BinderScope (ScopedInfo h) h)
binderScoped = underMap binderScoped' (from scoped)

reviewBinderScoped :: Scoped h => BinderScope (ScopedInfo h) h -> h
reviewBinderScoped = review binderScoped

embedScoped' :: Scoped h => Prism' h (EmbedScope (ScopedFunctor h) (ScopedType h))
embedScoped' = scoped . _UnderScope . _UnderEmbedScope

embedScoped :: (Scoped h, Functor (ScopedFunctor h)) => Prism' h (EmbedScope (ScopedFunctor h) h)
embedScoped = underMap embedScoped' (from scoped)

wrapScoped' :: Scoped h => ScopedFunctor h (ScopedType h) -> h
wrapScoped' = review scoped . Scope . UnderEmbedScope . EmbedScope

wrapScoped :: (Scoped h, Functor (ScopedFunctor h)) => ScopedFunctor h h -> h
wrapScoped = wrapScoped' . fmap (view scoped)

liftScoped :: (Scoped h, Functor (ScopedFunctor h)) => ScopedFunctor h (ScopedIdentifier h) -> h
liftScoped fa =
  let fs = Scope . UnderFreeScope . FreeScope <$> fa
  in wrapScoped' fs

abstractScoped :: (Scoped h, Functor (ScopedFunctor h), Eq (ScopedIdentifier h)) => ScopedInfo h -> Seq (ScopedIdentifier h) -> h -> h
abstractScoped si sas = over scoped (Scope . UnderBinderScope . abstract si sas)

instantiateScoped :: (Scoped h, Functor (ScopedFunctor h)) => Seq h -> h -> h
instantiateScoped vs = over scoped (instantiate (fmap (review (from scoped)) vs))

instance Scoped (Scope n f a) where
  type ScopedInfo (Scope n f a) = n
  type ScopedFunctor (Scope n f a) = f
  type ScopedIdentifier (Scope n f a) = a

  scoped = simple

-- transformScope :: Functor f => BottomUp n f g a -> Scope n f a -> Scope n g a
-- transformScope t (Scope us) =
--   case us of
--     ScopeB b -> Scope (ScopeB b)
--     ScopeF a -> Scope (ScopeF a)
--     ScopeA e -> Scope (ScopeA (unBinder (transformBinder t (Binder e))))
--     ScopeE fe -> Scope (ScopeE (t (transformScope t <$> fe)))

-- Abstraction and instantiation
subAbstract :: (Functor f, Eq a) => Int -> n -> Seq a -> Scope n f a -> BinderScope n (Scope n f a)
subAbstract n x ks s = BinderScope n x (scopeBindOpt 0 s ((Scope . UnderBoundScope . BoundScope <$>) . flip Seq.elemIndexL ks))

subInstantiate :: Functor f => Int -> Seq (Scope n f a) -> Scope n f a -> Scope n f a
subInstantiate n vs s@(Scope us) =
  case us of
    UnderBoundScope (BoundScope b) -> fromMaybe s (vs Seq.!? (b - n))
    UnderFreeScope _ -> s
    UnderBinderScope (BinderScope i x e) -> Scope (UnderBinderScope (BinderScope i x (subInstantiate (n + i) (scopeShift i <$> vs) e)))
    UnderEmbedScope (EmbedScope fe) -> Scope (UnderEmbedScope (EmbedScope (subInstantiate n vs <$> fe)))

abstract :: (Functor f, Eq a) => n -> Seq a -> Scope n f a -> Binder n f a
abstract x ks =
  let n = Seq.length ks
   in subAbstract n x ks . scopeShift n

instantiate :: Functor f => Seq (Scope n f a) -> Scope n f a -> Scope n f a
instantiate = subInstantiate 0

rawApply :: (ThrowSub m, Applicative m, Functor f) => Seq (Scope n f a) -> Int -> Scope n f a -> m (Scope n f a)
rawApply vs i e =
  let len = Seq.length vs
   in if len == i
        then pure (scopeShift (-1) (instantiate vs e))
        else throwSub (ApplyError len i)

apply :: (ThrowSub m, Applicative m, Functor f) => Seq (Scope n f a) -> Binder n f a -> m (Scope n f a)
apply vs (BinderScope i _ e) = rawApply vs i e

abstract1 :: (Functor f, Eq a) => n -> a -> Scope n f a -> Binder n f a
abstract1 n k = abstract n (Seq.singleton k)

instantiate1 :: Functor f => Scope n f a -> Scope n f a -> Scope n f a
instantiate1 v = instantiate (Seq.singleton v)

apply1 :: (ThrowSub m, Applicative m, Functor f) => Scope n f a -> Binder n f a -> m (Scope n f a)
apply1 v = apply (Seq.singleton v)

-- data ScopeFold n f a r =
--   ScopeFold
--     { sfBound :: Int -> r
--     , sfFree :: a -> r
--     , sfBinder :: Binder n f a -> r
--     , sfFunctor :: f (Scope n f a) -> r
--     }
--   deriving (Generic, Functor)

-- contramapFold :: (x -> y) -> ScopeFold n f a r -> ScopeFold x n f a r
-- contramapFold f (ScopeFold bound free binder functor) = undefined

-- transformFold :: Functor f => BottomUp n f g a -> ScopeFold n g a r -> ScopeFold n f a r
-- transformFold t (ScopeFold bound free binder functor) =
--   ScopeFold bound free (binder . transformBinder t) (functor . t . (transformScope t <$>))

-- boundFold :: ThrowSub m => (a -> m r) -> (Binder n f a -> m r) -> (f (Scope n f a) -> m r) -> ScopeFold n f a (m r)
-- boundFold = ScopeFold (const (throwSub . UnboundError))

-- foldScope :: ScopeFold n f a r -> Scope n f a -> r
-- foldScope (ScopeFold bound free binder functor) (Scope us) =
--   case us of
--     ScopeB b -> bound b
--     ScopeF a -> free a
--     ScopeA ub -> binder (Binder ub)
--     ScopeE fe -> functor fe

-- Name
data Name n a =
  Name
    { nameKey :: n
    , nameValue :: a
    }
  deriving (Generic, Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
  Name _ x == Name _ y = x == y

type NameOnly n = Name n ()
