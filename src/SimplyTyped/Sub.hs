{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module SimplyTyped.Sub where

import           Control.Monad        (ap)
import           Control.Monad.Except (Except, MonadError (..), runExcept)
import           Control.Monad.Trans  (MonadTrans (..))
import           Data.Bifoldable      (Bifoldable (..))
import           Data.Bifunctor       (Bifunctor (..))
import           Data.Bitraversable   (Bitraversable (..))
import           Data.Foldable        (toList)
import           Data.Maybe           (fromMaybe)
import           Data.Sequence        (Seq)
import qualified Data.Sequence        as Seq
import           Data.Set             (Set)
import qualified Data.Set             as Set
import qualified Data.Text            as Text
import           GHC.Generics         (Generic)
import           SimplyTyped.Prelude
import           SimplyTyped.Tree

-- Sub

data SubError =
      ApplyError Int Int
    | UnboundError Int
    | FunctorMatchError
    | BinderMatchError
    deriving (Generic, Eq, Show)

class ThrowSub m where
    throwSub :: SubError -> m a

newtype Sub a = Sub { unSub :: Except SubError a } deriving (Generic, Functor, Applicative, Monad)

instance ThrowSub Sub where
    throwSub = Sub . throwError

runSub :: Sub a -> Either SubError a
runSub = runExcept . unSub

-- UnderBinder

data UnderBinder n e = UnderBinder { ubArity :: Int, ubInfo :: n, ubBody :: e }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

-- UnderScope

data UnderScope n f e a =
      ScopeB Int
    | ScopeF a
    | ScopeA (UnderBinder n e)
    | ScopeE (f e)
    deriving (Generic, Eq, Show)

instance Functor f => Bifunctor (UnderScope n f) where
    bimap _ _ (ScopeB b)                   = ScopeB b
    bimap _ g (ScopeF a)                   = ScopeF (g a)
    bimap f _ (ScopeA (UnderBinder i x e)) = ScopeA (UnderBinder i x (f e))
    bimap f _ (ScopeE fe)                  = ScopeE (f <$> fe)

instance Foldable f => Bifoldable (UnderScope n f) where
    bifoldr _ _ z (ScopeB _)                   = z
    bifoldr _ g z (ScopeF a)                   = g a z
    bifoldr f _ z (ScopeA (UnderBinder _ _ e)) = f e z
    bifoldr f _ z (ScopeE fe)                  = foldr f z fe

instance Traversable f => Bitraversable (UnderScope n f) where
    bitraverse _ _ (ScopeB b)                   = pure (ScopeB b)
    bitraverse _ g (ScopeF a)                   = ScopeF <$> g a
    bitraverse f _ (ScopeA (UnderBinder i x e)) = ScopeA . UnderBinder i x <$> f e
    bitraverse f _ (ScopeE fe)                  = ScopeE <$> traverse f fe

-- Scope

newtype Scope n f a = Scope { unScope :: UnderScope n f (Scope n f a) a }
    deriving (Generic)

type BottomUp n f g a = f (Scope n g a) -> g (Scope n g a)

instance (Eq (f (Scope n f a)), Eq n, Eq a) => Eq (Scope n f a) where
    Scope u == Scope v = u == v

instance (Show (f (Scope n f a)), Show n, Show a) => Show (Scope n f a) where
    -- TODO show info
    showsPrec d (Scope u) = showsPrec d u

instance Functor f => Functor (Scope n f) where
    fmap f (Scope us) = Scope (bimap (fmap f) f us)

instance Foldable f => Foldable (Scope n f) where
    foldr f z (Scope us) = bifoldr (flip (foldr f)) f z us

instance Traversable f => Traversable (Scope n f) where
    traverse f (Scope us) = Scope <$> bitraverse (traverse f) f us

instance Functor f => Applicative (Scope n f) where
    pure = freeVarScope
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
    depsTree _ =
        mergeDepTrees
            [ selfDepsTree (Proxy :: Proxy n)
            , selfDepsTree (Proxy :: Proxy (f (Scope n f a)))
            ]
    parseTree p t = parseBound <|> parseFree <|> parseBinder <|> parseEmbed where
        parseBound =
            case t of
                Branch [Leaf "bound", Leaf tb] -> parseNat tb >>= pure . boundVarScope
                _ -> parseFail
        parseFree =
            case t of
                Branch [Leaf "free", Leaf ta] -> parseRead ta >>= pure . freeVarScope
                _ -> parseFail
        parseBinder =
            case t of
                Branch [Leaf "binder", Leaf ti, tx, te] -> do
                    i <- parseNat ti
                    x <- parseTree (Proxy :: Proxy n) tx
                    e <- parseTree p te
                    pure (Scope (ScopeA (UnderBinder i x e)))
                _ -> parseFail
        parseEmbed =
            case t of
                Branch [Leaf "embed", te] -> do
                    e <- parseTree (Proxy :: Proxy (f (Scope n f a))) te
                    pure (Scope (ScopeE e))
                _ -> parseFail
    renderTree (Scope us) =
        case us of
            ScopeB b                   -> Branch [Leaf "bound", Leaf (Text.pack (show b))]
            ScopeF a                   -> Branch [Leaf "free", Leaf (Text.pack (show a))]
            ScopeA (UnderBinder i x e) -> Branch [Leaf "binder", Leaf (Text.pack (show i)), renderTree x, renderTree e]
            ScopeE fe                  -> Branch [Leaf "embed", renderTree fe]

subScopeShift :: Functor f => Int -> Int -> Scope n f a -> Scope n f a
subScopeShift c d s@(Scope us) =
    case us of
        ScopeB b ->
            if b < c
                then s
                else Scope (ScopeB (b + d))
        ScopeF _ -> s
        ScopeA (UnderBinder i x e) -> Scope (ScopeA (UnderBinder i x (subScopeShift (c + i) d e)))
        ScopeE fe -> Scope (ScopeE (subScopeShift c d <$> fe))

scopeShift :: Functor f => Int -> Scope n f a -> Scope n f a
scopeShift = subScopeShift 0

scopeBind :: Functor f => Int -> Scope n f a -> (a -> Scope n f b) -> Scope n f b
scopeBind n (Scope us) f =
    case us of
        ScopeB b                   -> Scope (ScopeB b)
        ScopeF a                   -> scopeShift n (f a)
        ScopeA (UnderBinder i x e) -> Scope (ScopeA (UnderBinder i x (scopeBind (n + i) e f)))
        ScopeE fe                  -> Scope (ScopeE ((\e -> scopeBind n e f) <$> fe))

scopeBindOpt :: Functor f => Int -> Scope n f a -> (a -> Maybe (Scope n f a)) -> Scope n f a
scopeBindOpt n s@(Scope us) f =
    case us of
        ScopeB _                   -> s
        ScopeF a                   ->
            case f a of
                Nothing -> s
                Just s' -> scopeShift n s'
        ScopeA (UnderBinder i x e) -> Scope (ScopeA (UnderBinder i x (scopeBindOpt (n + i) e f)))
        ScopeE fe                  -> Scope (ScopeE ((\e -> scopeBindOpt n e f) <$> fe))

instance Functor f => Monad (Scope n f) where
    return = freeVarScope
    (>>=) = scopeBind 0

instance MonadTrans (Scope n) where
    lift = liftScope

boundVarScope :: Int -> Scope n f a
boundVarScope = Scope . ScopeB

freeVarScope :: a -> Scope n f a
freeVarScope = Scope . ScopeF

wrapScope :: f (Scope n f a) -> Scope n f a
wrapScope = Scope . ScopeE

liftScope :: Functor f => f a -> Scope n f a
liftScope = wrapScope . (freeVarScope <$>)

binderScope :: Binder n f a -> Scope n f a
binderScope = Scope . ScopeA . unBinder

scopeFreeVars :: (Foldable f, Ord a) => Scope n f a -> Set a
scopeFreeVars = Set.fromList . toList

-- TODO PRISMS!!!
matchFunctor :: Scope n f a -> Maybe (f (Scope n f a))
matchFunctor (Scope (ScopeE fe)) = pure fe
matchFunctor _                   = Nothing

forceFunctor :: (ThrowSub m, Applicative m) => Scope n f a -> m (f (Scope n f a))
forceFunctor s =
    case matchFunctor s of
        Just x  -> pure x
        Nothing -> throwSub FunctorMatchError

transformScope :: Functor f => BottomUp n f g a -> Scope n f a -> Scope n g a
transformScope t (Scope us) =
    case us of
        ScopeB b  -> Scope (ScopeB b)
        ScopeF a  -> Scope (ScopeF a)
        ScopeA e  -> Scope (ScopeA (unBinder (transformBinder t (Binder e))))
        ScopeE fe -> Scope (ScopeE (t (transformScope t <$> fe)))

-- Binder

newtype Binder n f a = Binder { unBinder :: UnderBinder n (Scope n f a) }
    deriving (Generic, Functor, Foldable, Traversable)

instance (Eq (f (Scope n f a)), Eq n, Eq a) => Eq (Binder n f a) where
    Binder u == Binder v = u == v

instance (Show (f (Scope n f a)), Show n, Show a) => Show (Binder n f a) where
    showsPrec d (Binder u) = showsPrec d u

matchBinder :: Scope n f a -> Maybe (Binder n f a)
matchBinder (Scope (ScopeA ub)) = pure (Binder ub)
matchBinder _                   = Nothing

forceBinder :: (ThrowSub m, Applicative m) => Scope n f a -> m (Binder n f a)
forceBinder s =
    case matchBinder s of
        Just x  -> pure x
        Nothing -> throwSub BinderMatchError

binderArity :: Binder n f a -> Int
binderArity = ubArity . unBinder

binderInfo :: Binder n f a -> n
binderInfo = ubInfo . unBinder

binderBody :: Binder n f a -> Scope n f a
binderBody = ubBody . unBinder

binderFreeVars :: (Foldable f, Ord a) => Binder n f a -> Set a
binderFreeVars = scopeFreeVars . binderBody

-- binderMapInfo :: Functor f => (n -> o) -> Binder n f a -> Binder o f a
-- binderMapInfo f (Binder (UnderBinder i x b)) = Binder (UnderBinder i (f x) (scopeMapInfo f b))

-- binderTraverseInfo :: (Traversable f, Applicative m) => (n -> m o) -> Binder n f a -> m (Binder o f a)
-- binderTraverseInfo f (Binder (UnderBinder i x b)) = (\y c -> Binder (UnderBinder i y c)) <$> f x <*> scopeTraverseInfo f b

transformBinder :: Functor f => BottomUp n f g a -> Binder n f a -> Binder n g a
transformBinder t (Binder (UnderBinder i x b)) = Binder (UnderBinder i x (transformScope t b))

-- Abstraction and instantiation

subAbstract :: (Functor f, Eq a) => Int -> n -> Seq a -> Scope n f a -> Binder n f a
subAbstract n x ks s = Binder (UnderBinder n x (scopeBindOpt 0 s ((boundVarScope <$>) . flip Seq.elemIndexL ks)))

-- TODO combine info on sub?
subInstantiate :: Functor f => Int -> Seq (Scope n f a) -> Scope n f a -> Scope n f a
subInstantiate n vs s@(Scope us) =
    case us of
        ScopeB b -> fromMaybe s (vs Seq.!? (b - n))
        ScopeF _ -> s
        ScopeA (UnderBinder i x e) -> Scope (ScopeA (UnderBinder i x (subInstantiate (n + i) (scopeShift i <$> vs) e)))
        ScopeE fe -> Scope (ScopeE (subInstantiate n vs <$> fe))

abstract :: (Functor f, Eq a) => n -> Seq a -> Scope n f a -> Binder n f a
abstract x ks = let n = Seq.length ks in subAbstract n x ks . scopeShift n

instantiate :: Functor f => Seq (Scope n f a) -> Scope n f a -> Scope n f a
instantiate = subInstantiate 0

rawApply :: (ThrowSub m, Applicative m, Functor f) => Seq (Scope n f a) -> Int -> Scope n f a -> m (Scope n f a)
rawApply vs i e =
    let len = Seq.length vs
    in if len == i
        then pure (scopeShift (-1) (instantiate vs e))
        else throwSub (ApplyError len i)

apply :: (ThrowSub m, Applicative m, Functor f)  => Seq (Scope n f a) -> Binder n f a -> m (Scope n f a)
apply vs (Binder (UnderBinder i _ e)) = rawApply vs i e

abstract1 :: (Functor f, Eq a) => n -> a -> Scope n f a -> Binder n f a
abstract1 n k = abstract n (Seq.singleton k)

instantiate1 :: Functor f => Scope n f a -> Scope n f a -> Scope n f a
instantiate1 v = instantiate (Seq.singleton v)

apply1 :: (ThrowSub m, Applicative m, Functor f) => Scope n f a -> Binder n f a -> m (Scope n f a)
apply1 v = apply (Seq.singleton v)

-- ScopeFold

data ScopeFold n f a r = ScopeFold
    { sfBound   :: Int -> r
    , sfFree    :: a -> r
    , sfBinder  :: Binder n f a -> r
    , sfFunctor :: f (Scope n f a) -> r
    } deriving (Generic, Functor)

-- contramapFold :: (x -> y) -> ScopeFold n f a r -> ScopeFold x n f a r
-- contramapFold f (ScopeFold bound free binder functor) = undefined

transformFold :: Functor f => BottomUp n f g a -> ScopeFold n g a r -> ScopeFold n f a r
transformFold t (ScopeFold bound free binder functor) = ScopeFold bound free (binder . transformBinder t) (functor . t . (transformScope t <$>))

-- boundFold :: ThrowSub m => (a -> m r) -> (Binder n f a -> m r) -> (f (Scope n f a) -> m r) -> ScopeFold n f a (m r)
-- boundFold = ScopeFold (const (throwSub . UnboundError))

foldScope :: ScopeFold n f a r -> Scope n f a -> r
foldScope (ScopeFold bound free binder functor) (Scope us) =
    case us of
        ScopeB b  -> bound b
        ScopeF a  -> free a
        ScopeA ub -> binder (Binder ub)
        ScopeE fe -> functor fe

-- Name

data Name n a = Name { nameKey :: n, nameValue :: a } deriving (Generic, Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Name n a) where
    Name _ x == Name _ y = x == y

type NameOnly n = Name n ()
