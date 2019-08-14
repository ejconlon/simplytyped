{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module SimplyTyped.Tree where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import SimplyTyped.Blanks.Scope
import SimplyTyped.Prelude
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import Text.Read (readMaybe)

newtype TreeParser a =
  TreeParser
    { unTreeParser :: Seq a
    }
  deriving (Functor, Applicative, Monad, Alternative)

parseRead :: (Alternative m, Read a) => Atom -> m a
parseRead (Atom t) = maybe empty pure (readMaybe (Text.unpack t))

parseNat :: Alternative m => Atom -> m Int
parseNat = parseRead

parseLeaf :: Alternative m => (Atom -> m a) -> (Tree -> m a)
parseLeaf f t =
  case t of
    Leaf a -> f a
    _ -> empty

showAtom :: Show a => a -> Atom
showAtom = Atom . Text.pack . show

newtype TreeIdent =
  TreeIdent
    { unTreeIdent :: Text
    }
  deriving (Generic, Eq, Ord, Show, IsString)

type DepState = Map TreeIdent TreeDef

newtype DepCollector a =
  DepCollector
    { unDepCollector :: State DepState a
    }
  deriving (Functor, Applicative, Monad, MonadState DepState)

crawlDep :: TreeProof -> DepCollector ()
crawlDep (TreeProof p) = do
  let n = refTree p
  m <- get
  if Map.member n m
    then pure ()
    else do
      let d = defineTree p
      modify (Map.insert n d)
      let es = depsTree p
      for_ es crawlDep

runDepCollector :: DepCollector () -> Map TreeIdent TreeDef
runDepCollector s = execState (unDepCollector s) Map.empty

runCrawlDeps :: Treeable a => Proxy a -> Map TreeIdent TreeDef
runCrawlDeps p = runDepCollector (for_ (depsTree p) crawlDep)

class Treeable a where
  refTree :: Proxy a -> TreeIdent
  defineTree :: Proxy a -> TreeDef
  depsTree :: Proxy a -> Seq TreeProof
  parseTree :: Proxy a -> Tree -> TreeParser a
  renderTree :: a -> Tree

data TreeProof where
  TreeProof :: Treeable a => Proxy a -> TreeProof

newtype Atom =
  Atom
    { unAtom :: Text
    }
  deriving (Generic, Eq, Ord, Show, IsString)

data LeafMatcher
  = LeafIdent
  | LeafNat
  | LeafKeyword Atom
  deriving (Generic, Eq, Show)

data BranchMatcher
  = BranchWild
  | BranchFixed (Seq TreeDef)
  | BranchRepeated TreeDef
  | BranchTagged LeafMatcher BranchMatcher
  deriving (Generic, Eq, Show)

data TreeDef
  = LeafDef LeafMatcher
  | BranchDef BranchMatcher
  | ChoiceDef (Seq TreeDef)
  | FixDef Atom TreeIdent
  | RefDef TreeIdent
  deriving (Generic, Eq, Show)

data Tree
  = Leaf Atom
  | Branch (Seq Tree)
  deriving (Generic, Eq, Show)

showTree :: Tree -> Text
showTree (Leaf (Atom l)) = l
showTree (Branch ts) = "(" <> Text.intercalate " " (showTree <$> toList ts) <> ")"

showTreeable :: Treeable a => a -> Text
showTreeable = showTree . renderTree

type TextParser = MP.Parsec Void Text

spaceConsumer :: TextParser ()
spaceConsumer = MPCL.space MPC.space1 lineCmnt blockCmnt
  where
    lineCmnt = MPCL.skipLineComment ";"
    blockCmnt = MPCL.skipBlockComment "#|" "|#"

lexeme :: TextParser a -> TextParser a
lexeme = MPCL.lexeme spaceConsumer

symbol :: Text -> TextParser Text
symbol = MPCL.symbol spaceConsumer

parens :: TextParser a -> TextParser a
parens = MP.between (symbol "(") (symbol ")")

nonDelimPred :: Char -> Bool
nonDelimPred c = c /= '(' && c /= ')' && c /= ' ' && c /= '\t' && c /= '\n'

atomParser :: TextParser Atom
atomParser = Atom <$> lexeme (MP.try (MP.takeWhile1P Nothing nonDelimPred))

leafParser :: TextParser Tree
leafParser = Leaf <$> atomParser

branchParser :: TextParser Tree
branchParser = Branch <$> (Seq.fromList <$> parens (MP.many (spaceConsumer *> treeParser)))

treeParser :: TextParser Tree
treeParser = leafParser <|> branchParser

readTree :: Text -> Maybe Tree
readTree = MP.parseMaybe treeParser

readTreeable :: Treeable a => Text -> Seq a
readTreeable t =
  case readTree t of
    Nothing -> Seq.empty
    Just tt -> unTreeParser (parseTree (Proxy :: Proxy a) tt)

data NoParseError =
  NoParseError
  deriving (Eq, Show, Typeable)

instance Exception NoParseError

data AmbiguityError a =
  AmbiguityError
    { options :: Seq a
    }
  deriving (Eq, Show, Typeable)

instance (Show a, Typeable a) => Exception (AmbiguityError a)

easyReadTreeable :: (MonadThrow m, Treeable a, Show a, Typeable a) => Proxy a -> Text -> m a
easyReadTreeable _ t =
  case readTreeable t of
    Empty -> throwM NoParseError
    a :<| Empty -> pure a
    as -> throwM (AmbiguityError as)

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
          _ -> empty
      parseFree =
        case t of
          Branch [Leaf "free", Leaf ta] -> Scope . UnderFreeScope . FreeScope <$> parseRead ta
          _ -> empty
      parseBinder =
        case t of
          Branch [Leaf "binder", Leaf ti, tx, te] ->
            let mi = parseNat ti
                mx = parseTree (Proxy :: Proxy n) tx
                me = parseTree p te
            in (\i x e -> Scope (UnderBinderScope (BinderScope i x e))) <$> mi <*> mx <*> me
          _ -> empty
      parseEmbed =
        case t of
          Branch [Leaf "embed", te] ->
            let me = parseTree (Proxy :: Proxy (f (Scope n f a))) te
            in Scope . UnderEmbedScope . EmbedScope <$> me
          _ -> empty
  renderTree (Scope us) =
    case us of
      UnderBoundScope (BoundScope b) -> Branch [Leaf "bound", Leaf (showAtom b)]
      UnderFreeScope (FreeScope a) -> Branch [Leaf "free", Leaf (showAtom a)]
      UnderBinderScope (BinderScope i x e) -> Branch [Leaf "binder", Leaf (showAtom i), renderTree x, renderTree e]
      UnderEmbedScope (EmbedScope fe) -> Branch [Leaf "embed", renderTree fe]
