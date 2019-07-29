module SimplyTyped.Tree where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map

-- import qualified Data.Map.Merge.Lazy as Merge
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
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

parseFail :: TreeParser a
parseFail = TreeParser Seq.empty

parseRead :: Read a => Atom -> TreeParser a
parseRead (Atom t) = maybe parseFail pure (readMaybe (Text.unpack t))

parseNat :: Atom -> TreeParser Int
parseNat = parseRead

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

-- mergeDepTrees :: Map TreeIdent TreeDef -> Map TreeIdent TreeDef -> Map TreeIdent TreeDef
-- mergeDepTrees = merge where
--   merge = Merge.merge Merge.preserveMissing Merge.preserveMissing (Merge.zipWithMatched match)
--   match k x y =
--     if x == y
--       then x
--       else error ("Dep mismatch " <> show k <> ": " <> show x <> " vs " <> show y)
-- crawlDepTree :: TreeProof -> Map TreeIdent TreeDef -> Map TreeIdent TreeDef
-- crawlDepTree (TreeProof p) m =
--   let n = refTree p
--   in if Map.member n m
--     then m
--     else mergeDepTrees m (selfDepsTree p)
-- crawlAllDepTrees :: Seq TreeProof -> Map TreeIdent TreeDef
-- crawlAllDepTrees = foldr crawlDepTree Map.empty
data Anno =
  Anno
    { annoStart :: MP.SourcePos
    , annoEnd :: MP.SourcePos
    }
  deriving (Generic, Eq, Show)

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
