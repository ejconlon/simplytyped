module SimplyTyped.Tree where

import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import SimplyTyped.Prelude
import qualified Text.Megaparsec            as MP
import qualified Text.Megaparsec.Char       as MPC
import qualified Text.Megaparsec.Char.Lexer as MPCL
import Text.Read (readMaybe)

newtype TreeParser a = TreeParser { unTreeParser :: Seq a }
  deriving (Functor, Applicative, Monad, Alternative)

parseFail :: TreeParser a
parseFail = TreeParser Seq.empty

parseRead :: Read a => Text -> TreeParser a
parseRead t = maybe parseFail pure (readMaybe (Text.unpack t))

parseNat :: Text -> TreeParser Int
parseNat = parseRead

type TreeIdent = Text

class Treeable a where
  refTree :: Proxy a -> TreeIdent
  defineTree :: Proxy a -> TreeDef
  depsTree :: Proxy a -> Map TreeIdent TreeDef
  parseTree :: Proxy a -> Tree -> TreeParser a
  renderTree :: a -> Tree
  selfDepsTree :: Proxy a -> Map TreeIdent TreeDef
  selfDepsTree p = Map.insert (refTree p) (defineTree p) (depsTree p)

type Atom = Text

data LeafMatcher =
      LeafIdent
    | LeafNat
    | LeafKeyword Atom
    deriving (Generic, Eq, Show)

data BranchMatcher =
      BranchWild
    | BranchFixed (Seq TreeDef)
    | BranchRepeated TreeDef deriving (Generic, Eq, Show)

data TreeDef =
      LeafDef LeafMatcher
    | BranchDef BranchMatcher
    | ChoiceDef (Seq TreeDef)
    | FixDef TreeIdent TreeDef
    | RefDef TreeIdent
    deriving (Generic, Eq, Show)

data Tree =
      Leaf Atom
    | Branch (Seq Tree)
    deriving (Generic, Eq, Show)

showTree :: Tree -> Text
showTree (Leaf l) = l
showTree (Branch ts) = "(" <> Text.intercalate " " (showTree <$> toList ts) <> ")"

showTreeable :: Treeable a => a -> Text
showTreeable = showTree . renderTree

-- TODO
mergeDepTrees :: Seq (Map TreeIdent TreeDef) -> Map TreeIdent TreeDef
mergeDepTrees = undefined

data Anno = Anno { annoStart :: MP.SourcePos, annoEnd :: MP.SourcePos }
    deriving (Generic, Eq, Show)

type TextParser = MP.Parsec Void Text

spaceConsumer :: TextParser ()
spaceConsumer = MPCL.space MPC.space1 lineCmnt blockCmnt
    where
    lineCmnt  = MPCL.skipLineComment ";"
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
atomParser = lexeme (MP.try (MP.takeWhile1P Nothing nonDelimPred))

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
