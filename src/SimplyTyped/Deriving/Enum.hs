{-# LANGUAGE ScopedTypeVariables #-}

module SimplyTyped.Deriving.Enum where

import qualified Data.Sequence as Seq
import SimplyTyped.Prelude
import SimplyTyped.Tree

class EnumWrapper a where
  enumRefTree :: Proxy a -> TreeIdent
  enumToValueKeyword :: a -> Atom
  enumFromValueKeyword :: Proxy a -> Atom -> Maybe a

newtype EnumWrapperTreeable a =
  EnumWrapperTreeable
    { unEnumWrapperTreeable :: a
    }

enumValues :: (Enum a, Bounded a) => Proxy a -> Seq a
enumValues _ = Seq.fromList (enumFrom minBound)

instance (Enum a, Bounded a, EnumWrapper a) => Treeable (EnumWrapperTreeable a) where
  refTree _ = enumRefTree (Proxy :: Proxy a)
  defineTree _ = ChoiceDef (LeafDef . LeafKeyword . enumToValueKeyword <$> enumValues (Proxy :: Proxy a))
  depsTree _ = Seq.empty
  parseTree _ t =
    case t of
      Leaf n -> maybe empty (pure . EnumWrapperTreeable) (enumFromValueKeyword (Proxy :: Proxy a) n)
      _ -> empty
  renderTree = Leaf . enumToValueKeyword . unEnumWrapperTreeable
