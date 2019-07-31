module SimplyTyped.Book.BoolLang where

import Prelude

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving (Eq, Show)

smallStep :: Term -> Maybe Term
smallStep t =
  case t of
    TmIf t1 t2 t3 ->
      case t1 of
        TmTrue -> Just t2
        TmFalse -> Just t3
        _ -> (\t1' -> TmIf t1' t2 t3) <$> smallStep t1
    _ -> Nothing
