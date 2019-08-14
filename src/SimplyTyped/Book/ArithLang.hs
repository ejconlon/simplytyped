module SimplyTyped.Book.ArithLang where

import Prelude

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

hasRedex :: Term -> Bool
hasRedex t =
  case t of
    TmIf _ _ _ -> True
    TmSucc t1 -> hasRedex t1
    TmPred _ -> True
    TmIsZero _ -> True
    _ -> False

isNumericVal :: Term -> Bool
isNumericVal t =
  case t of
    TmZero -> True
    TmSucc t1 -> isNumericVal t1
    _ -> False

smallStep :: Term -> Maybe Term
smallStep t =
  case t of
    TmIf t1 t2 t3 ->
      case t1 of
        TmTrue -> Just t2
        TmFalse -> Just t3
        _ -> (\t1' -> TmIf t1' t2 t3) <$> smallStep t1
    TmSucc t1 -> TmSucc <$> smallStep t1
    TmPred t1 ->
      if hasRedex t1
        then TmPred <$> smallStep t1
        else case t1 of
          TmZero -> Just TmZero
          TmSucc t1' ->
            if isNumericVal t1'
              then Just t1'
              else Nothing
          _ -> Nothing
    TmIsZero t1 ->
      if hasRedex t1
        then TmIsZero <$> smallStep t1
        else case t1 of
          TmZero -> Just TmTrue
          TmSucc t1' ->
            if isNumericVal t1'
              then Just TmFalse
              else Nothing
          _ -> Nothing
    _ -> Nothing

bigStep :: Term -> Term
bigStep t =
  case smallStep t of
    Just t' -> bigStep t'
    Nothing -> t
