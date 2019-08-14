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

data Kont = KontIf Term Term | KontPred | KontSucc | KontIsZero deriving (Eq, Show)

data Step = Value Term | Next Term | Stuck | Redex Term Kont deriving (Eq, Show)

reducePred :: Term -> Step
reducePred t =
  case t of
    TmZero -> Value TmZero
    TmSucc t' -> Value t'
    _ -> Stuck

reduceIsZero :: Term -> Step
reduceIsZero t =
  case t of
    TmZero -> Value TmTrue
    TmSucc _ -> Value TmFalse
    _ -> Stuck

reduceKont :: Kont -> Term -> Step
reduceKont k t1 =
  case k of
    KontIf t2 t3 ->
      case t1 of
        TmTrue -> Next t2
        TmFalse -> Next t3
        _ -> Stuck
    KontSucc -> Value (TmSucc t1)
    KontPred -> reducePred t1
    KontIsZero -> reduceIsZero t1

smallHoleStep :: Term -> Step
smallHoleStep t =
  case t of
    TmIf t1 t2 t3 -> Redex t1 (KontIf t2 t3)
    TmSucc t1 -> Redex t1 KontSucc
    TmPred t1 -> Redex t1 KontPred
    TmIsZero t1 -> Redex t1 KontIsZero
    _ -> Value t

bigHoleStep :: Term -> Maybe Term
bigHoleStep t = go (smallHoleStep t) where
  go s =
    case s of
      Value t' -> Just t'
      Next t' -> bigHoleStep t'
      Stuck -> Nothing
      Redex t' k -> bigHoleStep t' >>= go . reduceKont k
