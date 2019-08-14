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

data Kont = KontTop | KontIf Term Term Kont | KontPred Kont | KontSucc Kont | KontIsZero Kont deriving (Eq, Show)

data Step = Apply Term Kont | Redex Term Kont deriving (Eq, Show)

reduceKont :: Kont -> Term -> Maybe Step
reduceKont k t1 =
  case k of
    KontTop -> Nothing
    KontIf t2 t3 k' ->
      case t1 of
        TmTrue -> Just (Redex t2 k')
        TmFalse -> Just (Redex t3 k')
        _ -> Nothing
    KontSucc k' ->
      case t1 of
        TmZero -> Just (Apply (TmSucc t1) k')
        TmSucc _ -> Just (Apply (TmSucc t1) k')
        _ -> Nothing
    KontPred k' ->
      case t1 of
        TmZero -> Just (Apply TmZero k')
        TmSucc t1' -> Just (Apply t1' k')
        _ -> Nothing
    KontIsZero k' ->
      case t1 of
        TmZero -> Just (Apply TmTrue k')
        TmSucc _ -> Just (Apply TmFalse k')
        _ -> Nothing

smallHoleStep :: Kont -> Term -> Step
smallHoleStep k t =
  case t of
    TmIf t1 t2 t3 -> Redex t1 (KontIf t2 t3 k)
    TmSucc t1 -> Redex t1 (KontSucc k)
    TmPred t1 -> Redex t1 (KontPred k)
    TmIsZero t1 -> Redex t1 (KontIsZero k)
    _ -> Apply t k

initStep :: Term -> Step
initStep t = Redex t KontTop

nextStep :: Step -> Maybe Step
nextStep s =
  case s of
    Apply t k -> reduceKont k t
    Redex t k -> Just (smallHoleStep k t)

bigHoleStep :: Term -> Maybe Term
bigHoleStep t = go (initStep t) where
  go s =
    case s of
      Apply t' KontTop -> Just t'
      _ -> nextStep s >>= go
