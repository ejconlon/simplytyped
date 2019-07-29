module SimplyTyped.Exceptions where

import Control.Exception (toException)
import Data.Maybe (fromMaybe, isJust)
import Data.Typeable (cast)
import SimplyTyped.Prelude

data TyProof where
  TyProof :: Typeable y => y -> TyProof

mkTyProof :: Exception e => e -> TyProof
mkTyProof = someTyProof . toException

someTyProof :: SomeException -> TyProof
someTyProof (SomeException e) = TyProof e

data TyProxy where
  TyProxy :: Typeable x => Proxy x -> TyProxy

tyProxy :: Typeable x => Proxy x -> TyProof -> Maybe x
tyProxy _ (TyProof y) = cast y

tyMatch :: Typeable x => Proxy x -> TyProof -> Bool
tyMatch p i = isJust (tyProxy p i)

runTyProxy :: TyProxy -> TyProof -> Bool
runTyProxy (TyProxy p) y = isJust (tyProxy p y)

runTyProxies :: Foldable t => t TyProxy -> TyProof -> Bool
runTyProxies hs i = any (`runTyProxy` i) hs

tyHandle :: Typeable x => Proxy x -> (x -> a) -> TyProof -> Maybe a
tyHandle p f i = f <$> tyProxy p i

data TyHandler a where
  TyHandler :: Typeable x => Proxy x -> (x -> a) -> TyHandler a

runTyHandler :: TyHandler a -> TyProof -> Maybe a
runTyHandler (TyHandler p f) = tyHandle p f

runTyHandlers :: (Functor t, Foldable t) => t (TyHandler a) -> TyProof -> Maybe a
runTyHandlers hs i = asum (flip runTyHandler i <$> hs)

catchHandler :: (MonadCatch m, Exception e) => m a -> (e -> Maybe (m a)) -> m a
catchHandler action handler = catch action $ \exc -> fromMaybe (throwM exc) (handler exc)
