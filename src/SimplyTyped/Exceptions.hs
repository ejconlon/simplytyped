module SimplyTyped.Exceptions where

import Data.Maybe (fromMaybe, isJust)
import Data.Typeable (cast)
import SimplyTyped.Prelude

-- TODO use type families to simplify this
-- SomeException -> unwrap
-- otherwise -> use exception itself

data TyProof where
  TyProof :: Typeable y => y -> TyProof

someTyProof :: SomeException -> TyProof
someTyProof (SomeException e) = TyProof e

exactTyProof :: Typeable e => e -> TyProof
exactTyProof = TyProof

data TyProxy where
  TyProxy :: Typeable x => Proxy x -> TyProxy

tyProxy :: Typeable x => Proxy x -> TyProof -> Maybe x
tyProxy _ (TyProof y) = cast y

tyMatch :: Typeable x => Proxy x -> TyProof -> Bool
tyMatch p i = isJust (tyProxy p i)

runTyProxy :: TyProxy -> TyProof -> Bool
runTyProxy (TyProxy p) y = isJust (tyProxy p y)

runTyProxies :: Foldable t => t TyProxy -> TyProof -> Bool
runTyProxies hs i = any (flip runTyProxy i) hs

tyHandle :: Typeable x => Proxy x -> (x -> a) -> TyProof -> Maybe a
tyHandle p f i = f <$> tyProxy p i

data TyHandler a where
  TyHandler :: Typeable x => Proxy x -> (x -> a) -> TyHandler a

runTyHandler :: TyHandler a -> TyProof -> Maybe a
runTyHandler (TyHandler p f) = tyHandle p f

runTyHandlers :: (Functor t, Foldable t) => t (TyHandler a) -> TyProof -> Maybe a
runTyHandlers hs i = asum ((flip runTyHandler i) <$> hs)

-- isExcProxy :: (Typeable x, Typeable y) => Proxy x -> y -> Bool
-- isExcProxy p y = isJust (castExcProxy p y)

-- isExcTyProxy :: Typeable y => TyProxy -> y -> Bool
-- isExcTyProxy (TyProxy p) y = isExcProxy p y

-- isExcInTyProxies :: (Typeable y, Foldable t) => t TyProxy -> y -> Bool
-- isExcInTyProxies ps y = any (flip isExcTyProxy y) ps

-- catchPred :: (MonadCatch m, Exception e) => (e -> Bool) -> m a -> (e -> m a) -> m a
-- catchPred epred action handler = do
--   catch action $ \exc -> do
--     if epred exc
--       then handler exc
--       else throwM exc

catchHandler :: MonadCatch m => m a -> (SomeException -> Maybe (m a)) -> m a
catchHandler action handler =
  catch action $ \exc -> fromMaybe (throwM exc) (handler exc)

-- catchAs :: (MonadCatch m, Exception e) => Proxy e -> m a -> (e -> m a) -> m a
-- catchAs _ action handler = catch action handler
