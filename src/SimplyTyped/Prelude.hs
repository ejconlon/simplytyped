module SimplyTyped.Prelude
  ( module Prelude
  , Alternative(..)
  , Catch
  , Exception
  , Generic
  , Identity(..)
  , IsString
  , MonadCatch(..)
  , MonadReader(..)
  , MonadState(..)
  , MonadThrow(..)
  , Proxy(..)
  , Reader
  , ReaderT(..)
  , Seq(..)
  , SomeException(..)
  , State
  , StateT
  , Text
  , Typeable
  , Void
  , (&)
  , ap
  , asum
  , execState
  , for_
  , modify
  ) where

import Control.Applicative (Alternative(..))
import Control.Exception (Exception, SomeException(..))
import Control.Monad (ap)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Catch.Pure (Catch)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Reader (MonadReader(..), Reader, ReaderT(..))
import Control.Monad.State (MonadState(..), State, StateT(..), execState, modify)
import Data.Foldable (asum, for_)
import Data.Function ((&))
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq(..))
import Data.String (IsString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Prelude
