module SimplyTyped.Prelude
  ( module Prelude
  , Alternative(..)
  , Catch
  , Exception
  , Generic
  , MonadCatch(..)
  , MonadReader(..)
  , MonadThrow(..)
  , Proxy(..)
  , Reader
  , ReaderT(..)
  , Seq(..)
  , Text
  , Typeable
  , Void
  , ap
  ) where

import Control.Applicative (Alternative(..))
import Control.Exception (Exception)
import Control.Monad (ap)
import Control.Monad.Catch (MonadCatch(..), MonadThrow(..))
import Control.Monad.Catch.Pure (Catch)
import Control.Monad.Reader (MonadReader(..), Reader, ReaderT(..))
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq(..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Prelude
