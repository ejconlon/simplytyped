module SimplyTyped.Versions where

import Data.Map (Map)
import Data.Set (Set)
import SimplyTyped.Prelude

newtype PackageName = PackageName { unPackageName :: Text } deriving (Show, Eq, Ord, Generic, IsString)

newtype Version = Version { unVersion :: Text } deriving (Show, Eq, Ord, Generic, IsString)

data Bounds = Bounds deriving (Show, Eq, Generic)

newtype Meta = Meta { unMeta :: Map PackageName Bounds } deriving (Show, Eq, Generic)

newtype Repo = Repo { unRepo :: Map PackageName Meta } deriving (Show, Eq, Generic)

newtype Solution = Solution { unSolution :: Map PackageName Version } deriving (Show, Eq, Generic)

solve :: Set PackageName -> Repo -> Solution
solve = undefined
