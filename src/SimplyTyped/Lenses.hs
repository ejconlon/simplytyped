module SimplyTyped.Lenses
  ( makeFieldLenses
  , makeLenses
  , makePrisms
  ) where

import Control.Lens ((.~))
import Control.Lens.TH (DefName(TopName), lensField, lensRules, makeLenses, makeLensesWith, makePrisms)
import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)
import SimplyTyped.Prelude

-- TODO eh just scrap for the default?
makeFieldLenses :: Name -> DecsQ
makeFieldLenses =
  makeLensesWith $
  lensRules &
  lensField .~
  (\_ _ n ->
     let n' = nameBase n
      in case n' of
           '_':_ -> error ("Field name must not start with underscore: " ++ n')
           _ -> [TopName (mkName ('_' : n'))])
