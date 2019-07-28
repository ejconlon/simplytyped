module SimplyTyped.Lenses
  ( makeLenses
  , makePrisms
  ) where

import Control.Lens ((.~))
import Control.Lens.TH (DefName(TopName), lensField, lensRules, makeLensesWith, makePrisms)
import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)
import SimplyTyped.Prelude

makeLenses :: Name -> DecsQ
makeLenses =
  makeLensesWith $
  lensRules &
  lensField .~
  (\_ _ n ->
     let n' = nameBase n
      in case n' of
           '_':_ -> error ("Field name must not start with underscore: " ++ n')
           _ -> [TopName (mkName ('_' : n'))])
