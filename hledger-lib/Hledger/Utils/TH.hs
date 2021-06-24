module Hledger.Utils.TH
( makeClassyLensesTrailing
) where

import Data.List.Extra (unsnoc)
import Language.Haskell.TH (mkName, nameBase)
import Lens.Micro ((&), (.~))
import Lens.Micro.TH (DefName(TopName), lensField, makeLensesWith, classyRules)

makeClassyLensesTrailing x = flip makeLensesWith x $
  classyRules & lensField .~ (\_ _ n -> case unsnoc (nameBase n) of
      Just (name, '_') -> [TopName (mkName name)]
      _                -> []
      )
