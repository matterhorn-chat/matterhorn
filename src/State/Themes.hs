module State.Themes
  (
    listThemes
  , setTheme
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Themes ( themeToAttrMap )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (.=) )

import           State.Common
import           Themes
import           Types


listThemes :: MH ()
listThemes = do
    let themeList = T.intercalate "\n\n" $
                    "Available built-in themes:" :
                    (("  " <>) <$> internalThemeName <$> internalThemes)
    postInfoMessage themeList

setTheme :: Text -> MH ()
setTheme name =
    case lookupTheme name of
        Nothing -> listThemes
        Just it -> csResources.crTheme .=
            (themeToAttrMap $ internalTheme it)

