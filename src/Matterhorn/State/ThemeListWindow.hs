module Matterhorn.State.ThemeListWindow
  ( enterThemeListMode

  , themeListSelectDown
  , themeListSelectUp
  , themeListPageDown
  , themeListPageUp

  , setTheme
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( invalidateCache )
import           Brick.Themes ( themeToAttrMap )
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vec
import           Lens.Micro.Platform ( (.=) )

import           Network.Mattermost.Types

import           Matterhorn.State.ListWindow
import           Matterhorn.Themes
import           Matterhorn.Types


-- | Show the user list window with the given search scope, and issue a
-- request to gather the first search results.
enterThemeListMode :: TeamId -> MH ()
enterThemeListMode tId =
    enterListWindowMode tId (csTeam(tId).tsThemeListWindow)
        ThemeListWindow () (setInternalTheme tId) getThemesMatching

-- | Move the selection up in the user list window by one user.
themeListSelectUp :: TeamId -> MH ()
themeListSelectUp tId = themeListMove tId L.listMoveUp

-- | Move the selection down in the user list window by one user.
themeListSelectDown :: TeamId -> MH ()
themeListSelectDown tId = themeListMove tId L.listMoveDown

-- | Move the selection up in the user list window by a page of users
-- (themeListPageSize).
themeListPageUp :: TeamId -> MH ()
themeListPageUp tId = themeListMove tId (L.listMoveBy (-1 * themeListPageSize))

-- | Move the selection down in the user list window by a page of users
-- (themeListPageSize).
themeListPageDown :: TeamId -> MH ()
themeListPageDown tId = themeListMove tId (L.listMoveBy themeListPageSize)

-- | Transform the user list results in some way, e.g. by moving the
-- cursor, and then check to see whether the modification warrants a
-- prefetch of more search results.
themeListMove :: TeamId -> (L.List Name InternalTheme -> L.List Name InternalTheme) -> MH ()
themeListMove tId = listWindowMove (csTeam(tId).tsThemeListWindow)

-- | The number of users in a "page" for cursor movement purposes.
themeListPageSize :: Int
themeListPageSize = 10

getThemesMatching :: ()
                  -> Session
                  -> Text
                  -> IO (Vec.Vector InternalTheme)
getThemesMatching _ _ searchString = do
    let matching = filter matches internalThemes
        search = T.toLower searchString
        matches t = search `T.isInfixOf` T.toLower (internalThemeName t) ||
                    search `T.isInfixOf` T.toLower (internalThemeDesc t)
    return $ Vec.fromList matching

setInternalTheme :: TeamId -> InternalTheme -> MH Bool
setInternalTheme tId t = do
    setTheme tId $ internalThemeName t
    return False

setTheme :: TeamId -> Text -> MH ()
setTheme tId name =
    case lookupTheme name of
        Nothing -> enterThemeListMode tId
        Just it -> do
            mh invalidateCache
            csResources.crTheme .= (themeToAttrMap $ internalTheme it)
            csResources.crThemeOriginal .= internalTheme it
