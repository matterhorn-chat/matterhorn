module Matterhorn.State.ThemeListOverlay
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

import           Matterhorn.State.ListOverlay
import           Matterhorn.Themes
import           Matterhorn.Types


-- | Show the user list overlay with the given search scope, and issue a
-- request to gather the first search results.
enterThemeListMode :: MH ()
enterThemeListMode =
    enterListOverlayMode (csCurrentTeam.tsThemeListOverlay)
        ThemeListOverlay () setInternalTheme getThemesMatching

-- | Move the selection up in the user list overlay by one user.
themeListSelectUp :: MH ()
themeListSelectUp = themeListMove L.listMoveUp

-- | Move the selection down in the user list overlay by one user.
themeListSelectDown :: MH ()
themeListSelectDown = themeListMove L.listMoveDown

-- | Move the selection up in the user list overlay by a page of users
-- (themeListPageSize).
themeListPageUp :: MH ()
themeListPageUp = themeListMove (L.listMoveBy (-1 * themeListPageSize))

-- | Move the selection down in the user list overlay by a page of users
-- (themeListPageSize).
themeListPageDown :: MH ()
themeListPageDown = themeListMove (L.listMoveBy themeListPageSize)

-- | Transform the user list results in some way, e.g. by moving the
-- cursor, and then check to see whether the modification warrants a
-- prefetch of more search results.
themeListMove :: (L.List Name InternalTheme -> L.List Name InternalTheme) -> MH ()
themeListMove = listOverlayMove (csCurrentTeam.tsThemeListOverlay)

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

setInternalTheme :: InternalTheme -> MH Bool
setInternalTheme t = do
    setTheme $ internalThemeName t
    return False

setTheme :: Text -> MH ()
setTheme name =
    case lookupTheme name of
        Nothing -> enterThemeListMode
        Just it -> do
            mh invalidateCache
            csResources.crTheme .= (themeToAttrMap $ internalTheme it)
