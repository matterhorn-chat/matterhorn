module Matterhorn.Events.ThemeListOverlay where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.ThemeListOverlay
import           Matterhorn.State.ListOverlay
import           Matterhorn.Types


onEventThemeListOverlay :: Vty.Event -> MH ()
onEventThemeListOverlay =
    void . onEventListOverlay csThemeListOverlay themeListOverlayKeybindings

-- | The keybindings we want to use while viewing a user list overlay
themeListOverlayKeybindings :: KeyConfig -> KeyHandlerMap
themeListOverlayKeybindings = mkKeybindings themeListOverlayKeyHandlers

themeListOverlayKeyHandlers :: [KeyEventHandler]
themeListOverlayKeyHandlers =
    [ mkKb CancelEvent "Close the theme list" (exitListOverlay csThemeListOverlay)
    , mkKb SearchSelectUpEvent "Select the previous theme" themeListSelectUp
    , mkKb SearchSelectDownEvent "Select the next theme" themeListSelectDown
    , mkKb PageDownEvent "Page down in the theme list" themeListPageDown
    , mkKb PageUpEvent "Page up in the theme list" themeListPageUp
    , mkKb ActivateListItemEvent "Switch to the selected color theme" (listOverlayActivateCurrent csThemeListOverlay)
    ]
