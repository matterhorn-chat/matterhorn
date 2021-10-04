module Matterhorn.Events.ThemeListOverlay where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.ThemeListOverlay
import           Matterhorn.State.ListOverlay
import           Matterhorn.Types


onEventThemeListOverlay :: TeamId -> Vty.Event -> MH ()
onEventThemeListOverlay tId =
    void . onEventListOverlay tId (csTeam(tId).tsThemeListOverlay)
        (themeListOverlayKeybindings tId)

-- | The keybindings we want to use while viewing a user list overlay
themeListOverlayKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
themeListOverlayKeybindings tId = mkKeybindings (themeListOverlayKeyHandlers tId)

themeListOverlayKeyHandlers :: TeamId -> [KeyEventHandler]
themeListOverlayKeyHandlers tId =
    [ mkKb CancelEvent "Close the theme list"
      (exitListOverlay tId (csTeam(tId).tsThemeListOverlay))
    , mkKb SearchSelectUpEvent "Select the previous theme"
      themeListSelectUp
    , mkKb SearchSelectDownEvent "Select the next theme"
      themeListSelectDown
    , mkKb PageDownEvent "Page down in the theme list"
      themeListPageDown
    , mkKb PageUpEvent "Page up in the theme list"
      themeListPageUp
    , mkKb ActivateListItemEvent "Switch to the selected color theme"
      (listOverlayActivateCurrent tId (csTeam(tId).tsThemeListOverlay))
    ]
