module Matterhorn.Events.ThemeListWindow where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.ThemeListWindow
import           Matterhorn.State.ListWindow
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents


onEventThemeListWindow :: TeamId -> Vty.Event -> MH ()
onEventThemeListWindow tId =
    void . onEventListWindow (csTeam(tId).tsThemeListWindow)
        (themeListWindowKeybindings tId)

-- | The keybindings we want to use while viewing a user list window
themeListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
themeListWindowKeybindings tId = mkKeybindings (themeListWindowKeyHandlers tId)

themeListWindowKeyHandlers :: TeamId -> [KeyEventHandler KeyEvent MH]
themeListWindowKeyHandlers tId =
    [ mkKb CancelEvent "Close the theme list"
      (exitListWindow tId (csTeam(tId).tsThemeListWindow))
    , mkKb SearchSelectUpEvent "Select the previous theme" $
      themeListSelectUp tId
    , mkKb SearchSelectDownEvent "Select the next theme" $
      themeListSelectDown tId
    , mkKb PageDownEvent "Page down in the theme list" $
      themeListPageDown tId
    , mkKb PageUpEvent "Page up in the theme list" $
      themeListPageUp tId
    , mkKb ActivateListItemEvent "Switch to the selected color theme"
      (listWindowActivateCurrent tId (csTeam(tId).tsThemeListWindow))
    ]
