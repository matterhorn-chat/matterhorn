module Matterhorn.Events.ThemeListWindow where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings
import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.ThemeListWindow
import           Matterhorn.State.ListWindow
import           Matterhorn.Types


onEventThemeListWindow :: TeamId -> Vty.Event -> MH ()
onEventThemeListWindow tId =
    void . onEventListWindow (csTeam(tId).tsThemeListWindow)
        (themeListWindowKeybindings tId)

-- | The keybindings we want to use while viewing a user list window
themeListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
themeListWindowKeybindings tId = mkKeybindings (themeListWindowKeyHandlers tId)

themeListWindowKeyHandlers :: TeamId -> [MHKeyEventHandler]
themeListWindowKeyHandlers tId =
    [ onEvent CancelEvent "Close the theme list"
      (exitListWindow tId (csTeam(tId).tsThemeListWindow))
    , onEvent SearchSelectUpEvent "Select the previous theme" $
      themeListSelectUp tId
    , onEvent SearchSelectDownEvent "Select the next theme" $
      themeListSelectDown tId
    , onEvent PageDownEvent "Page down in the theme list" $
      themeListPageDown tId
    , onEvent PageUpEvent "Page up in the theme list" $
      themeListPageUp tId
    , onEvent ActivateListItemEvent "Switch to the selected color theme"
      (listWindowActivateCurrent tId (csTeam(tId).tsThemeListWindow))
    ]
