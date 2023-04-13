module Matterhorn.Events.Global
  ( globalKeybindings
  , globalKeyHandlers
  )
where

import           Brick.Keybindings

import           Matterhorn.Command
import           Matterhorn.Types
import           Matterhorn.State.Channels
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Teams


globalKeybindings :: KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
globalKeybindings kc = unsafeKeyDispatcher kc globalKeyHandlers

globalKeyHandlers :: [MHKeyEventHandler]
globalKeyHandlers =
    [ onEvent ToggleMessagePreviewEvent "Toggle message preview"
        toggleMessagePreview

    , onEvent ToggleChannelListVisibleEvent "Toggle channel list visibility"
        toggleChannelListVisibility

    , onEvent ToggleExpandedChannelTopicsEvent "Toggle display of expanded channel topics"
        toggleExpandedChannelTopics

    , onEvent NextTeamEvent "Switch to the next available team"
        nextTeam

    , onEvent PrevTeamEvent "Switch to the previous available team"
        prevTeam

    , onEvent MoveCurrentTeamLeftEvent "Move the current team to the left in the team list"
        moveCurrentTeamLeft

    , onEvent MoveCurrentTeamRightEvent "Move the current team to the right in the team list"
        moveCurrentTeamRight

    , onEvent
        QuitEvent
        "Quit"
        requestQuit
    ]
