module Matterhorn.Events.Global
  ( globalKeybindings
  , globalKeyHandlers
  )
where

import           Matterhorn.Command
import           Matterhorn.Types
import           Matterhorn.State.Channels
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Teams

import           Matterhorn.Events.Keybindings


globalKeybindings :: KeyConfig KeyEvent -> KeyHandlerMap
globalKeybindings = mkKeybindings globalKeyHandlers

globalKeyHandlers :: [KeyEventHandler]
globalKeyHandlers =
    [ mkKb ToggleMessagePreviewEvent "Toggle message preview"
        toggleMessagePreview

    , mkKb ToggleChannelListVisibleEvent "Toggle channel list visibility"
        toggleChannelListVisibility

    , mkKb ToggleExpandedChannelTopicsEvent "Toggle display of expanded channel topics"
        toggleExpandedChannelTopics

    , mkKb NextTeamEvent "Switch to the next available team"
        nextTeam

    , mkKb PrevTeamEvent "Switch to the previous available team"
        prevTeam

    , mkKb MoveCurrentTeamLeftEvent "Move the current team to the left in the team list"
        moveCurrentTeamLeft

    , mkKb MoveCurrentTeamRightEvent "Move the current team to the right in the team list"
        moveCurrentTeamRight

    , mkKb
        QuitEvent
        "Quit"
        requestQuit
    ]
