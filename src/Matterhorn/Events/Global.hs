module Matterhorn.Events.Global
  ( globalKeybindings
  , globalKeyHandlers
  )
where

import           Matterhorn.Command
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.State.Channels
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Teams


globalKeybindings :: KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
globalKeybindings = mkKeybindings globalKeyHandlers

globalKeyHandlers :: [MHKeyEventHandler]
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
