module Types where

import qualified Graphics.Vty as Vty
import Network.Mattermost.WebSocket.Types

data Event
  = VtyEvent Vty.Event
  | WSEvent WebsocketEvent
