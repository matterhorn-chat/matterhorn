module Matterhorn.Events.ThreadWindow
  ( onEventThreadWindow
  )
where

import Prelude ()
import Matterhorn.Prelude

import qualified Graphics.Vty as Vty
import Lens.Micro.Platform (Lens')

import Network.Mattermost.Types (TeamId)

import Matterhorn.Types
import Matterhorn.Events.MessageInterface

onEventThreadWindow :: TeamId -> Vty.Event -> MH Bool
onEventThreadWindow tId ev = do
    let ti :: Lens' ChatState ThreadInterface
        ti = unsafeThreadInterface tId

    handleMessageInterfaceEvent tId ti ev
