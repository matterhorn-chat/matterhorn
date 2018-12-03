module Events.ManageAttachments
  ( onEventManageAttachments
  )
where

import qualified Graphics.Vty as Vty

import           Types


onEventManageAttachments :: Vty.Event -> MH ()
onEventManageAttachments _ = setMode Main
