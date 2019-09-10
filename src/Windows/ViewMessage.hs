module Windows.ViewMessage
  ( viewMessageWindowTemplate
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick

import           Types

viewMessageWindowTemplate :: TabbedWindowTemplate ViewMessageWindowTab
viewMessageWindowTemplate =
    TabbedWindowTemplate { twtEntries = []
                         , twtTitle = const $ txt "View Message"
                         }
