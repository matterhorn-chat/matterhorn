module Draw.NotifyPrefs
  ( drawNotifyPrefs
  )
where

import Prelude ()
import Prelude.MH

import Data.Maybe (fromJust)
import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Forms (renderForm)

import Types

drawNotifyPrefs :: ChatState -> Widget Name
drawNotifyPrefs st =
    let form = fromJust $ st^.csNotifyPrefs
    in centerLayer $
       vLimit 20 $
       hLimit 60 $
       border $
       renderForm form
