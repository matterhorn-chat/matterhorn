module Draw.NotifyPrefs
  ( drawNotifyPrefs
  )
where

import Prelude ()
import Prelude.MH

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Forms (renderForm)

import Types

drawNotifyPrefs :: ChatState -> Widget Name
drawNotifyPrefs st =
    let Just form = st^.csNotifyPrefs
        label = str "Notification Preferences"
    in centerLayer $
       vLimit 20 $
       hLimit 60 $
       borderWithLabel label $
       renderForm form
