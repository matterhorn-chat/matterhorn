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
import Themes

drawNotifyPrefs :: ChatState -> Widget Name
drawNotifyPrefs st =
    let Just form = st^.csNotifyPrefs
        label = forceAttr clientEmphAttr $ str "Notification Preferences"
    in centerLayer $
       vLimit 20 $
       hLimit 60 $
       borderWithLabel label $
       padAll 1 $
       renderForm form
