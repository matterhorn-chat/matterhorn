module Matterhorn.Draw.NotifyPrefs
  ( drawNotifyPrefs
  )
where

import Prelude ()
import Matterhorn.Prelude

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Forms (renderForm)
import Data.List (intersperse)

import Matterhorn.Draw.Util (renderKeybindingHelp)
import Matterhorn.Types
import Matterhorn.Types.KeyEvents
import Matterhorn.Themes

drawNotifyPrefs :: ChatState -> Widget Name
drawNotifyPrefs st =
    let Just form = st^.csCurrentTeam.tsNotifyPrefs
        label = forceAttr clientEmphAttr $ str "Notification Preferences"
        formKeys = withDefAttr clientEmphAttr <$> txt <$> ["Tab", "BackTab"]
        bindings = vBox $ hCenter <$> [ renderKeybindingHelp st "Save" [FormSubmitEvent] <+> txt "  " <+>
                                        renderKeybindingHelp st "Cancel" [CancelEvent]
                                      , hBox ((intersperse (txt "/") formKeys) <> [txt (":Cycle form fields")])
                                      , hBox [withDefAttr clientEmphAttr $ txt "Space", txt ":Toggle form field"]
                                      ]
    in centerLayer $
       vLimit 25 $
       hLimit 39 $
       joinBorders $
       borderWithLabel label $
       (padAll 1 $ renderForm form) <=> hBorder <=> bindings
