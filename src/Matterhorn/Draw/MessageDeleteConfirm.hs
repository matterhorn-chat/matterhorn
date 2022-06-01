module Matterhorn.Draw.MessageDeleteConfirm
  ( drawMessageDeleteConfirm
  )
where

import Prelude ()
import Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import qualified Data.Text as T

import           Matterhorn.Types
import           Matterhorn.Themes


drawMessageDeleteConfirm :: Widget Name
drawMessageDeleteConfirm =
    let msg = "Are you sure you want to delete the selected message? (y/n)"
    in centerLayer $
       borderWithLabel (withAttr channelListHeaderAttr $ txt "Confirm") $
       hLimit (T.length msg + 4) $
       vLimit 3 $
       center $
       withDefAttr errorMessageAttr $
       txt msg

