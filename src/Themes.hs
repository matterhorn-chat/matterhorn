{-# LANGUAGE OverloadedStrings #-}
module Themes
  ( colorTheme

  -- * Attribute names
  , timeAttr
  , channelHeaderAttr
  , channelListHeaderAttr
  ) where

import Graphics.Vty
import Brick.AttrMap
import Brick.Util (fg, bg)

defaultAttr :: Attr
defaultAttr = bg black

timeAttr :: AttrName
timeAttr = "time"

channelHeaderAttr :: AttrName
channelHeaderAttr = "channelHeader"

channelListHeaderAttr :: AttrName
channelListHeaderAttr = "channelListHeader"

colorTheme :: AttrMap
colorTheme = attrMap defaultAttr
  [ (timeAttr,                fg white)
  , (channelHeaderAttr,       fg white)
  , (channelListHeaderAttr,   fg cyan)
  ]
