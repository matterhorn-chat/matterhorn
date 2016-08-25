{-# LANGUAGE OverloadedStrings #-}
module Themes
  ( colorTheme

  -- * Attribute names
  , timeAttr
  , channelHeaderAttr
  , channelListHeaderAttr
  , currentChannelNameAttr
  ) where

import Graphics.Vty
import Brick.AttrMap
import Brick.Util (fg, bg)

timeAttr :: AttrName
timeAttr = "time"

channelHeaderAttr :: AttrName
channelHeaderAttr = "channelHeader"

channelListHeaderAttr :: AttrName
channelListHeaderAttr = "channelListHeader"

currentChannelNameAttr :: AttrName
currentChannelNameAttr = "currentChannelName"

colorTheme :: AttrMap
colorTheme = attrMap (bg black)
  [ (timeAttr,                fg white)
  , (channelHeaderAttr,       fg white)
  , (channelListHeaderAttr,   fg cyan)
  , (currentChannelNameAttr,  fg white `withStyle` bold)
  ]
