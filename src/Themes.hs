{-# LANGUAGE OverloadedStrings #-}
module Themes
  ( colorTheme

  -- * Attribute names
  , timeAttr
  , channelHeaderAttr
  , channelListHeaderAttr
  , currentChannelNameAttr
  , urlAttr
  , markdownAttr
  , emailAttr
  , emojiAttr

  -- * Username formatting
  , colorUsername
  , attrForUsername
  ) where

import Data.Hashable (hash)
import Data.Monoid ((<>))
import Graphics.Vty
import Brick

timeAttr :: AttrName
timeAttr = "time"

channelHeaderAttr :: AttrName
channelHeaderAttr = "channelHeader"

channelListHeaderAttr :: AttrName
channelListHeaderAttr = "channelListHeader"

currentChannelNameAttr :: AttrName
currentChannelNameAttr = "currentChannelName"

urlAttr :: AttrName
urlAttr = "url"

markdownAttr :: AttrName
markdownAttr = "markdown"

emailAttr :: AttrName
emailAttr = "email"

emojiAttr :: AttrName
emojiAttr = "emoji"

colorTheme :: AttrMap
colorTheme = attrMap (bg black) $
  [ (timeAttr,                fg white)
  , (channelHeaderAttr,       fg white `withStyle` underline)
  , (channelListHeaderAttr,   fg cyan)
  , (currentChannelNameAttr,  black `on` yellow `withStyle` bold)
  , (urlAttr,                 fg yellow)
  , (emailAttr,               fg yellow)
  , (markdownAttr,            fg magenta)
  , (emojiAttr,               fg yellow)
  ] <>
  ((\(i, a) -> (usernameAttr i, a)) <$> zip [0..] usernameColors)

usernameAttr :: Int -> AttrName
usernameAttr i = "username" <> (attrName $ show i)

colorUsername :: String -> Widget a
colorUsername s = withDefAttr (attrForUsername s) $ str s

attrForUsername :: String -> AttrName
attrForUsername ('@':s) = usernameAttr $ hash s `mod` (length usernameColors)
attrForUsername s = usernameAttr $ hash s `mod` (length usernameColors)

usernameColors :: [Attr]
usernameColors =
    [ fg red
    , fg green
    , fg yellow
    , fg blue
    , fg magenta
    , fg cyan
    , fg white
    , fg brightRed
    , fg brightGreen
    , fg brightYellow
    , fg brightBlue
    , fg brightMagenta
    , fg brightCyan
    , fg brightWhite
    ]
