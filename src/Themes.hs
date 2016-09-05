{-# LANGUAGE OverloadedStrings #-}
module Themes
  ( defaultThemeName
  , themes

  -- * Attribute names
  , timeAttr
  , channelHeaderAttr
  , channelListHeaderAttr
  , currentChannelNameAttr
  , unreadChannelAttr
  , urlAttr
  , markdownAttr
  , emailAttr
  , emojiAttr
  , clientMessageAttr
  , dateTransitionAttr
  , errorMessageAttr

  -- * Username formatting
  , colorUsername
  , attrForUsername
  ) where

import Data.Hashable (hash)
import Data.Monoid ((<>))
import Graphics.Vty
import Brick

defaultThemeName :: String
defaultThemeName = darkColorThemeName

darkColorThemeName :: String
darkColorThemeName = "builtin:dark"

lightColorThemeName :: String
lightColorThemeName = "builtin:light"

timeAttr :: AttrName
timeAttr = "time"

channelHeaderAttr :: AttrName
channelHeaderAttr = "channelHeader"

channelListHeaderAttr :: AttrName
channelListHeaderAttr = "channelListHeader"

currentChannelNameAttr :: AttrName
currentChannelNameAttr = "currentChannelName"

unreadChannelAttr :: AttrName
unreadChannelAttr = "unreadChannel"

dateTransitionAttr :: AttrName
dateTransitionAttr = "dateTransition"

urlAttr :: AttrName
urlAttr = "url"

markdownAttr :: AttrName
markdownAttr = "markdown"

emailAttr :: AttrName
emailAttr = "email"

emojiAttr :: AttrName
emojiAttr = "emoji"

clientMessageAttr :: AttrName
clientMessageAttr = "clientMessage"

errorMessageAttr :: AttrName
errorMessageAttr = "errorMessage"

themes :: [(String, AttrMap)]
themes =
    [ (darkColorThemeName,  darkColorTheme)
    , (lightColorThemeName, lightColorTheme)
    ]

lightColorTheme :: AttrMap
lightColorTheme = attrMap (black `on` white) $
  [ (timeAttr,                fg black)
  , (channelHeaderAttr,       fg black `withStyle` underline)
  , (channelListHeaderAttr,   fg cyan)
  , (currentChannelNameAttr,  black `on` yellow `withStyle` bold)
  , (unreadChannelAttr,       black `on` cyan   `withStyle` bold)
  , (urlAttr,                 fg brightYellow)
  , (emailAttr,               fg yellow)
  , (markdownAttr,            fg magenta)
  , (emojiAttr,               fg yellow)
  , (clientMessageAttr,       fg white)
  , (dateTransitionAttr,      fg green)
  , (errorMessageAttr,        fg red)
  ] <>
  ((\(i, a) -> (usernameAttr i, a)) <$> zip [0..] usernameColors)

darkColorTheme :: AttrMap
darkColorTheme = attrMap (bg black) $
  [ (timeAttr,                fg white)
  , (channelHeaderAttr,       fg white `withStyle` underline)
  , (channelListHeaderAttr,   fg cyan)
  , (currentChannelNameAttr,  black `on` yellow `withStyle` bold)
  , (unreadChannelAttr,       black `on` cyan   `withStyle` bold)
  , (urlAttr,                 fg yellow)
  , (emailAttr,               fg yellow)
  , (markdownAttr,            fg magenta)
  , (emojiAttr,               fg yellow)
  , (clientMessageAttr,       fg white)
  , (dateTransitionAttr,      fg green)
  , (errorMessageAttr,        fg red)
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
    , fg brightRed
    , fg brightGreen
    , fg brightYellow
    , fg brightBlue
    , fg brightMagenta
    , fg brightCyan
    ]
