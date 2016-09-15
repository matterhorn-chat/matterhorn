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
  , codeAttr
  , emailAttr
  , emojiAttr
  , clientMessageAttr
  , clientHeaderAttr
  , clientEmphAttr
  , clientStrongAttr
  , dateTransitionAttr
  , errorMessageAttr
  , helpAttr
  , helpEmphAttr
  , channelSelectPromptAttr
  , channelSelectMatchAttr
  , completionAlternativeListAttr
  , completionAlternativeCurrentAttr
  , dialogAttr
  , dialogEmphAttr
  , recentMarkerAttr

  -- * Username formatting
  , colorUsername
  , attrForUsername
  ) where

import Data.Hashable (hash)
import Data.Monoid ((<>))
import Graphics.Vty
import Brick
import Brick.Widgets.List
import qualified Data.Text as T

defaultThemeName :: T.Text
defaultThemeName = darkColorThemeName

helpAttr :: AttrName
helpAttr = "help"

helpEmphAttr :: AttrName
helpEmphAttr = "helpEmphasis"

recentMarkerAttr :: AttrName
recentMarkerAttr = "recentMarker"

dialogAttr :: AttrName
dialogAttr = "dialog"

dialogEmphAttr :: AttrName
dialogEmphAttr = "dialogEmphasis"

channelSelectMatchAttr :: AttrName
channelSelectMatchAttr = "channelSelectMatch"

channelSelectPromptAttr :: AttrName
channelSelectPromptAttr = "channelSelectPrompt"

completionAlternativeListAttr :: AttrName
completionAlternativeListAttr = "completionAlternativeList"

completionAlternativeCurrentAttr :: AttrName
completionAlternativeCurrentAttr = "completionAlternativeCurrent"

darkColorThemeName :: T.Text
darkColorThemeName = "builtin:dark"

lightColorThemeName :: T.Text
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

codeAttr :: AttrName
codeAttr = "code"

emailAttr :: AttrName
emailAttr = "email"

emojiAttr :: AttrName
emojiAttr = "emoji"

clientMessageAttr :: AttrName
clientMessageAttr = "clientMessage"

clientHeaderAttr :: AttrName
clientHeaderAttr = "clientHeader"

clientEmphAttr :: AttrName
clientEmphAttr = "clientEmph"

clientStrongAttr :: AttrName
clientStrongAttr = "clientStrong"

errorMessageAttr :: AttrName
errorMessageAttr = "errorMessage"

themes :: [(T.Text, AttrMap)]
themes =
    [ (darkColorThemeName,  darkColorTheme)
    , (lightColorThemeName, lightColorTheme)
    ]

lightColorTheme :: AttrMap
lightColorTheme = attrMap (black `on` white) $
  [ (timeAttr,                         fg black)
  , (channelHeaderAttr,                fg black `withStyle` underline)
  , (channelListHeaderAttr,            fg cyan)
  , (currentChannelNameAttr,           black `on` yellow `withStyle` bold)
  , (unreadChannelAttr,                black `on` cyan   `withStyle` bold)
  , (urlAttr,                          fg brightYellow)
  , (emailAttr,                        fg yellow)
  , (codeAttr,                         fg magenta)
  , (emojiAttr,                        fg yellow)
  , (clientMessageAttr,                fg black)
  , (clientEmphAttr,                   fg black `withStyle` bold)
  , (clientStrongAttr,                 fg black `withStyle` bold `withStyle` underline)
  , (clientHeaderAttr,                 fg red `withStyle` bold)
  , (dateTransitionAttr,               fg green)
  , (errorMessageAttr,                 fg red)
  , (helpAttr,                         black `on` cyan)
  , (helpEmphAttr,                     fg white)
  , (channelSelectMatchAttr,           black `on` magenta)
  , (channelSelectPromptAttr,          fg black)
  , (completionAlternativeListAttr,    white `on` blue)
  , (completionAlternativeCurrentAttr, black `on` yellow)
  , (dialogAttr,                       black `on` cyan)
  , (dialogEmphAttr,                   fg white)
  , (listSelectedFocusedAttr,          black `on` yellow)
  , (recentMarkerAttr,                 fg black `withStyle` bold)
  ] <>
  ((\(i, a) -> (usernameAttr i, a)) <$> zip [0..] usernameColors)

darkColorTheme :: AttrMap
darkColorTheme = attrMap (bg black) $
  [ (timeAttr,                         fg white)
  , (channelHeaderAttr,                fg white `withStyle` underline)
  , (channelListHeaderAttr,            fg cyan)
  , (currentChannelNameAttr,           black `on` yellow `withStyle` bold)
  , (unreadChannelAttr,                black `on` cyan   `withStyle` bold)
  , (urlAttr,                          fg yellow)
  , (emailAttr,                        fg yellow)
  , (codeAttr,                         fg magenta)
  , (emojiAttr,                        fg yellow)
  , (clientMessageAttr,                fg white)
  , (clientEmphAttr,                   fg white `withStyle` bold)
  , (clientStrongAttr,                 fg white `withStyle` bold `withStyle` underline)
  , (clientHeaderAttr,                 fg red `withStyle` bold)
  , (dateTransitionAttr,               fg green)
  , (errorMessageAttr,                 fg red)
  , (helpAttr,                         black `on` cyan)
  , (helpEmphAttr,                     fg white)
  , (channelSelectMatchAttr,           black `on` magenta)
  , (channelSelectPromptAttr,          fg white)
  , (completionAlternativeListAttr,    white `on` blue)
  , (completionAlternativeCurrentAttr, black `on` yellow)
  , (dialogAttr,                       black `on` cyan)
  , (dialogEmphAttr,                   fg white)
  , (listSelectedFocusedAttr,          black `on` yellow)
  , (recentMarkerAttr,                 fg yellow `withStyle` bold)
  ] <>
  ((\(i, a) -> (usernameAttr i, a)) <$> zip [0..] usernameColors)

usernameAttr :: Int -> AttrName
usernameAttr i = "username" <> (attrName $ show i)

colorUsername :: T.Text -> Widget a
colorUsername s = withDefAttr (attrForUsername s) $ txt s

attrForUsername :: T.Text -> AttrName
attrForUsername s
    | "@" `T.isPrefixOf` s ||
      "+" `T.isPrefixOf` s ||
      "-" `T.isPrefixOf` s ||
      " " `T.isPrefixOf` s
      = usernameAttr $ hash (T.tail s) `mod` (length usernameColors)
    | otherwise            = usernameAttr $ hash s          `mod` (length usernameColors)

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
