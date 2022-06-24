module Matterhorn.Draw.ChannelListWindow
  ( drawChannelListWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import qualified Data.Text as T
import           Text.Wrap ( defaultWrapSettings, preserveIndentation )

import           Network.Mattermost.Types
import           Network.Mattermost.Lenses

import           Matterhorn.Draw.ListWindow ( drawListWindow, WindowPosition(..) )
import           Matterhorn.Types
import           Matterhorn.Types.Common ( sanitizeUserText )
import           Matterhorn.Themes


drawChannelListWindow :: ChatState -> TeamId -> Widget Name
drawChannelListWindow st tId =
    let window = drawListWindow (st^.csTeam(tId).tsChannelListWindow) channelSearchScopeHeader
                                  channelSearchScopeNoResults channelSearchScopePrompt
                                  renderChannel
                                  Nothing
                                  WindowCenter
                                  80
    in joinBorders window

channelSearchScopePrompt :: ChannelSearchScope -> Widget Name
channelSearchScopePrompt scope =
    txt $ case scope of
        AllChannels -> "Search channels:"

channelSearchScopeNoResults :: ChannelSearchScope -> Widget Name
channelSearchScopeNoResults scope =
    txt $ case scope of
        AllChannels -> "No matching channels found."

channelSearchScopeHeader :: ChannelSearchScope -> Widget Name
channelSearchScopeHeader scope =
    withDefAttr clientEmphAttr $ txt $ case scope of
        AllChannels -> "Join a Channel"

renderChannel :: Bool -> Channel -> Widget Name
renderChannel _ chan =
    let baseStr = (sanitizeUserText $ chan^.channelDisplayNameL) <>
                  " (" <> (sanitizeUserText $ chan^.channelNameL) <> ")"
        s = "  " <> (T.strip $ sanitizeUserText $ chan^.channelPurposeL)
    in (vLimit 1 $ padRight Max $ withDefAttr clientEmphAttr $ txt baseStr) <=>
       (vLimit 1 $ txtWrapWith (defaultWrapSettings { preserveIndentation = True }) s)
