module Draw.ChannelListOverlay
  ( drawChannelListOverlay
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import qualified Data.Text as T
import           Text.Wrap ( defaultWrapSettings, preserveIndentation )

import           Network.Mattermost.Types
import           Network.Mattermost.Lenses

import           Draw.Main
import           Draw.ListOverlay ( drawListOverlay )
import           Types
import           Types.Common ( sanitizeUserText )


drawChannelListOverlay :: ChatState -> [Widget Name]
drawChannelListOverlay st =
    let overlay = drawListOverlay (st^.csChannelListOverlay) channelSearchScopeHeader
                                  channelSearchScopeNoResults channelSearchScopePrompt
                                  renderChannel
    in joinBorders overlay : drawMain False st

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
    txt $ case scope of
        AllChannels -> "Join a Channel"

renderChannel :: Bool -> Channel -> Widget Name
renderChannel _ chan =
    let baseStr = (sanitizeUserText $ chan^.channelNameL) <> " (" <> (sanitizeUserText $ chan^.channelDisplayNameL) <> ")"
        s = "  " <> (T.strip $ sanitizeUserText $ chan^.channelPurposeL)
    in (vLimit 1 $ padRight Max $ txt baseStr) <=>
       (vLimit 1 $ txtWrapWith (defaultWrapSettings { preserveIndentation = True }) s)
