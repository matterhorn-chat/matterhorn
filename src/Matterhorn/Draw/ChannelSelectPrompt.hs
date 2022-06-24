module Matterhorn.Draw.ChannelSelectPrompt
  ( drawChannelSelectPrompt
  )
where

import Prelude ()
import Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Edit ( renderEditor )
import qualified Data.Text as T
import           Network.Mattermost.Types ( TeamId)

import           Matterhorn.Types
import           Matterhorn.Themes


drawChannelSelectPrompt :: ChatState -> TeamId -> Widget Name
drawChannelSelectPrompt st tId =
    Widget Greedy Greedy $ do
       ctx <- getContext
       let rowOffset = ctx^.availHeightL - 1
           e = st^.csTeam(tId).tsChannelSelectState.channelSelectInput
       render $ translateBy (Location (0, rowOffset)) $
                withDefAttr channelSelectPromptAttr $
                (txt "Switch to channel [use ^ and $ to anchor]: ") <+>
                (renderEditor (txt . T.concat) True e)

