module State.ChannelScroll
  (
    channelScrollToTop
  , channelScrollToBottom
  , channelScrollUp
  , channelScrollDown
  , channelPageUp
  , channelPageDown
  )
 where

import           Prelude ()
import           Prelude.MH

import           Brick.Main

import           Constants
import           Types


channelPageUp :: MH ()
channelPageUp = do
    cId <- use csCurrentChannelId
    mh $ vScrollBy (viewportScroll (ChannelMessages cId)) (-1 * pageAmount)

channelPageDown :: MH ()
channelPageDown = do
    cId <- use csCurrentChannelId
    mh $ vScrollBy (viewportScroll (ChannelMessages cId)) pageAmount

channelScrollUp :: MH ()
channelScrollUp = do
    cId <- use csCurrentChannelId
    mh $ vScrollBy (viewportScroll (ChannelMessages cId)) (-1)

channelScrollDown :: MH ()
channelScrollDown = do
    cId <- use csCurrentChannelId
    mh $ vScrollBy (viewportScroll (ChannelMessages cId)) 1

channelScrollToTop :: MH ()
channelScrollToTop = do
    cId <- use csCurrentChannelId
    mh $ vScrollToBeginning (viewportScroll (ChannelMessages cId))

channelScrollToBottom :: MH ()
channelScrollToBottom = do
    cId <- use csCurrentChannelId
    mh $ vScrollToEnd (viewportScroll (ChannelMessages cId))
