module Events.ChannelScroll where

import Prelude ()
import Prelude.Compat

import Brick
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import Events.Keybindings
import State

channelScrollKeybindings :: KeyConfig -> [Keybinding]
channelScrollKeybindings = mkKeybindings
  [ mkKb LoadMoreEvent "Load more messages in the current channel"
    loadMoreMessages
  , mkKb OpenMessageURLEvent "Select and open a URL posted to the current channel"
    startUrlSelect
  , mkKb ScrollUpEvent "Scroll up"
    channelScrollUp
  , mkKb ScrollDownEvent "Scroll down"
    channelScrollDown
  , mkKb PageUpEvent "Scroll up"
    channelPageUp
  , mkKb PageDownEvent "Scroll down"
    channelPageDown
  , mkKb CancelEvent "Cancel scrolling and return to channel view" $
    csMode .= Main
  , mkKb ScrollTopEvent "Scroll to top"
    channelScrollToTop
  , mkKb ScrollBottomEvent "Scroll to bottom"
    channelScrollToBottom
  ]

onEventChannelScroll :: Vty.Event -> MH ()
onEventChannelScroll =
  handleKeyboardEvent channelScrollKeybindings $ \ e -> case e of
    (Vty.EvResize _ _) -> do
      cId <- use csCurrentChannelId
      mh $ do
        invalidateCache
        let vp = ChannelMessages cId
        vScrollToEnd $ viewportScroll vp
    _ -> return ()
