module Matterhorn.Events.ReactionEmojiListOverlay
  ( onEventReactionEmojiListOverlay
  , reactionEmojiListOverlayKeybindings
  , reactionEmojiListOverlayKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.ReactionEmojiListOverlay
import           Matterhorn.State.ListOverlay
import           Matterhorn.Types


onEventReactionEmojiListOverlay :: TeamId -> Vty.Event -> MH ()
onEventReactionEmojiListOverlay tId =
    void . onEventListOverlay (csTeam(tId).tsReactionEmojiListOverlay)
           (reactionEmojiListOverlayKeybindings tId)

-- | The keybindings we want to use while viewing an emoji list overlay
reactionEmojiListOverlayKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
reactionEmojiListOverlayKeybindings tId = mkKeybindings (reactionEmojiListOverlayKeyHandlers tId)

reactionEmojiListOverlayKeyHandlers :: TeamId -> [KeyEventHandler]
reactionEmojiListOverlayKeyHandlers tId =
    [ mkKb CancelEvent "Close the emoji search window"
      (exitListOverlay (csTeam(tId).tsReactionEmojiListOverlay))
    , mkKb SearchSelectUpEvent "Select the previous emoji" $
      reactionEmojiListSelectUp tId
    , mkKb SearchSelectDownEvent "Select the next emoji" $
      reactionEmojiListSelectDown tId
    , mkKb PageDownEvent "Page down in the emoji list" $
      reactionEmojiListPageDown tId
    , mkKb PageUpEvent "Page up in the emoji list" $
      reactionEmojiListPageUp tId
    , mkKb ActivateListItemEvent "Post the selected emoji reaction"
      (listOverlayActivateCurrent (csTeam(tId).tsReactionEmojiListOverlay))
    ]
