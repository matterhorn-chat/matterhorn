module Events.ReactionEmojiListOverlay
  ( onEventReactionEmojiListOverlay
  , reactionEmojiListOverlayKeybindings
  )
where

import qualified Graphics.Vty as Vty

import           Events.Keybindings
import           State.ReactionEmojiListOverlay
import           State.ListOverlay
import           Types


onEventReactionEmojiListOverlay :: Vty.Event -> MH ()
onEventReactionEmojiListOverlay =
    onEventListOverlay csReactionEmojiListOverlay reactionEmojiListOverlayKeybindings

-- | The keybindings we want to use while viewing an emoji list overlay
reactionEmojiListOverlayKeybindings :: KeyConfig -> [Keybinding]
reactionEmojiListOverlayKeybindings = mkKeybindings
    [ mkKb CancelEvent "Close the emoji search window" (exitListOverlay csReactionEmojiListOverlay)
    , mkKb SearchSelectUpEvent "Select the previous emoji" reactionEmojiListSelectUp
    , mkKb SearchSelectDownEvent "Select the next emoji" reactionEmojiListSelectDown
    , mkKb PageDownEvent "Page down in the emoji list" reactionEmojiListPageDown
    , mkKb PageUpEvent "Page up in the emoji list" reactionEmojiListPageUp
    , mkKb ActivateListItemEvent "Post the selected emoji reaction" (listOverlayActivateCurrent csReactionEmojiListOverlay)
    ]
