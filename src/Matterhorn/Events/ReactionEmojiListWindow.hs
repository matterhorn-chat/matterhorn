module Matterhorn.Events.ReactionEmojiListWindow
  ( onEventReactionEmojiListWindow
  , reactionEmojiListWindowKeybindings
  , reactionEmojiListWindowKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Graphics.Vty as Vty

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Events.Keybindings
import           Matterhorn.State.ReactionEmojiListWindow
import           Matterhorn.State.ListWindow
import           Matterhorn.Types


onEventReactionEmojiListWindow :: TeamId -> Vty.Event -> MH ()
onEventReactionEmojiListWindow tId =
    void . onEventListWindow (csTeam(tId).tsReactionEmojiListWindow)
           (reactionEmojiListWindowKeybindings tId)

-- | The keybindings we want to use while viewing an emoji list window
reactionEmojiListWindowKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
reactionEmojiListWindowKeybindings tId = mkKeybindings (reactionEmojiListWindowKeyHandlers tId)

reactionEmojiListWindowKeyHandlers :: TeamId -> [KeyEventHandler]
reactionEmojiListWindowKeyHandlers tId =
    [ mkKb CancelEvent "Close the emoji search window"
      (exitListWindow tId (csTeam(tId).tsReactionEmojiListWindow))
    , mkKb SearchSelectUpEvent "Select the previous emoji" $
      reactionEmojiListSelectUp tId
    , mkKb SearchSelectDownEvent "Select the next emoji" $
      reactionEmojiListSelectDown tId
    , mkKb PageDownEvent "Page down in the emoji list" $
      reactionEmojiListPageDown tId
    , mkKb PageUpEvent "Page up in the emoji list" $
      reactionEmojiListPageUp tId
    , mkKb ActivateListItemEvent "Post the selected emoji reaction"
      (listWindowActivateCurrent tId (csTeam(tId).tsReactionEmojiListWindow))
    ]
