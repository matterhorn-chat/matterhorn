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

import           Matterhorn.State.ReactionEmojiListWindow
import           Matterhorn.State.ListWindow
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents


onEventReactionEmojiListWindow :: TeamId -> Vty.Event -> MH ()
onEventReactionEmojiListWindow tId =
    void . onEventListWindow (csTeam(tId).tsReactionEmojiListWindow)
           (reactionEmojiListWindowKeybindings tId)

-- | The keybindings we want to use while viewing an emoji list window
reactionEmojiListWindowKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyHandlerMap KeyEvent MH
reactionEmojiListWindowKeybindings tId = mkKeybindings (reactionEmojiListWindowKeyHandlers tId)

reactionEmojiListWindowKeyHandlers :: TeamId -> [MHKeyEventHandler]
reactionEmojiListWindowKeyHandlers tId =
    [ onEvent CancelEvent "Close the emoji search window"
      (exitListWindow tId (csTeam(tId).tsReactionEmojiListWindow))
    , onEvent SearchSelectUpEvent "Select the previous emoji" $
      reactionEmojiListSelectUp tId
    , onEvent SearchSelectDownEvent "Select the next emoji" $
      reactionEmojiListSelectDown tId
    , onEvent PageDownEvent "Page down in the emoji list" $
      reactionEmojiListPageDown tId
    , onEvent PageUpEvent "Page up in the emoji list" $
      reactionEmojiListPageUp tId
    , onEvent ActivateListItemEvent "Post the selected emoji reaction"
      (listWindowActivateCurrent tId (csTeam(tId).tsReactionEmojiListWindow))
    ]
