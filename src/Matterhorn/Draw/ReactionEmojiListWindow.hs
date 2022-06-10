module Matterhorn.Draw.ReactionEmojiListWindow
  ( drawReactionEmojiListWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.List ( listSelectedFocusedAttr )
import qualified Data.Text as T

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Draw.ListWindow ( drawListWindow, WindowPosition(..) )
import           Matterhorn.Types
import           Matterhorn.Themes


drawReactionEmojiListWindow :: ChatState -> TeamId -> Widget Name
drawReactionEmojiListWindow st tId =
    let window = drawListWindow (st^.csTeam(tId).tsReactionEmojiListWindow)
                                  (const $ txt "Search Emoji")
                                  (const $ txt "No matching emoji found.")
                                  (const $ txt "Search emoji:")
                                  renderEmoji
                                  Nothing
                                  WindowCenter
                                  80
    in joinBorders window

renderEmoji :: Bool -> (Bool, T.Text) -> Widget Name
renderEmoji sel (mine, e) =
    let maybeForce = if sel
                     then forceAttr listSelectedFocusedAttr
                     else id
    in clickable (ReactionEmojiListWindowEntry (mine, e)) $
       maybeForce $
       padRight Max $
       hBox [ if mine then txt " * " else txt "   "
            , withDefAttr emojiAttr $ txt $ ":" <> e <> ":"
            ]
