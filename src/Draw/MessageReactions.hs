module Draw.MessageReactions
  ( drawMessageReactions
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Map as M
import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center

import           Draw.Main
import           Types
import           Themes
import           Markdown (renderText')
import           State.MessageSelect


drawMessageReactions :: ChatState -> [Widget Name]
drawMessageReactions st = messageReactionsPopup st : drawMain st

messageReactionsPopup :: ChatState -> Widget Name
messageReactionsPopup st =
    centerLayer $
    hLimit maxWidth $
    vLimit maxHeight $
    borderWithLabel (withDefAttr clientEmphAttr $ str "Message Reactions") $
    viewport MessageReactionsArea Vertical body
    where
        maxWidth = 80
        maxHeight = 25
        body = case getSelectedMessage st of
            Nothing -> txt "No message selected."
            Just m -> reactionsText st m

reactionsText :: ChatState -> Message -> Widget Name
reactionsText st m =
    vBox $ mkEntry <$> M.toList (m^.mReactions)
    where
        mkEntry (reactionName, userIdSet) =
            (withDefAttr emojiAttr $ txt reactionName) <=>
            (padLeft (Pad 2) $ usernameText userIdSet)

        hs = getHighlightSet st

        usernameText uids =
            renderText' hs $
            T.intercalate ", " $
            fmap (userSigil <>) $
            catMaybes (lookupUsername <$> F.toList uids)

        lookupUsername uid = usernameForUserId uid st
