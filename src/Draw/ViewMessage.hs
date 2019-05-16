module Draw.ViewMessage
  ( drawViewMessage
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Lens.Micro.Platform ( to )
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Foldable as F

import           Draw.Main
import           Draw.Messages ( nameForUserRef )
import           Markdown
import           Themes
import           Types


drawViewMessage :: ChatState -> [Widget Name]
drawViewMessage st = (viewMessageBox st) : (forceAttr "invalid" <$> drawMain st)

maxWidth :: Int
maxWidth = 78

maxHeight :: Int
maxHeight = 25

viewMessageBox :: ChatState -> Widget Name
viewMessageBox st =
    let body = case st^.csViewedMessage of
          Nothing -> str "BUG: no message to show, please report!"
          Just msg ->
              let hs = getHighlightSet st
                  parent = case msg^.mInReplyToMsg of
                       NotAReply -> Nothing
                       InReplyTo pId -> getMessageForPostId st pId
                  reactionsBody = reactionsText st msg
                  msgW = renderMessage $ MessageData { mdEditThreshold     = Nothing
                                                     , mdShowOlderEdits    = False
                                                     , mdMessage           = msg
                                                     , mdIsBot             = isBotMessage msg
                                                     , mdUserName          = msg^.mUser.to (nameForUserRef st)
                                                     , mdParentMessage     = parent
                                                     , mdParentUserName    = parent >>= (^.mUser.to (nameForUserRef st))
                                                     , mdRenderReplyParent = True
                                                     , mdHighlightSet      = hs
                                                     , mdIndentBlocks      = True
                                                     , mdThreadState       = NoThread
                                                     , mdShowReactions     = False
                                                     }
              in cached ViewMessageArea $ msgW <=> reactionsBody

    in centerLayer $
       Widget Fixed Fixed $ do
           ctx <- getContext
           let width = min (maxWidth + 2) (ctx^.availWidthL)
               height = min maxHeight (ctx^.availHeightL)
           render $ vLimit height $
                    hLimit width $
                    borderWithLabel (withDefAttr clientEmphAttr $ str "View Message") $
                    viewport ViewMessageArea Both $
                    body

reactionsText :: ChatState -> Message -> Widget Name
reactionsText st m =
    hLimit maxWidth body
    where
        body = case null reacList of
            True -> emptyWidget
            False -> padTop (Pad 1) $
                     vBox $
                     (hBorderWithLabel (withDefAttr clientEmphAttr $ txt "Reactions")) : (mkEntry <$> reacList)
        reacList = M.toList (m^.mReactions)
        mkEntry (reactionName, userIdSet) =
            (withDefAttr emojiAttr $ txt $ ":" <> reactionName <> ":") <=>
            (padLeft (Pad 2) $ usernameText userIdSet)

        hs = getHighlightSet st

        usernameText uids =
            renderText' hs $
            T.intercalate ", " $
            fmap (userSigil <>) $
            catMaybes (lookupUsername <$> F.toList uids)

        lookupUsername uid = usernameForUserId uid st
