module Draw.Messages
  ( nameForUserRef
  , maxMessageHeight
  , renderSingleMessage
  , unsafeRenderMessageSelection
  )
where

import           Brick
import           Brick.Widgets.Border
import           Control.Monad.Trans.Reader ( withReaderT )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (.~), to )
import           Network.Mattermost.Types ( ServerTime(..) )
import           Prelude ()
import           Prelude.MH

import           Draw.Util
import           Markdown
import           Themes
import           Types

maxMessageHeight :: Int
maxMessageHeight = 200


-- | nameForUserRef converts the UserRef into a printable name, based
-- on the current known user data.
nameForUserRef :: ChatState -> UserRef -> Maybe Text
nameForUserRef st uref = case uref of
                           NoUser -> Nothing
                           UserOverride t -> Just t
                           UserI uId -> displaynameForUserId uId st

-- | renderSingleMessage is the main message drawing function.
--
-- The `ind` argument specifies an "indicator boundary".  Showing
-- various indicators (e.g. "edited") is not typically done for
-- messages that are older than this boundary value.
renderSingleMessage :: ChatState -> HighlightSet -> Maybe ServerTime -> Message -> Widget Name
renderSingleMessage st hs ind =
  renderChatMessage st hs ind (withBrackets . renderTime st . withServerTime)

renderChatMessage :: ChatState -> HighlightSet -> Maybe ServerTime -> (ServerTime -> Widget Name) -> Message -> Widget Name
renderChatMessage st hs ind renderTimeFunc msg =
    let showOlderEdits = configShowOlderEdits $ st^.csResources.crConfiguration
        parent = case msg^.mInReplyToMsg of
          NotAReply -> Nothing
          InReplyTo pId -> getMessageForPostId st pId
        m = renderMessage MessageData
              { mdMessage           = msg
              , mdUserName          = msg^.mUser.to (nameForUserRef st)
              , mdParentMessage     = parent
              , mdParentUserName    = parent >>= (^.mUser.to (nameForUserRef st))
              , mdEditThreshold     = ind
              , mdHighlightSet      = hs
              , mdShowOlderEdits    = showOlderEdits
              , mdRenderReplyParent = True
              , mdIndentBlocks      = True
              }
        msgAtch = if Seq.null (msg^.mAttachments)
          then Nothing
          else Just $ withDefAttr clientMessageAttr $ vBox
                 [ txt ("  [attached: `" <> a^.attachmentName <> "`]")
                 | a <- toList (msg^.mAttachments)
                 ]
        msgReac = if Map.null (msg^.mReactions)
          then Nothing
          else let renderR e 1 = " [" <> e <> "]"
                   renderR e n
                     | n > 1     = " [" <> e <> " " <> T.pack (show n) <> "]"
                     | otherwise = ""
                   reacMsg = Map.foldMapWithKey renderR (msg^.mReactions)
               in Just $ withDefAttr emojiAttr $ txt ("   " <> reacMsg)
        msgTxt =
          case msg^.mUser of
            NoUser ->
                case msg^.mType of
                    C DateTransition -> withDefAttr dateTransitionAttr (hBorderWithLabel m)
                    C NewMessagesTransition -> withDefAttr newMessageTransitionAttr (hBorderWithLabel m)
                    C Error -> withDefAttr errorMessageAttr m
                    C UnknownGap -> withDefAttr gapMessageAttr m
                    _ -> withDefAttr clientMessageAttr m
            _ | msg^.mType == CP Join || msg^.mType == CP Leave ->
                  withDefAttr clientMessageAttr m
              | otherwise -> m
        fullMsg = vBox $ msgTxt : catMaybes [msgAtch, msgReac]
        maybeRenderTime w = hBox [renderTimeFunc (msg^.mDate), txt " ", w]
        maybeRenderTimeWith f = case msg^.mType of
            C DateTransition -> id
            C NewMessagesTransition -> id
            _ -> f
    in maybeRenderTimeWith maybeRenderTime fullMsg

-- | Render a selected message with focus, including the messages
-- before and the messages after it. The foldable parameters exist
-- because (depending on the situation) we might use either of the
-- message list types for the 'before' and 'after' (i.e. the
-- chronological or retrograde message sequences).
unsafeRenderMessageSelection ::
  (Foldable f, Foldable g) =>
  (Message, (f Message, g Message)) ->
  (Message -> Widget Name) ->
  Widget Name
unsafeRenderMessageSelection (curMsg, (before, after)) doMsgRender =
  Widget Greedy Greedy $ do
    let relaxHeight c = c & availHeightL .~ (max maxMessageHeight (c^.availHeightL))

        render1HLimit fjoin lim img msg
          | Vty.imageHeight img >= lim = return img
          | otherwise = fjoin img <$> render1 msg

        render1 msg = case msg^.mDeleted of
                        True -> return Vty.emptyImage
                        False -> do
                          r <- withReaderT relaxHeight $
                               render $ padRight Max $
                               doMsgRender msg
                          return $ r^.imageL

    ctx <- getContext
    curMsgResult <- withReaderT relaxHeight $ render $
                    forceAttr messageSelectAttr $
                    padRight Max $ doMsgRender curMsg

    let targetHeight = ctx^.availHeightL
        upperHeight = targetHeight `div` 2
        lowerHeight = targetHeight - upperHeight

        lowerRender = render1HLimit Vty.vertJoin targetHeight
        upperRender = render1HLimit (flip Vty.vertJoin) targetHeight

    lowerHalf <- foldM lowerRender Vty.emptyImage after
    upperHalf <- foldM upperRender Vty.emptyImage before

    let curHeight = Vty.imageHeight $ curMsgResult^.imageL
        uncropped = upperHalf Vty.<-> curMsgResult^.imageL Vty.<-> lowerHalf
        img = if Vty.imageHeight lowerHalf < (lowerHeight - curHeight)
              then Vty.cropTop targetHeight uncropped
              else if Vty.imageHeight upperHalf < upperHeight
                   then Vty.cropBottom targetHeight uncropped
                   else Vty.cropTop upperHeight upperHalf Vty.<->
                        curMsgResult^.imageL Vty.<->
                        (if curHeight < lowerHeight
                          then Vty.cropBottom (lowerHeight - curHeight) lowerHalf
                          else Vty.cropBottom lowerHeight lowerHalf)
    return $ emptyResult & imageL .~ img
