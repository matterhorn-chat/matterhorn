{-# LANGUAGE MultiWayIf #-}
module Draw.Messages
  ( nameForUserRef
  , renderSingleMessage
  , unsafeRenderMessageSelection
  , renderLastMessages
  , isMessageOutgoing
  )
where

import           Brick
import           Brick.Widgets.Border
import           Control.Monad.Trans.Reader ( withReaderT )
import qualified Data.Foldable as F
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (.~), to )
import           Network.Mattermost.Types ( ServerTime(..) )
import           Prelude ()
import           Prelude.MH

import           Draw.Util
import           Markdown
import           Themes
import           Types
import           Types.DirectionalSeq

maxMessageHeight :: Int
maxMessageHeight = 200

-- | nameForUserRef converts the UserRef into a printable name, based
-- on the current known user data.
nameForUserRef :: ChatState -> UserRef -> Maybe Text
nameForUserRef st uref =
    case uref of
        NoUser -> Nothing
        UserOverride _ t -> Just t
        UserI _ uId -> displayNameForUserId uId st

-- | renderSingleMessage is the main message drawing function.
--
-- The `ind` argument specifies an "indicator boundary".  Showing
-- various indicators (e.g. "edited") is not typically done for
-- messages that are older than this boundary value.
renderSingleMessage :: ChatState
                    -> HighlightSet
                    -> Maybe ServerTime
                    -> Message
                    -> ThreadState
                    -> Widget Name
renderSingleMessage st hs ind m threadState =
  renderChatMessage st hs ind threadState (withBrackets . renderTime st . withServerTime) m

renderChatMessage :: ChatState
                  -> HighlightSet
                  -> Maybe ServerTime
                  -> ThreadState
                  -> (ServerTime -> Widget Name)
                  -> Message
                  -> Widget Name
renderChatMessage st hs ind threadState renderTimeFunc msg =
    let showOlderEdits = configShowOlderEdits $ st^.csResources.crConfiguration
        parent = case msg^.mInReplyToMsg of
          NotAReply -> Nothing
          InReplyTo pId -> getMessageForPostId st pId
        m = renderMessage MessageData
              { mdMessage           = msg
              , mdUserName          = msg^.mUser.to (nameForUserRef st)
              , mdIsOutgoing        = isMessageOutgoing msg st
              , mdParentMessage     = parent
              , mdParentUserName    = parent >>= (^.mUser.to (nameForUserRef st))
              , mdEditThreshold     = ind
              , mdHighlightSet      = hs
              , mdShowOlderEdits    = showOlderEdits
              , mdRenderReplyParent = True
              , mdIndentBlocks      = True
              , mdThreadState       = threadState
              , mdShowReactions     = True
              , mdMessageWidthLimit = Nothing
              }
        fullMsg =
          case msg^.mUser of
            NoUser
              | isGap msg -> withDefAttr gapMessageAttr m
              | otherwise ->
                case msg^.mType of
                    C DateTransition ->
                        withDefAttr dateTransitionAttr (hBorderWithLabel m)
                    C NewMessagesTransition ->
                        withDefAttr newMessageTransitionAttr (hBorderWithLabel m)
                    C Error ->
                        withDefAttr errorMessageAttr m
                    _ ->
                        withDefAttr clientMessageAttr m
            _ | isJoinLeave msg -> withDefAttr clientMessageAttr m
              | otherwise -> m
        maybeRenderTime w =
            let maybePadTime = if threadState == InThreadShowParent
                               then (txt " " <=>) else id
            in hBox [maybePadTime $ renderTimeFunc (msg^.mDate), txt " ", w]
        maybeRenderTimeWith f = if isTransition msg then id else f
    in maybeRenderTimeWith maybeRenderTime fullMsg


isMessageOutgoing :: Message -> ChatState -> Bool
isMessageOutgoing msg st = maybe False (== myUserId st) (msg^.mUser.to idForUserRef)

-- | Render a selected message with focus, including the messages
-- before and the messages after it. The foldable parameters exist
-- because (depending on the situation) we might use either of the
-- message list types for the 'before' and 'after' (i.e. the
-- chronological or retrograde message sequences).
unsafeRenderMessageSelection :: (Foldable f, Foldable g)
                             => ((Message, ThreadState), (f (Message, ThreadState), g (Message, ThreadState)))
                             -> (Message -> ThreadState -> Widget Name)
                             -> Widget Name
unsafeRenderMessageSelection ((curMsg, curThreadState), (before, after)) doMsgRender =
  Widget Greedy Greedy $ do
    ctx <- getContext
    curMsgResult <- withReaderT relaxHeight $ render $
                    forceAttr messageSelectAttr $
                    padRight Max $ doMsgRender curMsg curThreadState

    let targetHeight = ctx^.availHeightL
        upperHeight = targetHeight `div` 2
        lowerHeight = targetHeight - upperHeight

        lowerRender img (m, tState) = render1HLimit doMsgRender Vty.vertJoin targetHeight img tState m
        upperRender img (m, tState) = render1HLimit doMsgRender (flip Vty.vertJoin) targetHeight img tState m

    lowerHalf <- foldM lowerRender Vty.emptyImage after
    upperHalf <- foldM upperRender Vty.emptyImage before

    let curHeight = Vty.imageHeight $ curMsgResult^.imageL
        uncropped = upperHalf Vty.<-> curMsgResult^.imageL Vty.<-> lowerHalf
        img = if | Vty.imageHeight lowerHalf < (lowerHeight - curHeight) ->
                     Vty.cropTop targetHeight uncropped
                 | Vty.imageHeight upperHalf < upperHeight ->
                     Vty.cropBottom targetHeight uncropped
                 | otherwise ->
                     Vty.cropTop upperHeight upperHalf Vty.<-> curMsgResult^.imageL Vty.<->
                        (if curHeight < lowerHeight
                          then Vty.cropBottom (lowerHeight - curHeight) lowerHalf
                          else Vty.cropBottom lowerHeight lowerHalf)
    return $ emptyResult & imageL .~ img

renderLastMessages :: ChatState
                   -> HighlightSet
                   -> Maybe ServerTime
                   -> DirectionalSeq Retrograde (Message, ThreadState)
                   -> Widget Name
renderLastMessages st hs editCutoff msgs =
    Widget Greedy Greedy $ do
        ctx <- getContext
        let targetHeight = ctx^.availHeightL
            doMsgRender = renderSingleMessage st hs editCutoff

            newMessagesTransitions = filterMessages (isNewMessagesTransition . fst) msgs
            newMessageTransition = fst <$> (listToMaybe $ F.toList newMessagesTransitions)

            isBelow m transition = m^.mDate > transition^.mDate

            go :: Vty.Image -> DirectionalSeq Retrograde (Message, ThreadState) -> RenderM Name Vty.Image
            go img ms | messagesLength ms == 0 = return img
            go img ms = do
                let Just (m, threadState) = messagesHead ms
                    newMessagesAbove = maybe False (isBelow m) newMessageTransition
                newImg <- render1HLimit doMsgRender (flip Vty.vertJoin) targetHeight img threadState m
                -- If the new message fills the window, check whether
                -- there is still a "New Messages" transition that is
                -- not displayed. If there is, then we need to replace
                -- the top line of the new image with a "New Messages"
                -- indicator.
                if Vty.imageHeight newImg >= targetHeight && newMessagesAbove
                then do
                    transitionResult <- render $ withDefAttr newMessageTransitionAttr $
                                                 hBorderWithLabel (txt "New Messages ↑")
                    let newImg2 = Vty.vertJoin (transitionResult^.imageL)
                                               (Vty.cropTop (targetHeight - 1) newImg)
                    return newImg2
                else go newImg $ messagesDrop 1 ms

        img <- go Vty.emptyImage msgs
        return $ emptyResult & imageL .~ (Vty.cropTop targetHeight img)

relaxHeight :: Context -> Context
relaxHeight c = c & availHeightL .~ (max maxMessageHeight (c^.availHeightL))

render1HLimit :: (Message -> ThreadState -> Widget Name)
              -> (Vty.Image -> Vty.Image -> Vty.Image)
              -> Int
              -> Vty.Image
              -> ThreadState
              -> Message
              -> RenderM Name Vty.Image
render1HLimit doMsgRender fjoin lim img threadState msg
  | Vty.imageHeight img >= lim = return img
  | otherwise = fjoin img <$> render1 doMsgRender threadState msg

render1 :: (Message -> ThreadState -> Widget Name)
        -> ThreadState
        -> Message
        -> RenderM Name Vty.Image
render1 doMsgRender threadState msg = case msg^.mDeleted of
    True -> return Vty.emptyImage
    False -> do
        r <- withReaderT relaxHeight $
             render $ padRight Max $
             doMsgRender msg threadState
        return $ r^.imageL
