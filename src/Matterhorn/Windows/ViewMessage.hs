module Matterhorn.Windows.ViewMessage
  ( viewMessageWindowTemplate
  , viewMessageKeybindings
  , viewMessageKeyHandlers
  , viewMessageReactionsKeybindings
  , viewMessageReactionsKeyHandlers
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( to )

import           Network.Mattermost.Types ( TeamId, Post (postId) )

import           Matterhorn.Constants
import           Matterhorn.Events.Keybindings
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( Inline(EUser) )
import           Matterhorn.Draw.RichText
import           Matterhorn.Draw.Messages ( renderMessage, MessageData(..), printableNameForUserRef )

-- | The template for "View Message" windows triggered by message
-- selection mode.
viewMessageWindowTemplate :: TeamId -> TabbedWindowTemplate ChatState MH Name ViewMessageWindowTab
viewMessageWindowTemplate tId =
    TabbedWindowTemplate { twtEntries = [ messageEntry tId
                                        , reactionsEntry tId
                                        ]
                         , twtTitle = const $ txt "View Message"
                         }

messageEntry :: TeamId -> TabbedWindowEntry ChatState MH Name ViewMessageWindowTab
messageEntry tId =
    TabbedWindowEntry { tweValue = VMTabMessage
                      , tweRender = renderTab tId
                      , tweHandleEvent = handleEvent tId
                      , tweTitle = tabTitle
                      , tweShowHandler = onShow tId
                      }

reactionsEntry :: TeamId -> TabbedWindowEntry ChatState MH Name ViewMessageWindowTab
reactionsEntry tId =
    TabbedWindowEntry { tweValue = VMTabReactions
                      , tweRender = renderTab tId
                      , tweHandleEvent = handleEvent tId
                      , tweTitle = tabTitle
                      , tweShowHandler = onShow tId
                      }

tabTitle :: ViewMessageWindowTab -> Bool -> T.Text
tabTitle VMTabMessage _ = "Message"
tabTitle VMTabReactions _ = "Reactions"

-- When we show the tabs, we need to reset the viewport scroll position
-- for viewports in that tab. This is because an older View Message
-- window used the same handle for the viewport and we don't want that
-- old state affecting this window. This also means that switching tabs
-- in an existing window resets this state, too.
onShow :: TeamId -> ViewMessageWindowTab -> MH ()
onShow tId VMTabMessage = resetVp $ ViewMessageArea tId
onShow tId VMTabReactions = resetVp $ ViewMessageArea tId

resetVp :: Name -> MH ()
resetVp n = do
    let vs = viewportScroll n
    mh $ do
        vScrollToBeginning vs
        hScrollToBeginning vs

renderTab :: TeamId -> ViewMessageWindowTab -> ChatState -> Widget Name
renderTab tId tab cs =
    let mLatestMessage = case cs^.csTeam(tId).tsViewedMessage of
          Nothing -> error "BUG: no message to show, please report!"
          Just (m, _) -> getLatestMessage cs tId m
    in case mLatestMessage of
        Nothing -> emptyWidget
        Just latestMessage ->
            case tab of
                VMTabMessage -> viewMessageBox cs tId latestMessage
                VMTabReactions -> reactionsText cs tId latestMessage

getLatestMessage :: ChatState -> TeamId -> Message -> Maybe Message
getLatestMessage cs tId m =
    case m^.mMessageId of
        Nothing -> Just m
        Just mId -> do
            cId <- cs^.csCurrentChannelId(tId)
            chan <- cs^?csChannel(cId)
            findMessage mId $ chan^.ccMessageInterface.miMessages

handleEvent :: TeamId -> ViewMessageWindowTab -> Vty.Event -> MH ()
handleEvent tId VMTabMessage =
    void . handleKeyboardEvent (viewMessageKeybindings tId)
handleEvent tId VMTabReactions =
    void . handleKeyboardEvent (viewMessageReactionsKeybindings tId)

reactionsText :: ChatState -> TeamId -> Message -> Widget Name
reactionsText st tId m = viewport vpName Vertical body
    where
        vpName = ViewMessageReactionsArea tId
        body = case null reacList of
            True -> txt "This message has no reactions."
            False -> vBox $ mkEntry <$> reacList
        reacList = M.toList (m^.mReactions)
        mkEntry (reactionName, userIdSet) =
            let count = str $ "(" <> show (S.size userIdSet) <> ")"
                name = withDefAttr emojiAttr $ txt $ ":" <> reactionName <> ":"
                clickableName = makeClickableName name reactionName userIdSet
                usernameList = usernameText userIdSet
            in (clickableName <+> (padLeft (Pad 1) count)) <=>
               (padLeft (Pad 2) usernameList)

        hs = getHighlightSet st tId

        clickableUsernames i (EUser un) =
            Just $ ClickableUsername Nothing vpName i un
        clickableUsernames _ _ =
            Nothing

        usernameText uids =
            renderText' Nothing (myUsername st) hs (Just clickableUsernames) $
            T.intercalate ", " $
            fmap addUserSigil $
            catMaybes (lookupUsername <$> F.toList uids)

        lookupUsername uid = usernameForUserId uid st

        makeName e us = do
            pid <- postId <$> m^.mOriginalPost
            Just $ ClickableReaction pid vpName e us

        makeClickableName w e us =
            case makeName e us of
                Just n ->  clickable n w
                Nothing -> w

viewMessageBox :: ChatState -> TeamId -> Message -> Widget Name
viewMessageBox st tId msg =
    let maybeWarn = if not (msg^.mDeleted) then id else warn
        warn w = vBox [w, hBorder, deleteWarning]
        deleteWarning = withDefAttr errorMessageAttr $
                        txtWrap $ "Alert: this message has been deleted and " <>
                                  "will no longer be accessible once this window " <>
                                  "is closed."
        mkBody vpWidth =
            let hs = getHighlightSet st tId
                parent = case msg^.mInReplyToMsg of
                     NotAReply -> Nothing
                     InReplyTo pId -> getMessageForPostId st pId
                md = MessageData { mdEditThreshold     = Nothing
                                 , mdShowOlderEdits    = False
                                 , mdMessage           = msg
                                 , mdUserName          = msg^.mUser.to (printableNameForUserRef st)
                                 , mdParentMessage     = parent
                                 , mdParentUserName    = parent >>= (^.mUser.to (printableNameForUserRef st))
                                 , mdRenderReplyParent = True
                                 , mdHighlightSet      = hs
                                 , mdIndentBlocks      = True
                                 , mdThreadState       = NoThread
                                 , mdShowReactions     = False
                                 , mdMessageWidthLimit = Just vpWidth
                                 , mdMyUsername        = myUsername st
                                 , mdMyUserId          = myUserId st
                                 , mdWrapNonhighlightedCodeBlocks = False
                                 , mdTruncateVerbatimBlocks = Nothing
                                 , mdClickableNameTag  = ViewMessageArea tId
                                 , mdRenderReplyIndent = False
                                 }
            in renderMessage md

    in Widget Greedy Greedy $ do
        ctx <- getContext
        render $ maybeWarn $ viewport (ViewMessageArea tId) Both $ mkBody (ctx^.availWidthL)

viewMessageKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
viewMessageKeybindings tId = mkKeybindings (viewMessageKeyHandlers tId)

viewMessageKeyHandlers :: TeamId -> [KeyEventHandler]
viewMessageKeyHandlers tId =
    let vs = viewportScroll . ViewMessageArea
    in [ mkKb PageUpEvent "Page up" $ do
           mh $ vScrollBy (vs tId) (-1 * pageAmount)

       , mkKb PageDownEvent "Page down" $ do
           mh $ vScrollBy (vs tId) pageAmount

       , mkKb PageLeftEvent "Page left" $ do
           mh $ hScrollBy (vs tId) (-2 * pageAmount)

       , mkKb PageRightEvent "Page right" $ do
           mh $ hScrollBy (vs tId) (2 * pageAmount)

       , mkKb ScrollUpEvent "Scroll up" $ do
           mh $ vScrollBy (vs tId) (-1)

       , mkKb ScrollDownEvent "Scroll down" $ do
           mh $ vScrollBy (vs tId) 1

       , mkKb ScrollLeftEvent "Scroll left" $ do
           mh $ hScrollBy (vs tId) (-1)

       , mkKb ScrollRightEvent "Scroll right" $ do
           mh $ hScrollBy (vs tId) 1

       , mkKb ScrollBottomEvent "Scroll to the end of the message" $ do
           mh $ vScrollToEnd (vs tId)

       , mkKb ScrollTopEvent "Scroll to the beginning of the message" $ do
           mh $ vScrollToBeginning (vs tId)
       ]

viewMessageReactionsKeybindings :: TeamId -> KeyConfig -> KeyHandlerMap
viewMessageReactionsKeybindings tId = mkKeybindings (viewMessageReactionsKeyHandlers tId)

viewMessageReactionsKeyHandlers :: TeamId -> [KeyEventHandler]
viewMessageReactionsKeyHandlers tId =
    let vs = viewportScroll . ViewMessageReactionsArea
    in [ mkKb PageUpEvent "Page up" $ do
           mh $ vScrollBy (vs tId) (-1 * pageAmount)

       , mkKb PageDownEvent "Page down" $ do
           mh $ vScrollBy (vs tId) pageAmount

       , mkKb ScrollUpEvent "Scroll up" $ do
           mh $ vScrollBy (vs tId) (-1)

       , mkKb ScrollDownEvent "Scroll down" $ do
           mh $ vScrollBy (vs tId) 1

       , mkKb ScrollBottomEvent "Scroll to the end of the reactions list" $ do
           mh $ vScrollToEnd (vs tId)

       , mkKb ScrollTopEvent "Scroll to the beginning of the reactions list" $ do
           mh $ vScrollToBeginning (vs tId)
       ]
