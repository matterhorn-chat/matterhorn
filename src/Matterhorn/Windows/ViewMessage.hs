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
import           Brick.Keybindings
import           Brick.Widgets.Border

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( to )

import           Network.Mattermost.Types ( TeamId, Post (postId) )

import           Matterhorn.Constants
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
                                        , authorInfoEntry tId
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

authorInfoEntry :: TeamId -> TabbedWindowEntry ChatState MH Name ViewMessageWindowTab
authorInfoEntry tId =
    TabbedWindowEntry { tweValue = VMTabAuthorInfo
                      , tweRender = renderTab tId
                      , tweHandleEvent = handleEvent tId
                      , tweTitle = tabTitle
                      , tweShowHandler = onShow tId
                      }

tabTitle :: ViewMessageWindowTab -> Bool -> T.Text
tabTitle VMTabMessage _ = "Message"
tabTitle VMTabReactions _ = "Reactions"
tabTitle VMTabAuthorInfo _ = "Author"

-- When we show the tabs, we need to reset the viewport scroll position
-- for viewports in that tab. This is because an older View Message
-- window used the same handle for the viewport and we don't want that
-- old state affecting this window. This also means that switching tabs
-- in an existing window resets this state, too.
onShow :: TeamId -> ViewMessageWindowTab -> MH ()
onShow tId VMTabMessage = resetVp $ ViewMessageArea tId
onShow tId VMTabReactions = resetVp $ ViewMessageReactionsArea tId
onShow tId VMTabAuthorInfo = resetVp $ ViewMessageAuthorArea tId

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
                VMTabAuthorInfo -> authorInfo cs tId latestMessage

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
    void . mhHandleKeyboardEvent (viewMessageKeybindings tId)
handleEvent tId VMTabReactions =
    void . mhHandleKeyboardEvent (viewMessageReactionsKeybindings tId)
handleEvent _ VMTabAuthorInfo =
    const $ return ()

authorInfo :: ChatState -> TeamId -> Message -> Widget Name
authorInfo cs tId m =
    let mkPairs u = [ ("Username:", _uiName u)
                    , ("Nickname:", fromMaybe "(none)" $ _uiNickName u)
                    , ("Full name:", _uiFirstName u <> " " <> _uiLastName u)
                    ]
        renderPair (label, value) =
            txt label <+> padBottom (Pad 1) (padLeft (Pad 1) (withDefAttr clientEmphAttr $ txt value))
    in case _mUser m of
        NoUser -> txt "No author."
        UserOverride _ label -> txt $ "User: " <> label
        UserI _ uId ->
            case userById uId cs of
                Nothing ->
                    txt "Author info not loaded yet."
                Just author ->
                    viewport (ViewMessageAuthorArea tId) Vertical $
                    vBox $ renderPair <$> mkPairs author

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

viewMessageKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
viewMessageKeybindings tId kc = unsafeKeyDispatcher kc (viewMessageKeyHandlers tId)

viewMessageKeyHandlers :: TeamId -> [MHKeyEventHandler]
viewMessageKeyHandlers tId =
    let vs = viewportScroll . ViewMessageArea
    in [ onEvent PageUpEvent "Page up" $ do
           mh $ vScrollBy (vs tId) (-1 * pageAmount)

       , onEvent PageDownEvent "Page down" $ do
           mh $ vScrollBy (vs tId) pageAmount

       , onEvent PageLeftEvent "Page left" $ do
           mh $ hScrollBy (vs tId) (-2 * pageAmount)

       , onEvent PageRightEvent "Page right" $ do
           mh $ hScrollBy (vs tId) (2 * pageAmount)

       , onEvent ScrollUpEvent "Scroll up" $ do
           mh $ vScrollBy (vs tId) (-1)

       , onEvent ScrollDownEvent "Scroll down" $ do
           mh $ vScrollBy (vs tId) 1

       , onEvent ScrollLeftEvent "Scroll left" $ do
           mh $ hScrollBy (vs tId) (-1)

       , onEvent ScrollRightEvent "Scroll right" $ do
           mh $ hScrollBy (vs tId) 1

       , onEvent ScrollBottomEvent "Scroll to the end of the message" $ do
           mh $ vScrollToEnd (vs tId)

       , onEvent ScrollTopEvent "Scroll to the beginning of the message" $ do
           mh $ vScrollToBeginning (vs tId)
       ]

viewMessageReactionsKeybindings :: TeamId -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
viewMessageReactionsKeybindings tId kc = unsafeKeyDispatcher kc (viewMessageReactionsKeyHandlers tId)

viewMessageReactionsKeyHandlers :: TeamId -> [MHKeyEventHandler]
viewMessageReactionsKeyHandlers tId =
    let vs = viewportScroll . ViewMessageReactionsArea
    in [ onEvent PageUpEvent "Page up" $ do
           mh $ vScrollBy (vs tId) (-1 * pageAmount)

       , onEvent PageDownEvent "Page down" $ do
           mh $ vScrollBy (vs tId) pageAmount

       , onEvent ScrollUpEvent "Scroll up" $ do
           mh $ vScrollBy (vs tId) (-1)

       , onEvent ScrollDownEvent "Scroll down" $ do
           mh $ vScrollBy (vs tId) 1

       , onEvent ScrollBottomEvent "Scroll to the end of the reactions list" $ do
           mh $ vScrollToEnd (vs tId)

       , onEvent ScrollTopEvent "Scroll to the beginning of the reactions list" $ do
           mh $ vScrollToBeginning (vs tId)
       ]
