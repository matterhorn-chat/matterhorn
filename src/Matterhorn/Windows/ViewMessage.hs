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
import           Data.Maybe ( fromJust )
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( to )

import           Matterhorn.Constants
import           Matterhorn.Events.Keybindings
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Draw.RichText
import           Matterhorn.Draw.Messages ( renderMessage, MessageData(..), nameForUserRef )

-- | The template for "View Message" windows triggered by message
-- selection mode.
viewMessageWindowTemplate :: TabbedWindowTemplate ViewMessageWindowTab
viewMessageWindowTemplate =
    TabbedWindowTemplate { twtEntries = [ messageEntry
                                        , reactionsEntry
                                        ]
                         , twtTitle = const $ txt "View Message"
                         }

messageEntry :: TabbedWindowEntry ViewMessageWindowTab
messageEntry =
    TabbedWindowEntry { tweValue = VMTabMessage
                      , tweRender = renderTab
                      , tweHandleEvent = handleEvent
                      , tweTitle = tabTitle
                      , tweShowHandler = onShow
                      }

reactionsEntry :: TabbedWindowEntry ViewMessageWindowTab
reactionsEntry =
    TabbedWindowEntry { tweValue = VMTabReactions
                      , tweRender = renderTab
                      , tweHandleEvent = handleEvent
                      , tweTitle = tabTitle
                      , tweShowHandler = onShow
                      }

tabTitle :: ViewMessageWindowTab -> Bool -> T.Text
tabTitle VMTabMessage _ = "Message"
tabTitle VMTabReactions _ = "Reactions"

-- When we show the tabs, we need to reset the viewport scroll position
-- for viewports in that tab. This is because an older View Message
-- window used the same handle for the viewport and we don't want that
-- old state affecting this window. This also means that switching tabs
-- in an existing window resets this state, too.
onShow :: ViewMessageWindowTab -> MH ()
onShow VMTabMessage = resetVp ViewMessageArea
onShow VMTabReactions = resetVp ViewMessageArea

resetVp :: Name -> MH ()
resetVp n = do
    let vs = viewportScroll n
    mh $ do
        vScrollToBeginning vs
        hScrollToBeginning vs

renderTab :: ViewMessageWindowTab -> ChatState -> Widget Name
renderTab tab cs =
    let latestMessage = case cs^.csCurrentTeam.tsViewedMessage of
          Nothing -> error "BUG: no message to show, please report!"
          Just (m, _) -> getLatestMessage cs m
    in case tab of
        VMTabMessage -> viewMessageBox cs latestMessage
        VMTabReactions -> reactionsText cs latestMessage

getLatestMessage :: ChatState -> Message -> Message
getLatestMessage cs m =
    case m^.mMessageId of
        Nothing -> m
        Just mId -> fromJust $ findMessage mId $ cs^.csCurrentChannel.ccContents.cdMessages

handleEvent :: ViewMessageWindowTab -> Vty.Event -> MH ()
handleEvent VMTabMessage =
    void . handleKeyboardEvent viewMessageKeybindings (const $ return ())
handleEvent VMTabReactions =
    void . handleKeyboardEvent viewMessageReactionsKeybindings (const $ return ())

reactionsText :: ChatState -> Message -> Widget Name
reactionsText st m = viewport ViewMessageReactionsArea Vertical body
    where
        body = case null reacList of
            True -> txt "This message has no reactions."
            False -> vBox $ mkEntry <$> reacList
        reacList = M.toList (m^.mReactions)
        mkEntry (reactionName, userIdSet) =
            let count = str $ "(" <> show (S.size userIdSet) <> ")"
                name = withDefAttr emojiAttr $ txt $ ":" <> reactionName <> ":"
                usernameList = usernameText userIdSet
            in (name <+> (padLeft (Pad 1) count)) <=>
               (padLeft (Pad 2) usernameList)

        hs = getHighlightSet st

        usernameText uids =
            renderText' Nothing (myUsername st) hs $
            T.intercalate ", " $
            fmap (userSigil <>) $
            catMaybes (lookupUsername <$> F.toList uids)

        lookupUsername uid = usernameForUserId uid st

viewMessageBox :: ChatState -> Message -> Widget Name
viewMessageBox st msg =
    let maybeWarn = if not (msg^.mDeleted) then id else warn
        warn w = vBox [w, hBorder, deleteWarning]
        deleteWarning = withDefAttr errorMessageAttr $
                        txtWrap $ "Alert: this message has been deleted and " <>
                                  "will no longer be accessible once this window " <>
                                  "is closed."
        mkBody vpWidth =
            let hs = getHighlightSet st
                parent = case msg^.mInReplyToMsg of
                     NotAReply -> Nothing
                     InReplyTo pId -> getMessageForPostId st pId
                md = MessageData { mdEditThreshold     = Nothing
                                 , mdShowOlderEdits    = False
                                 , mdMessage           = msg
                                 , mdUserName          = msg^.mUser.to (nameForUserRef st)
                                 , mdParentMessage     = parent
                                 , mdParentUserName    = parent >>= (^.mUser.to (nameForUserRef st))
                                 , mdRenderReplyParent = True
                                 , mdHighlightSet      = hs
                                 , mdIndentBlocks      = True
                                 , mdThreadState       = NoThread
                                 , mdShowReactions     = False
                                 , mdMessageWidthLimit = Just vpWidth
                                 , mdMyUsername        = myUsername st
                                 , mdWrapNonhighlightedCodeBlocks = False
                                 }
            in renderMessage md

    in Widget Greedy Greedy $ do
        ctx <- getContext
        render $ maybeWarn $ viewport ViewMessageArea Both $ mkBody (ctx^.availWidthL)

viewMessageKeybindings :: KeyConfig -> KeyHandlerMap
viewMessageKeybindings = mkKeybindings viewMessageKeyHandlers

viewMessageKeyHandlers :: [KeyEventHandler]
viewMessageKeyHandlers =
    let vs = viewportScroll ViewMessageArea
    in [ mkKb PageUpEvent "Page up" $
           mh $ vScrollBy vs (-1 * pageAmount)

       , mkKb PageDownEvent "Page down" $
           mh $ vScrollBy vs pageAmount

       , mkKb PageLeftEvent "Page left" $
           mh $ hScrollBy vs (-2 * pageAmount)

       , mkKb PageRightEvent "Page right" $
           mh $ hScrollBy vs (2 * pageAmount)

       , mkKb ScrollUpEvent "Scroll up" $
           mh $ vScrollBy vs (-1)

       , mkKb ScrollDownEvent "Scroll down" $
           mh $ vScrollBy vs 1

       , mkKb ScrollLeftEvent "Scroll left" $
           mh $ hScrollBy vs (-1)

       , mkKb ScrollRightEvent "Scroll right" $
           mh $ hScrollBy vs 1

       , mkKb ScrollBottomEvent "Scroll to the end of the message" $
           mh $ vScrollToEnd vs

       , mkKb ScrollTopEvent "Scroll to the beginning of the message" $
           mh $ vScrollToBeginning vs
       ]

viewMessageReactionsKeybindings :: KeyConfig -> KeyHandlerMap
viewMessageReactionsKeybindings = mkKeybindings viewMessageReactionsKeyHandlers

viewMessageReactionsKeyHandlers :: [KeyEventHandler]
viewMessageReactionsKeyHandlers =
    let vs = viewportScroll ViewMessageReactionsArea
    in [ mkKb PageUpEvent "Page up" $
           mh $ vScrollBy vs (-1 * pageAmount)

       , mkKb PageDownEvent "Page down" $
           mh $ vScrollBy vs pageAmount

       , mkKb ScrollUpEvent "Scroll up" $
           mh $ vScrollBy vs (-1)

       , mkKb ScrollDownEvent "Scroll down" $
           mh $ vScrollBy vs 1

       , mkKb ScrollBottomEvent "Scroll to the end of the reactions list" $
           mh $ vScrollToEnd vs

       , mkKb ScrollTopEvent "Scroll to the beginning of the reactions list" $
           mh $ vScrollToBeginning vs
       ]
