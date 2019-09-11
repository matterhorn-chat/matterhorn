module Windows.ViewMessage
  ( viewMessageWindowTemplate
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( to )

import           Constants
import           Events.Keybindings
import           Themes
import           Types
import           Markdown
import           Draw.Messages ( nameForUserRef )

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
                      , tweRender = renderMessageTab
                      , tweHandleEvent = handleEventMessageTab
                      , tweTitle = const $ const $ txt "Message"
                      , tweShowHandler = onShowMessage
                      }

onShowMessage :: ViewMessageWindowTab -> MH ()
onShowMessage _ = do
    let vs = viewportScroll ViewMessageArea
    mh $ do
        vScrollToBeginning vs
        hScrollToBeginning vs
        invalidateCacheEntry ViewMessageArea

reactionsEntry :: TabbedWindowEntry ViewMessageWindowTab
reactionsEntry =
    TabbedWindowEntry { tweValue = VMTabReactions
                      , tweRender = renderReactionsTab
                      , tweHandleEvent = handleEventReactionsTab
                      , tweTitle = const $ const $ txt "Reactions"
                      , tweShowHandler = onShowReactions
                      }

onShowReactions :: ViewMessageWindowTab -> MH ()
onShowReactions _ = do
    let vs = viewportScroll ViewMessageReactionsArea
    mh $ do
        vScrollToBeginning vs
        hScrollToBeginning vs
        invalidateCacheEntry ViewMessageReactionsArea

renderMessageTab :: ViewMessageWindowTab -> ChatState -> Widget Name
renderMessageTab _ cs = viewMessageBox cs

renderReactionsTab :: ViewMessageWindowTab -> ChatState -> Widget Name
renderReactionsTab _ cs = case cs^.csViewedMessage of
    Nothing -> error "BUG: renderReactionsTab: nothing found"
    Just (m, _) -> reactionsText cs m

handleEventMessageTab :: ViewMessageWindowTab -> Vty.Event -> MH ()
handleEventMessageTab _ =
    handleKeyboardEvent viewMessageKeybindings (const $ return ())

handleEventReactionsTab :: ViewMessageWindowTab -> Vty.Event -> MH ()
handleEventReactionsTab _ =
    handleKeyboardEvent viewMessageReactionsKeybindings (const $ return ())

reactionsText :: ChatState -> Message -> Widget Name
reactionsText st m = viewport ViewMessageReactionsArea Vertical $
                     cached ViewMessageReactionsArea body
    where
        body = case null reacList of
            True -> emptyWidget
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
            renderText' hs $
            T.intercalate ", " $
            fmap (userSigil <>) $
            catMaybes (lookupUsername <$> F.toList uids)

        lookupUsername uid = usernameForUserId uid st

viewMessageBox :: ChatState -> Widget Name
viewMessageBox st =
    let body = case st^.csViewedMessage of
          Nothing -> str "BUG: no message to show, please report!"
          Just (msg, _) ->
              let hs = getHighlightSet st
                  parent = case msg^.mInReplyToMsg of
                       NotAReply -> Nothing
                       InReplyTo pId -> getMessageForPostId st pId
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
              in cached ViewMessageArea msgW

    in viewport ViewMessageArea Both body

viewMessageKeybindings :: KeyConfig -> [Keybinding]
viewMessageKeybindings =
    let vs = viewportScroll ViewMessageArea
    in mkKeybindings
           [ mkKb PageUpEvent "Page up" $
               mh $ vScrollBy vs (-1 * pageAmount)

           , mkKb PageDownEvent "Page down" $
               mh $ vScrollBy vs pageAmount

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

viewMessageReactionsKeybindings :: KeyConfig -> [Keybinding]
viewMessageReactionsKeybindings =
    let vs = viewportScroll ViewMessageReactionsArea
    in mkKeybindings
           [ mkKb PageUpEvent "Page up" $
               mh $ vScrollBy vs (-1 * pageAmount)

           , mkKb PageDownEvent "Page down" $
               mh $ vScrollBy vs pageAmount

           , mkKb ScrollUpEvent "Scroll up" $
               mh $ vScrollBy vs (-1)

           , mkKb ScrollDownEvent "Scroll down" $
               mh $ vScrollBy vs 1

           , mkKb ScrollBottomEvent "Scroll to the end of the message" $
               mh $ vScrollToEnd vs

           , mkKb ScrollTopEvent "Scroll to the beginning of the message" $
               mh $ vScrollToBeginning vs
           ]
