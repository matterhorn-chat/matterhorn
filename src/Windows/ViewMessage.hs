module Windows.ViewMessage
  ( viewMessageWindowTemplate
  , viewMessageKeybindings
  , viewMessageReactionsKeybindings
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

tabTitle :: ViewMessageWindowTab -> Bool -> Widget Name
tabTitle VMTabMessage _ = txt "Message"
tabTitle VMTabReactions _ = txt "Reactions"

onShow :: ViewMessageWindowTab -> MH ()
onShow VMTabMessage = do
    let vs = viewportScroll ViewMessageArea

    -- When we show the message tab, we need to reset the rendering
    -- cache of the tab's contents and reset the viewport scroll
    -- position. This is because an older View Message window used the
    -- same handle for the viewport and we don't want that old state
    -- affecting this window. This also means that switching tabs in an
    -- existing window resets this state, too.
    mh $ do
        vScrollToBeginning vs
        hScrollToBeginning vs
        invalidateCacheEntry ViewMessageArea
onShow VMTabReactions = do
    let vs = viewportScroll ViewMessageReactionsArea

    -- When we show the reactions tab, we need to reset the rendering
    -- cache of the tab's contents and reset the viewport scroll
    -- position. This is because an older View Message window used the
    -- same handle for the viewport and we don't want that old state
    -- affecting this window. This also means that switching tabs in an
    -- existing window resets this state, too.
    mh $ do
        vScrollToBeginning vs
        hScrollToBeginning vs
        invalidateCacheEntry ViewMessageReactionsArea

renderTab :: ViewMessageWindowTab -> ChatState -> Widget Name
renderTab VMTabMessage cs =
    viewMessageBox cs
renderTab VMTabReactions cs =
    case cs^.csViewedMessage of
        Nothing -> error "BUG: renderReactionsTab: nothing found"
        Just (m, _) -> reactionsText cs m

handleEvent :: ViewMessageWindowTab -> Vty.Event -> MH ()
handleEvent VMTabMessage =
    handleKeyboardEvent viewMessageKeybindings (const $ return ())
handleEvent VMTabReactions =
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
    let mkBody vpWidth = case st^.csViewedMessage of
          Nothing -> str "BUG: no message to show, please report!"
          Just (msg, _) ->
              let hs = getHighlightSet st
                  parent = case msg^.mInReplyToMsg of
                       NotAReply -> Nothing
                       InReplyTo pId -> getMessageForPostId st pId
                  msgW = renderMessage md
                  md = MessageData { mdEditThreshold     = Nothing
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
                                   , mdMessageWidthLimit = Just vpWidth
                                   }
              in cached ViewMessageArea msgW

    in Widget Greedy Greedy $ do
        ctx <- getContext
        render $ viewport ViewMessageArea Both $ mkBody (ctx^.availWidthL)

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

           , mkKb ScrollBottomEvent "Scroll to the end of the reactions list" $
               mh $ vScrollToEnd vs

           , mkKb ScrollTopEvent "Scroll to the beginning of the reactions list" $
               mh $ vScrollToBeginning vs
           ]
