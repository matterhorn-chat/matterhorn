{-# LANGUAGE MultiWayIf #-}
module Events.Main where

import           Prelude ()
import           Prelude.MH

import           Brick hiding ( Direction )
import           Brick.Widgets.Edit
import qualified Brick.Widgets.List as L
import           Data.Char ( isSpace )
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (%=), (.=), to, at, _Just )

import           Command
import           Constants
import           Events.Keybindings
import           HelpTopics ( mainHelpTopic )
import           InputHistory
import           State.Help
import           State.Channels
import           State.ChannelSelect
import           State.Editing
import           State.MessageSelect
import           State.PostListOverlay ( enterFlaggedPostListMode )
import           State.UrlSelect
import           State.Messages ( sendMessage )
import           Types


onEventMain :: Vty.Event -> MH ()
onEventMain =
  handleKeyboardEvent mainKeybindings $ \ ev -> case ev of
    (Vty.EvPaste bytes) -> handlePaste bytes
    _ -> handleEditingInput ev

mainKeybindings :: KeyConfig -> [Keybinding]
mainKeybindings = mkKeybindings
    [ mkKb ShowHelpEvent
        "Show this help screen"
        (showHelpScreen mainHelpTopic)

    , mkKb EnterSelectModeEvent
        "Select a message to edit/reply/delete"
        beginMessageSelect

    , mkKb ReplyRecentEvent
        "Reply to the most recent message"
        replyToLatestMessage

    , mkKb ToggleMessagePreviewEvent "Toggle message preview"
        toggleMessagePreview

    , mkKb ToggleChannelListVisibleEvent "Toggle channel list visibility"
        toggleChannelListVisibility

    , mkKb
        InvokeEditorEvent
        "Invoke *$EDITOR* to edit the current message"
        invokeExternalEditor

    , mkKb
        EnterFastSelectModeEvent
        "Enter fast channel selection mode"
         beginChannelSelect

    , mkKb
        QuitEvent
        "Quit"
        requestQuit

    , staticKb "Tab-complete forward"
         (Vty.EvKey (Vty.KChar '\t') []) $
         tabComplete Forwards

    , staticKb "Tab-complete backward"
         (Vty.EvKey (Vty.KBackTab) []) $
         tabComplete Backwards

    , mkKb
        ScrollUpEvent
        "Scroll up in the channel input history" $ do
             -- Up in multiline mode does the usual thing; otherwise we
             -- navigate the history.
             isMultiline <- use (csEditState.cedMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (csEditState.cedEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KUp [])
                 False -> channelHistoryBackward

    , mkKb
        ScrollDownEvent
        "Scroll down in the channel input history" $ do
             -- Down in multiline mode does the usual thing; otherwise
             -- we navigate the history.
             isMultiline <- use (csEditState.cedMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (csEditState.cedEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KDown [])
                 False -> channelHistoryForward

    , mkKb PageUpEvent "Page up in the channel message list" $ do
             cId <- use csCurrentChannelId
             let vp = ChannelMessages cId
             mh $ invalidateCacheEntry vp
             mh $ vScrollToEnd $ viewportScroll vp
             mh $ vScrollBy (viewportScroll vp) (-1 * pageAmount)
             setMode ChannelScroll

    , mkKb NextChannelEvent "Change to the next channel in the channel list"
         nextChannel

    , mkKb PrevChannelEvent "Change to the previous channel in the channel list"
         prevChannel

    , mkKb NextUnreadChannelEvent "Change to the next channel with unread messages"
         nextUnreadChannel

    , mkKb NextUnreadUserOrChannelEvent "Change to the next channel with unread messages preferring direct messages"
         nextUnreadUserOrChannel

    , mkKb LastChannelEvent "Change to the most recently-focused channel"
         recentChannel

    , staticKb "Send the current message"
         (Vty.EvKey Vty.KEnter []) $ do
             isMultiline <- use (csEditState.cedMultiline)
             case isMultiline of
                 -- Enter in multiline mode does the usual thing; we
                 -- only send on Enter when we're outside of multiline
                 -- mode.
                 True -> mhHandleEventLensed (csEditState.cedEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KEnter [])
                 False -> handleInputSubmission

    , mkKb EnterOpenURLModeEvent "Select and open a URL posted to the current channel"
           startUrlSelect

    , mkKb ClearUnreadEvent "Clear the current channel's unread / edited indicators" $
           csCurrentChannel %= (clearNewMessageIndicator .
                                clearEditedThreshold)

    , mkKb ToggleMultiLineEvent "Toggle multi-line message compose mode"
           toggleMultilineEditing

    , mkKb CancelEvent "Cancel autocomplete, message reply, or edit, in that order"
         cancelAutocompleteOrReplyOrEdit

    , mkKb EnterFlaggedPostsEvent "View currently flagged posts"
         enterFlaggedPostListMode
    ]

handleInputSubmission :: MH ()
handleInputSubmission = do
  cmdLine <- use (csEditState.cedEditor)
  cId <- use csCurrentChannelId

  -- send the relevant message
  mode <- use (csEditState.cedEditMode)
  let (line:rest) = getEditContents cmdLine
      allLines = T.intercalate "\n" $ line : rest

  -- We clean up before dispatching the command or sending the message
  -- since otherwise the command could change the state and then doing
  -- cleanup afterwards could clean up the wrong things.
  csEditState.cedEditor         %= applyEdit Z.clearZipper
  csEditState.cedInputHistory   %= addHistoryEntry allLines cId
  csEditState.cedInputHistoryPosition.at cId .= Nothing

  case T.uncons allLines of
    Just ('/', cmd) -> dispatchCommand cmd
    _               -> sendMessage mode allLines

  -- Reset the autocomplete UI
  csEditState.cedAutocomplete .= Nothing

  -- Reset the edit mode *after* handling the input so that the input
  -- handler can tell whether we're editing, replying, etc.
  csEditState.cedEditMode       .= NewPost

data Direction = Forwards | Backwards

tabComplete :: Direction -> MH ()
tabComplete dir = do
    let transform list =
            let len = list^.L.listElementsL.to length
            in case dir of
                Forwards ->
                    if (L.listSelected list == Just (len - 1)) ||
                       (L.listSelected list == Nothing && len > 0)
                    then L.listMoveTo 0 list
                    else L.listMoveBy 1 list
                Backwards ->
                    if (L.listSelected list == Just 0) ||
                       (L.listSelected list == Nothing && len > 0)
                    then L.listMoveTo (len - 1) list
                    else L.listMoveBy (-1) list
    csEditState.cedAutocomplete._Just.acCompletionList %= transform

    mac <- use (csEditState.cedAutocomplete)
    case mac of
        Nothing -> return ()
        Just ac -> do
            case ac^.acCompletionList.to L.listSelectedElement of
                Nothing -> return ()
                Just (_, alternative) -> do
                    let replacement = autocompleteAlternativeReplacement alternative
                        maybeEndOfWord z =
                            if maybe True isSpace (Z.currentChar z)
                            then z
                            else Z.moveWordRight z
                    csEditState.cedEditor %=
                        applyEdit (Z.insertMany replacement . Z.deletePrevWord .
                                   maybeEndOfWord)
