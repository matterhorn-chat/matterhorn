{-# LANGUAGE MultiWayIf #-}
module Events.Main where

import Prelude ()
import Prelude.Compat

import Brick
import Brick.Widgets.Edit
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import Types.Channels (ccInfo, cdType, clearNewMessageIndicator, clearEditedThreshold)
import Types.Users (uiDeleted, uiName)
import Events.Keybindings
import State
import State.PostListOverlay (enterFlaggedPostListMode)
import State.Editing
import Command
import Completion
import InputHistory
import HelpTopics (mainHelpTopic)
import Constants

import Network.Mattermost.Types (Type(..))

onEventMain :: Vty.Event -> MH ()
onEventMain =
  handleKeyboardEvent mainKeybindings $ \ ev -> case ev of
    (Vty.EvPaste bytes) -> handlePaste bytes
    _ -> handleEditingInput ev
  -- conf <- use (csResources.crConfiguration)
  -- let keyMap = mainKeybindings (configUserKeys conf)
  -- case e of
  --   _ | Just kb <- lookupKeybinding e keyMap -> kbAction kb
  --   (Vty.EvPaste bytes) -> handlePaste bytes
  --   _ -> handleEditingInput e

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
             csMode .= ChannelScroll

    , mkKb NextChannelEvent "Change to the next channel in the channel list"
         nextChannel

    , mkKb PrevChannelEvent "Change to the previous channel in the channel list"
         prevChannel

    , mkKb NextUnreadChannelEvent "Change to the next channel with unread messages"
         nextUnreadChannel

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
                 False -> do
                   csEditState.cedCurrentCompletion .= Nothing
                   handleInputSubmission

    , mkKb EnterOpenURLModeEvent "Select and open a URL posted to the current channel"
           startUrlSelect

    , mkKb ClearUnreadEvent "Clear the current channel's unread / edited indicators" $
           csCurrentChannel %= (clearNewMessageIndicator .
                                clearEditedThreshold)

    , mkKb ToggleMultiLineEvent "Toggle multi-line message compose mode"
           toggleMultilineEditing

    , mkKb CancelEvent "Cancel message reply or update"
         cancelReplyOrEdit

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

  -- Reset the edit mode *after* handling the input so that the input
  -- handler can tell whether we're editing, replying, etc.
  csEditState.cedEditMode       .= NewPost

tabComplete :: Completion.Direction -> MH ()
tabComplete dir = do
  st <- use id
  allUserIds <- getAllUserIds
  allChanNames <- getAllChannelNames

  let completableChannels = catMaybes (flip map allChanNames $ \cname -> do
          -- Only permit completion of channel names for non-Group channels
          ch <- getChannelByName' st cname
          case ch^.ccInfo.cdType of
              Group -> Nothing
              _     -> Just cname
          )

      completableUsers = catMaybes (flip map allUserIds $ \uId -> do
          -- Only permit completion of user names for non-deleted users
          case getUserById' uId st of
              Nothing -> Nothing
              Just u ->
                  if u^.uiDeleted
                     then Nothing
                     else Just $ u^.uiName
          )

      priorities  = [] :: [T.Text]-- XXX: add recent completions to this
      completions = Set.fromList (completableUsers ++
                                  completableChannels ++
                                  map (userSigil <>) completableUsers ++
                                  map (normalChannelSigil <>) completableChannels ++
                                  map ("/" <>) (commandName <$> commandList))

      line        = Z.currentLine $ st^.csEditState.cedEditor.editContentsL
      curComp     = st^.csEditState.cedCurrentCompletion
      (nextComp, alts) = case curComp of
          Nothing -> let cw = currentWord line
                     in (Just cw, filter (cw `T.isPrefixOf`) $ Set.toList completions)
          Just cw -> (Just cw, filter (cw `T.isPrefixOf`) $ Set.toList completions)

      mb_word     = wordComplete dir priorities completions line curComp
  csEditState.cedCurrentCompletion .= nextComp
  csEditState.cedCompletionAlternatives .= alts
  let (edit, curAlternative) = case mb_word of
          Nothing -> (id, "")
          Just w -> (Z.insertMany w . Z.deletePrevWord, w)

  csEditState.cedEditor %= (applyEdit edit)
  csEditState.cedCurrentAlternative .= curAlternative
