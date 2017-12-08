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
import Types.Users (uiDeleted, findUserById)
import Types.Keybindings
import State
import State.PostListOverlay (enterFlaggedPostListMode)
import State.Editing
import Command
import Completion
import InputHistory
import HelpTopics (mainHelpTopic)
import Constants

import Network.Mattermost (Type(..))

onEventMain :: Vty.Event -> MH ()
onEventMain e | Just kb <- lookupKeybinding e mainKeybindings = kbAction kb
onEventMain (Vty.EvPaste bytes) = handlePaste bytes
onEventMain e = handleEditingInput e

mainKeybindings :: [Keybinding]
mainKeybindings =
    [ KB "Show this help screen"
         (Vty.EvKey (Vty.KFun 1) []) $
         showHelpScreen mainHelpTopic

    , KB "Select a message to edit/reply/delete"
         (Vty.EvKey (Vty.KChar 's') [Vty.MCtrl]) $
         beginMessageSelect

    , KB "Reply to the most recent message"
         (Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]) $
         replyToLatestMessage

    , KB "Toggle message preview"
         (Vty.EvKey (Vty.KChar 'p') [Vty.MMeta]) $
         toggleMessagePreview

    , KB "Invoke *$EDITOR* to edit the current message"
         (Vty.EvKey (Vty.KChar 'k') [Vty.MMeta]) $
         invokeExternalEditor

    , KB "Enter fast channel selection mode"
         (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) $
         beginChannelSelect

    , KB "Quit"
         (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) $ requestQuit

    , KB "Tab-complete forward"
         (Vty.EvKey (Vty.KChar '\t') []) $
         tabComplete Forwards

    , KB "Tab-complete backward"
         (Vty.EvKey (Vty.KBackTab) []) $
         tabComplete Backwards

    , KB "Scroll up in the channel input history"
         (Vty.EvKey Vty.KUp []) $ do
             -- Up in multiline mode does the usual thing; otherwise we
             -- navigate the history.
             isMultiline <- use (csEditState.cedMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (csEditState.cedEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KUp [])
                 False -> channelHistoryBackward

    , KB "Scroll down in the channel input history"
         (Vty.EvKey Vty.KDown []) $ do
             -- Down in multiline mode does the usual thing; otherwise
             -- we navigate the history.
             isMultiline <- use (csEditState.cedMultiline)
             case isMultiline of
                 True -> mhHandleEventLensed (csEditState.cedEditor) handleEditorEvent
                                           (Vty.EvKey Vty.KDown [])
                 False -> channelHistoryForward

    , KB "Page up in the channel message list"
         (Vty.EvKey Vty.KPageUp []) $ do
             cId <- use csCurrentChannelId
             let vp = ChannelMessages cId
             mh $ invalidateCacheEntry vp
             mh $ vScrollToEnd $ viewportScroll vp
             mh $ vScrollBy (viewportScroll vp) (-1 * pageAmount)
             csMode .= ChannelScroll

    , KB "Change to the next channel in the channel list"
         (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) $
         nextChannel

    , KB "Change to the previous channel in the channel list"
         (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) $
         prevChannel

    , KB "Change to the next channel with unread messages"
         (Vty.EvKey (Vty.KChar 'a') [Vty.MMeta]) $
         nextUnreadChannel

    , KB "Change to the most recently-focused channel"
         (Vty.EvKey (Vty.KChar 's') [Vty.MMeta]) $
         recentChannel

    , KB "Send the current message"
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

    , KB "Select and open a URL posted to the current channel"
         (Vty.EvKey (Vty.KChar 'o') [Vty.MCtrl]) $
           startUrlSelect

    , KB "Clear the current channel's unread / edited indicators"
         (Vty.EvKey (Vty.KChar 'l') [Vty.MMeta]) $
           csCurrentChannel %= (clearNewMessageIndicator .
                                clearEditedThreshold)

    , KB "Toggle multi-line message compose mode"
         (Vty.EvKey (Vty.KChar 'e') [Vty.MMeta]) $
           toggleMultilineEditing

    , KB "Cancel message reply or update"
         (Vty.EvKey Vty.KEsc []) $
         cancelReplyOrEdit

    , KB "Cancel message reply or update"
         (Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl]) $
         cancelReplyOrEdit

    , KB "View currently flagged posts"
         (Vty.EvKey (Vty.KChar '8') [Vty.MMeta]) $
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

  case T.uncons line of
    Just ('/',cmd) -> dispatchCommand cmd
    _              -> sendMessage mode allLines

  -- Reset the edit mode *after* handling the input so that the input
  -- handler can tell whether we're editing, replying, etc.
  csEditState.cedEditMode       .= NewPost

tabComplete :: Completion.Direction -> MH ()
tabComplete dir = do
  st <- use id
  knownUsers <- use csUsers

  let completableChannels = catMaybes (flip map (st^.csNames.cnChans) $ \cname -> do
          -- Only permit completion of channel names for non-Group channels
          cId <- st^.csNames.cnToChanId.at cname
          let cType = st^?csChannel(cId).ccInfo.cdType
          case cType of
              Just Group -> Nothing
              _          -> Just cname
          )

      completableUsers = catMaybes (flip map (st^.csNames.cnUsers) $ \uname -> do
          -- Only permit completion of user names for non-deleted users
          uId <- st^.csNames.cnToUserId.at uname
          case findUserById uId knownUsers of
              Nothing -> Nothing
              Just u ->
                  if u^.uiDeleted
                     then Nothing
                     else Just uname
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
