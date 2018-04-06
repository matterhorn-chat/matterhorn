{-# LANGUAGE MultiWayIf #-}
module Events.Main where

import Prelude ()
import Prelude.MH

import Brick hiding (Direction)
import Brick.Widgets.Edit
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform ((%=), (.=), to, at)

import Types
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
             setMode ChannelScroll

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
                   csEditState.cedCompleter .= Nothing
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

data Direction = Forwards | Backwards

tabComplete :: Direction -> MH ()
tabComplete dir = do
  st <- use id
  allUIds <- gets allUserIds
  allChanNames <- gets allChannelNames
  displayNick <- use (to useNickname)

  let channelCompletions = concat $ catMaybes (flip map allChanNames $ \cname -> do
          -- Only permit completion of channel names for non-Group channels
          ch <- channelByName cname st
          case ch^.ccInfo.cdType of
              Group -> Nothing
              _     -> Just [ (cname, normalChannelSigil <> cname)
                            , dupe $ normalChannelSigil <> cname
                            ]
          )

      userCompletions = concat $ catMaybes (flip map allUIds $ \uId ->
          -- Only permit completion of user names for non-deleted users
          case userById uId st of
              Nothing -> Nothing
              Just u | u^.uiDeleted -> Nothing
              Just u ->
                  let mNick = case u^.uiNickName of
                        Just nick | displayNick -> [ (userSigil <> nick, userSigil <> u^.uiName)
                                                   , (nick, userSigil <> u^.uiName)
                                                   ]
                        _ -> []
                  in Just $ [ (u^.uiName, userSigil <> u^.uiName)
                            , dupe $ userSigil <> u^.uiName
                            ] <> mNick
          )

      commandCompletions = dupe <$> map ("/" <>) (commandName <$> commandList)
      dupe a = (a, a)
      completions = Set.fromList (userCompletions ++
                                  channelCompletions ++
                                  commandCompletions)

  mCompleter <- use (csEditState.cedCompleter)
  case mCompleter of
      Just _ -> do
          -- Since there is already a completion in progress, cycle it
          -- according to the directional preference.
          let func = case dir of
                Forwards -> nextCompletion
                Backwards -> previousCompletion
          csEditState.cedCompleter %= fmap func
      Nothing -> do
          -- There is no completion in progress, so start a new
          -- completion from the current input.
          let line = Z.currentLine $ st^.csEditState.cedEditor.editContentsL
          case wordComplete completions line of
              Nothing ->
                  -- No matches were found, so do nothing.
                  return ()
              Just (Left single) ->
                  -- Only a single match was found, so just replace the
                  -- current word with the only match.
                  csEditState.cedEditor %= applyEdit (Z.insertMany single . Z.deletePrevWord)
              Just (Right many) -> do
                  -- More than one match was found, so start a
                  -- completion by storing the completer state.
                  csEditState.cedCompleter .= Just many

  -- Get the current completer state (potentially just cycled to
  -- the next completion above) and update the editor with the current
  -- alternative.
  mComp <- use (csEditState.cedCompleter)
  case mComp of
      Nothing -> return ()
      Just comp -> do
          let replacement = snd $ currentAlternative comp
          csEditState.cedEditor %= applyEdit (Z.insertMany replacement . Z.deletePrevWord)
