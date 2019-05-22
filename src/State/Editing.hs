{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module State.Editing
  ( requestSpellCheck
  , editingKeybindings
  , toggleMultilineEditing
  , invokeExternalEditor
  , handlePaste
  , handleInputSubmission
  , getEditorContent
  , handleEditingInput
  , cancelAutocompleteOrReplyOrEdit
  , replyToLatestMessage
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( invalidateCache, invalidateCacheEntry )
import           Brick.Widgets.Edit ( Editor, applyEdit , handleEditorEvent
                                    , getEditContents, editContentsL )
import qualified Brick.Widgets.List as L
import qualified Codec.Binary.UTF8.Generic as UTF8
import           Control.Arrow
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import           Data.Time ( getCurrentTime )
import           Graphics.Vty ( Event(..), Key(..), Modifier(..) )
import           Lens.Micro.Platform ( (%=), (.=), (.~), to )
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.IO.Temp as Sys
import qualified System.Process as Sys
import           Text.Aspell ( AspellResponse(..), mistakeWord, askAspell )

import           Network.Mattermost.Types ( Post(..), ChannelId )

import           Config
import {-# SOURCE #-} Command ( dispatchCommand )
import           InputHistory
import           Events.Keybindings
import           State.Common
import           State.Autocomplete
import           State.Attachments
import           State.Messages
import           Types hiding ( newState )
import           Types.Common ( sanitizeChar, sanitizeUserText' )


startMultilineEditing :: MH ()
startMultilineEditing = do
    mh invalidateCache
    csEditState.cedEphemeral.eesMultiline .= True

toggleMultilineEditing :: MH ()
toggleMultilineEditing = do
    mh invalidateCache
    csEditState.cedEphemeral.eesMultiline %= not

    -- If multiline is now disabled and there is more than one line in
    -- the editor, that means we're showing the multiline message status
    -- (see Draw.Main.renderUserCommandBox.commandBox) so we want to be
    -- sure no autocomplete UI is present in case the cursor was left on
    -- a word that would otherwise show completion alternatives.
    multiline <- use (csEditState.cedEphemeral.eesMultiline)
    numLines <- use (csEditState.cedEditor.to getEditContents.to length)
    when (not multiline && numLines > 1) resetAutocomplete

invokeExternalEditor :: MH ()
invokeExternalEditor = do
    -- If EDITOR is in the environment, write the current message to a
    -- temp file, invoke EDITOR on it, read the result, remove the temp
    -- file, and update the program state.
    --
    -- If EDITOR is not present, fall back to 'vi'.
    mEnv <- liftIO $ Sys.lookupEnv "EDITOR"
    let editorProgram = maybe "vi" id mEnv

    mhSuspendAndResume $ \ st -> do
      Sys.withSystemTempFile "matterhorn_editor.tmp" $ \tmpFileName tmpFileHandle -> do
        -- Write the current message to the temp file
        Sys.hPutStr tmpFileHandle $ T.unpack $ T.intercalate "\n" $
            getEditContents $ st^.csEditState.cedEditor
        Sys.hClose tmpFileHandle

        -- Run the editor
        status <- Sys.system (editorProgram <> " " <> tmpFileName)

        -- On editor exit, if exited with zero status, read temp file.
        -- If non-zero status, skip temp file read.
        case status of
            Sys.ExitSuccess -> do
                tmpBytes <- BS.readFile tmpFileName
                case T.decodeUtf8' tmpBytes of
                    Left _ -> do
                        postErrorMessageIO "Failed to decode file contents as UTF-8" st
                    Right t -> do
                        let tmpLines = T.lines $ sanitizeUserText' t
                        return $ st & csEditState.cedEditor.editContentsL .~ (Z.textZipper tmpLines Nothing)
            Sys.ExitFailure _ -> return st

handlePaste :: BS.ByteString -> MH ()
handlePaste bytes = do
  let pasteStr = T.pack (UTF8.toString bytes)
  csEditState.cedEditor %= applyEdit (Z.insertMany (sanitizeUserText' pasteStr))
  contents <- use (csEditState.cedEditor.to getEditContents)
  case length contents > 1 of
      True -> startMultilineEditing
      False -> return ()

editingPermitted :: ChatState -> Bool
editingPermitted st =
    (length (getEditContents $ st^.csEditState.cedEditor) == 1) ||
    st^.csEditState.cedEphemeral.eesMultiline

editingKeybindings :: [Keybinding]
editingKeybindings =
  let kb desc ev mote = KB desc ev mote Nothing in
  map withUserTypingAction
  [ kb "Transpose the final two characters"
    (EvKey (KChar 't') [MCtrl]) $ do
    csEditState.cedEditor %= applyEdit Z.transposeChars
  , kb "Go to the start of the current line"
    (EvKey (KChar 'a') [MCtrl]) $ do
    csEditState.cedEditor %= applyEdit Z.gotoBOL
  , kb "Go to the end of the current line"
    (EvKey (KChar 'e') [MCtrl]) $ do
    csEditState.cedEditor %= applyEdit Z.gotoEOL
  , kb "Delete the character at the cursor"
    (EvKey (KChar 'd') [MCtrl]) $ do
    csEditState.cedEditor %= applyEdit Z.deleteChar
  , kb "Delete from the cursor to the start of the current line"
    (EvKey (KChar 'u') [MCtrl]) $ do
    csEditState.cedEditor %= applyEdit Z.killToBOL
  , kb "Move one character to the right"
    (EvKey (KChar 'f') [MCtrl]) $ do
    csEditState.cedEditor %= applyEdit Z.moveRight
  , kb "Move one character to the left"
    (EvKey (KChar 'b') [MCtrl]) $ do
    csEditState.cedEditor %= applyEdit Z.moveLeft
  , kb "Move one word to the right"
    (EvKey (KChar 'f') [MMeta]) $ do
    csEditState.cedEditor %= applyEdit Z.moveWordRight
  , kb "Move one word to the left"
    (EvKey (KChar 'b') [MMeta]) $ do
    csEditState.cedEditor %= applyEdit Z.moveWordLeft
  , kb "Delete the word to the left of the cursor"
    (EvKey KBS [MMeta]) $ do
    csEditState.cedEditor %= applyEdit Z.deletePrevWord
  , kb "Delete the word to the left of the cursor"
    (EvKey (KChar 'w') [MCtrl]) $ do
    csEditState.cedEditor %= applyEdit Z.deletePrevWord
  , kb "Delete the word to the right of the cursor"
    (EvKey (KChar 'd') [MMeta]) $ do
    csEditState.cedEditor %= applyEdit Z.deleteWord
  , kb "Move the cursor to the beginning of the input"
    (EvKey KHome []) $ do
    csEditState.cedEditor %= applyEdit gotoHome
  , kb "Move the cursor to the end of the input"
    (EvKey KEnd []) $ do
    csEditState.cedEditor %= applyEdit gotoEnd
  , kb "Kill the line to the right of the current position and copy it"
    (EvKey (KChar 'k') [MCtrl]) $ do
      z <- use (csEditState.cedEditor.editContentsL)
      let restOfLine = Z.currentLine (Z.killToBOL z)
      csEditState.cedYankBuffer .= restOfLine
      csEditState.cedEditor %= applyEdit Z.killToEOL
  , kb "Paste the current buffer contents at the cursor"
    (EvKey (KChar 'y') [MCtrl]) $ do
      buf <- use (csEditState.cedYankBuffer)
      csEditState.cedEditor %= applyEdit (Z.insertMany buf)
  ]
  where
    withUserTypingAction (KB {..}) =
      KB kbDescription kbEvent
         (kbAction >> sendUserTypingAction)
         kbBindingInfo

getEditorContent :: MH Text
getEditorContent = do
    cmdLine <- use (csEditState.cedEditor)
    let (line:rest) = getEditContents cmdLine
    return $ T.intercalate "\n" $ line : rest

-- | Handle an input submission in the message editor.
--
-- This handles the specified input text as if it were user input for
-- the specified channel. This means that if the specified input text
-- contains a command ("/...") then it is executed as normal. Otherwise
-- the text is sent as a message to the specified channel.
--
-- However, this function assumes that the message editor is the
-- *source* of the text, so it also takes care of clearing the editor,
-- resetting the edit mode, updating the input history for the specified
-- channel, etc.
handleInputSubmission :: ChannelId -> Text -> MH ()
handleInputSubmission cId content = do
    -- We clean up before dispatching the command or sending the message
    -- since otherwise the command could change the state and then doing
    -- cleanup afterwards could clean up the wrong things.
    csEditState.cedEditor %= applyEdit Z.clearZipper
    csEditState.cedInputHistory %= addHistoryEntry content cId
    csEditState.cedEphemeral.eesInputHistoryPosition .= Nothing

    case T.uncons content of
      Just ('/', cmd) ->
          dispatchCommand cmd
      _ -> do
          attachments <- use (csEditState.cedAttachmentList.L.listElementsL)
          mode <- use (csEditState.cedEditMode)
          sendMessage cId mode content $ F.toList attachments

    -- Reset the autocomplete UI
    resetAutocomplete

    -- Empty the attachment list
    resetAttachmentList

    -- Reset the edit mode *after* handling the input so that the input
    -- handler can tell whether we're editing, replying, etc.
    csEditState.cedEditMode .= NewPost

handleEditingInput :: Event -> MH ()
handleEditingInput e = do
    -- Only handle input events to the editor if we permit editing:
    -- if multiline mode is off, or if there is only one line of text
    -- in the editor. This means we skip input this catch-all handler
    -- if we're *not* in multiline mode *and* there are multiple lines,
    -- i.e., we are showing the user the status message about the
    -- current editor state and editing is not permitted.

    smartBacktick <- use (csResources.crConfiguration.to configSmartBacktick)
    let smartChars = "*`_"
    st <- use id
    csEditState.cedEphemeral.eesInputHistoryPosition .= Nothing

    case lookupKeybinding e editingKeybindings of
      Just kb | editingPermitted st -> kbAction kb
      _ -> do
        case e of
          -- Not editing; backspace here means cancel multi-line message
          -- composition
          EvKey KBS [] | (not $ editingPermitted st) ->
            csEditState.cedEditor %= applyEdit Z.clearZipper

          -- Backspace in editing mode with smart pair insertion means
          -- smart pair removal when possible
          EvKey KBS [] | editingPermitted st && smartBacktick ->
              let backspace = csEditState.cedEditor %= applyEdit Z.deletePrevChar
              in case cursorAtOneOf smartChars (st^.csEditState.cedEditor) of
                  Nothing -> backspace
                  Just ch ->
                      -- Smart char removal:
                      if | (cursorAtChar ch $ applyEdit Z.moveLeft $ st^.csEditState.cedEditor) &&
                           (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.csEditState.cedEditor) ->
                             csEditState.cedEditor %= applyEdit (Z.deleteChar >>> Z.deletePrevChar)
                         | otherwise -> backspace

          EvKey (KChar ch) []
            | editingPermitted st && smartBacktick && ch `elem` smartChars ->
              -- Smart char insertion:
              let doInsertChar = do
                    csEditState.cedEditor %= applyEdit (Z.insertChar ch)
                    sendUserTypingAction
                  curLine = Z.currentLine $ st^.csEditState.cedEditor.editContentsL
              -- First case: if the cursor is at the end of the current
              -- line and it contains "``" and the user entered a third
              -- "`", enable multi-line mode since they're likely typing
              -- a code block.
              in if | (cursorIsAtEnd $ st^.csEditState.cedEditor) &&
                         curLine == "``" &&
                         ch == '`' -> do
                        csEditState.cedEditor %= applyEdit (Z.insertMany (T.singleton ch))
                        csEditState.cedEphemeral.eesMultiline .= True
                    -- Second case: user entered some smart character
                    -- (don't care which) on an empty line or at the end
                    -- of the line after whitespace, so enter a pair of
                    -- the smart chars and put the cursor between them.
                    | (editorEmpty $ st^.csEditState.cedEditor) ||
                         ((cursorAtChar ' ' (applyEdit Z.moveLeft $ st^.csEditState.cedEditor)) &&
                          (cursorIsAtEnd $ st^.csEditState.cedEditor)) ->
                        csEditState.cedEditor %= applyEdit (Z.insertMany (T.pack $ ch:ch:[]) >>> Z.moveLeft)
                    -- Third case: the cursor is already on a smart
                    -- character and that character is the last one
                    -- on the line, so instead of inserting a new
                    -- character, just move past it.
                    | (cursorAtChar ch $ st^.csEditState.cedEditor) &&
                      (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.csEditState.cedEditor) ->
                        csEditState.cedEditor %= applyEdit Z.moveRight
                    -- Fall-through case: just insert one of the chars
                    -- without doing anything smart.
                    | otherwise -> doInsertChar
            | editingPermitted st -> do
              csEditState.cedEditor %= applyEdit (Z.insertMany (sanitizeChar ch))
              sendUserTypingAction
          _ | editingPermitted st -> do
              mhHandleEventLensed (csEditState.cedEditor) handleEditorEvent e
              sendUserTypingAction
            | otherwise -> return ()

    checkForAutocompletion False
    liftIO $ resetSpellCheckTimer $ st^.csEditState

-- | Send the user_typing action to the server asynchronously, over the
-- connected websocket. If the websocket is not connected, drop the
-- action silently.
sendUserTypingAction :: MH ()
sendUserTypingAction = do
  st <- use id
  when (configShowTypingIndicator (st^.csResources.crConfiguration)) $
    case st^.csConnectionStatus of
      Connected -> do
        let pId = case st^.csEditState.cedEditMode of
                    Replying _ post -> Just $ postId post
                    _               -> Nothing
        liftIO $ do
          now <- getCurrentTime
          let action = UserTyping now (st^.csCurrentChannelId) pId
          STM.atomically $ STM.writeTChan (st^.csResources.crWebsocketActionChan) action
      Disconnected -> return ()

-- Kick off an async request to the spell checker for the current editor
-- contents.
requestSpellCheck :: MH ()
requestSpellCheck = do
    st <- use id
    case st^.csEditState.cedSpellChecker of
        Nothing -> return ()
        Just (checker, _) -> do
            -- Get the editor contents.
            contents <- getEditContents <$> use (csEditState.cedEditor)
            doAsyncWith Preempt $ do
                -- For each line in the editor, submit an aspell request.
                let query = concat <$> mapM (askAspell checker) contents
                    postMistakes :: [AspellResponse] -> MH ()
                    postMistakes responses = do
                        let getMistakes AllCorrect = []
                            getMistakes (Mistakes ms) = mistakeWord <$> ms
                            allMistakes = S.fromList $ concat $ getMistakes <$> responses
                        csEditState.cedMisspellings .= allMistakes

                tryMM query (return . Just . postMistakes)

editorEmpty :: Editor Text a -> Bool
editorEmpty e = cursorIsAtEnd e &&
                cursorIsAtBeginning e

cursorIsAtEnd :: Editor Text a -> Bool
cursorIsAtEnd e =
    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z
        z = e^.editContentsL
    in col == T.length curLine

cursorIsAtBeginning :: Editor Text a -> Bool
cursorIsAtBeginning e =
    let col = snd $ Z.cursorPosition z
        z = e^.editContentsL
    in col == 0

cursorAtOneOf :: [Char] -> Editor Text a -> Maybe Char
cursorAtOneOf [] _ = Nothing
cursorAtOneOf (c:cs) e =
    if cursorAtChar c e
    then Just c
    else cursorAtOneOf cs e

cursorAtChar :: Char -> Editor Text a -> Bool
cursorAtChar ch e =
    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z
        z = e^.editContentsL
    in (T.singleton ch) `T.isPrefixOf` T.drop col curLine

gotoHome :: Z.TextZipper Text -> Z.TextZipper Text
gotoHome = Z.moveCursor (0, 0)

gotoEnd :: Z.TextZipper Text -> Z.TextZipper Text
gotoEnd z =
    let zLines = Z.getText z
        numLines = length zLines
        lastLineLength = T.length $ last zLines
    in if numLines > 0
       then Z.moveCursor (numLines - 1, lastLineLength) z
       else z

cancelAutocompleteOrReplyOrEdit :: MH ()
cancelAutocompleteOrReplyOrEdit = do
    cId <- use csCurrentChannelId
    mh $ invalidateCacheEntry $ ChannelMessages cId
    ac <- use (csEditState.cedAutocomplete)
    case ac of
        Just _ -> do
            resetAutocomplete
        Nothing -> do
            mode <- use (csEditState.cedEditMode)
            case mode of
                NewPost -> return ()
                _ -> do
                    csEditState.cedEditMode .= NewPost
                    csEditState.cedEditor %= applyEdit Z.clearZipper
                    resetAttachmentList

replyToLatestMessage :: MH ()
replyToLatestMessage = do
  msgs <- use (csCurrentChannel . ccContents . cdMessages)
  case findLatestUserMessage isReplyable msgs of
    Just msg | isReplyable msg ->
        do let Just p = msg^.mOriginalPost
           setMode Main
           cId <- use csCurrentChannelId
           mh $ invalidateCacheEntry $ ChannelMessages cId
           csEditState.cedEditMode .= Replying msg p
    _ -> return ()
