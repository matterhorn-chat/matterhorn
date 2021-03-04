{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.Editing
  ( requestSpellCheck
  , editingKeybindings
  , editingKeyHandlers
  , messageEditingKeybindings
  , toggleMultilineEditing
  , invokeExternalEditor
  , handlePaste
  , handleInputSubmission
  , getEditorContent
  , handleEditingInput
  , cancelAutocompleteOrReplyOrEdit
  , replyToLatestMessage
  , Direction(..)
  , tabComplete
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( invalidateCache, invalidateCacheEntry )
import           Brick.Widgets.Edit ( Editor, applyEdit , handleEditorEvent
                                    , getEditContents, editContentsL )
import qualified Brick.Widgets.List as L
import qualified Codec.Binary.UTF8.Generic as UTF8
import           Control.Arrow
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import           Data.Char ( isSpace )
import qualified Data.Foldable as F
import qualified Data.Map as M
import           Data.Maybe ( fromJust )
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import           Data.Time ( getCurrentTime )
import           Graphics.Vty ( Event(..), Key(..) )
import           Lens.Micro.Platform ( Lens', (%=), (.=), (.~), to, _Just )
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.IO.Temp as Sys
import qualified System.Process as Sys
import           Text.Aspell ( AspellResponse(..), mistakeWord, askAspell )

import           Network.Mattermost.Types ( Post(..), ChannelId, TeamId )

import           Matterhorn.Config
import {-# SOURCE #-} Matterhorn.Command ( dispatchCommand )
import           Matterhorn.InputHistory
import           Matterhorn.Events.Keybindings
import           Matterhorn.State.Common
import           Matterhorn.State.Autocomplete
import           Matterhorn.State.Attachments
import           Matterhorn.State.Messages
import           Matterhorn.Types hiding ( newState )
import           Matterhorn.Types.Common ( sanitizeUserText' )


startMultilineEditing :: MH ()
startMultilineEditing = do
    mh invalidateCache
    csCurrentTeam.tsEditState.cedEphemeral.eesMultiline .= True

toggleMultilineEditing :: MH ()
toggleMultilineEditing = do
    mh invalidateCache
    csCurrentTeam.tsEditState.cedEphemeral.eesMultiline %= not

    -- If multiline is now disabled and there is more than one line in
    -- the editor, that means we're showing the multiline message status
    -- (see Draw.Main.renderUserCommandBox.commandBox) so we want to be
    -- sure no autocomplete UI is present in case the cursor was left on
    -- a word that would otherwise show completion alternatives.
    multiline <- use (csCurrentTeam.tsEditState.cedEphemeral.eesMultiline)
    numLines <- use (csCurrentTeam.tsEditState.cedEditor.to getEditContents.to length)
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
      Sys.withSystemTempFile "matterhorn_editor.md" $ \tmpFileName tmpFileHandle -> do
        -- Write the current message to the temp file
        Sys.hPutStr tmpFileHandle $ T.unpack $ T.intercalate "\n" $
            getEditContents $ st^.csCurrentTeam.tsEditState.cedEditor
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
                        return $ st & csCurrentTeam.tsEditState.cedEditor.editContentsL .~ (Z.textZipper tmpLines Nothing)
            Sys.ExitFailure _ -> return st

handlePaste :: BS.ByteString -> MH ()
handlePaste bytes = do
  let pasteStr = T.pack (UTF8.toString bytes)
  csCurrentTeam.tsEditState.cedEditor %= applyEdit (Z.insertMany (sanitizeUserText' pasteStr))
  contents <- use (csCurrentTeam.tsEditState.cedEditor.to getEditContents)
  case length contents > 1 of
      True -> startMultilineEditing
      False -> return ()

editingPermitted :: ChatState -> Bool
editingPermitted st =
    (length (getEditContents $ st^.csCurrentTeam.tsEditState.cedEditor) == 1) ||
    st^.csCurrentTeam.tsEditState.cedEphemeral.eesMultiline

messageEditingKeybindings :: KeyConfig -> KeyHandlerMap
messageEditingKeybindings kc =
    let KeyHandlerMap m = editingKeybindings (csCurrentTeam.tsEditState.cedEditor) kc
    in KeyHandlerMap $ M.map withUserTypingAction m

withUserTypingAction :: KeyHandler -> KeyHandler
withUserTypingAction kh =
    kh { khHandler = newH }
    where
        oldH = khHandler kh
        newH = oldH { kehHandler = newKEH }
        oldKEH = kehHandler oldH
        newKEH = oldKEH { ehAction = ehAction oldKEH >> sendUserTypingAction }

editingKeybindings :: Lens' ChatState (Editor T.Text Name) -> KeyConfig -> KeyHandlerMap
editingKeybindings editor = mkKeybindings $ editingKeyHandlers editor

editingKeyHandlers :: Lens' ChatState (Editor T.Text Name) -> [KeyEventHandler]
editingKeyHandlers editor =
  [ mkKb EditorTransposeCharsEvent
    "Transpose the final two characters"
    (editor %= applyEdit Z.transposeChars)
  , mkKb EditorBolEvent
    "Go to the start of the current line"
    (editor %= applyEdit Z.gotoBOL)
  , mkKb EditorEolEvent
    "Go to the end of the current line"
    (editor %= applyEdit Z.gotoEOL)
  , mkKb EditorDeleteCharacter
    "Delete the character at the cursor"
    (editor %= applyEdit Z.deleteChar)
  , mkKb EditorKillToBolEvent
    "Delete from the cursor to the start of the current line"
    (editor %= applyEdit Z.killToBOL)
  , mkKb EditorKillToEolEvent
    "Kill the line to the right of the current position and copy it" $ do
      z <- use (editor.editContentsL)
      let restOfLine = Z.currentLine (Z.killToBOL z)
      csCurrentTeam.tsEditState.cedYankBuffer .= restOfLine
      editor %= applyEdit Z.killToEOL
  , mkKb EditorNextCharEvent
    "Move one character to the right"
    (editor %= applyEdit Z.moveRight)
  , mkKb EditorPrevCharEvent
    "Move one character to the left"
    (editor %= applyEdit Z.moveLeft)
  , mkKb EditorNextWordEvent
    "Move one word to the right"
    (editor %= applyEdit Z.moveWordRight)
  , mkKb EditorPrevWordEvent
    "Move one word to the left"
    (editor %= applyEdit Z.moveWordLeft)
  , mkKb EditorDeletePrevWordEvent
    "Delete the word to the left of the cursor" $ do
    editor %= applyEdit Z.deletePrevWord
  , mkKb EditorDeleteNextWordEvent
    "Delete the word to the right of the cursor" $ do
    editor %= applyEdit Z.deleteWord
  , mkKb EditorHomeEvent
    "Move the cursor to the beginning of the input" $ do
    editor %= applyEdit gotoHome
  , mkKb EditorEndEvent
    "Move the cursor to the end of the input" $ do
    editor %= applyEdit gotoEnd
  , mkKb EditorYankEvent
    "Paste the current buffer contents at the cursor" $ do
      buf <- use (csCurrentTeam.tsEditState.cedYankBuffer)
      editor %= applyEdit (Z.insertMany buf)
  ]

getEditorContent :: MH Text
getEditorContent = do
    cmdLine <- use (csCurrentTeam.tsEditState.cedEditor)
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
handleInputSubmission :: TeamId -> ChannelId -> Text -> MH ()
handleInputSubmission tId cId content = do
    -- We clean up before dispatching the command or sending the message
    -- since otherwise the command could change the state and then doing
    -- cleanup afterwards could clean up the wrong things.
    csTeam(tId).tsEditState.cedEditor %= applyEdit Z.clearZipper
    csTeam(tId).tsEditState.cedEphemeral.eesInputHistoryPosition .= Nothing

    csInputHistory %= addHistoryEntry content cId

    case T.uncons content of
      Just ('/', cmd) ->
          dispatchCommand cmd
      _ -> do
          attachments <- use (csTeam(tId).tsEditState.cedAttachmentList.L.listElementsL)
          mode <- use (csTeam(tId).tsEditState.cedEditMode)
          sendMessage cId mode content $ F.toList attachments

          -- Empty the attachment list only if a mesage is actually sent, since
          -- it's possible to /attach a file before actually sending the
          -- message
          resetAttachmentList

    -- Reset the autocomplete UI
    resetAutocomplete


    -- Reset the edit mode *after* handling the input so that the input
    -- handler can tell whether we're editing, replying, etc.
    csTeam(tId).tsEditState.cedEditMode .= NewPost

closingPunctuationMarks :: String
closingPunctuationMarks = ".,'\";:)]!?"

isSmartClosingPunctuation :: Event -> Bool
isSmartClosingPunctuation (EvKey (KChar c) []) = c `elem` closingPunctuationMarks
isSmartClosingPunctuation _ = False

handleEditingInput :: Event -> MH ()
handleEditingInput e = do
    -- Only handle input events to the editor if we permit editing:
    -- if multiline mode is off, or if there is only one line of text
    -- in the editor. This means we skip input this catch-all handler
    -- if we're *not* in multiline mode *and* there are multiple lines,
    -- i.e., we are showing the user the status message about the
    -- current editor state and editing is not permitted.

    -- Record the input line count before handling the editing event
    -- so we can tell whether the editing event changes the line count
    -- later.
    beforeLineCount <- use (csCurrentTeam.tsEditState.cedEditor.to getEditContents.to length)

    smartBacktick <- use (csResources.crConfiguration.configSmartBacktickL)
    let smartChars = "*`_"
    st <- use id
    csCurrentTeam.tsEditState.cedEphemeral.eesInputHistoryPosition .= Nothing

    smartEditing <- use (csResources.crConfiguration.configSmartEditingL)
    justCompleted <- use (csCurrentTeam.tsEditState.cedJustCompleted)

    conf <- use (csResources.crConfiguration)
    let keyMap = editingKeybindings (csCurrentTeam.tsEditState.cedEditor) (configUserKeys conf)
    case lookupKeybinding e keyMap of
      Just kb | editingPermitted st -> (ehAction $ kehHandler $ khHandler kb)
      _ -> do
        case e of
          -- Not editing; backspace here means cancel multi-line message
          -- composition
          EvKey KBS [] | (not $ editingPermitted st) ->
            csCurrentTeam.tsEditState.cedEditor %= applyEdit Z.clearZipper

          -- Backspace in editing mode with smart pair insertion means
          -- smart pair removal when possible
          EvKey KBS [] | editingPermitted st && smartBacktick ->
              let backspace = csCurrentTeam.tsEditState.cedEditor %= applyEdit Z.deletePrevChar
              in case cursorAtOneOf smartChars (st^.csCurrentTeam.tsEditState.cedEditor) of
                  Nothing -> backspace
                  Just ch ->
                      -- Smart char removal:
                      if | (cursorAtChar ch $ applyEdit Z.moveLeft $ st^.csCurrentTeam.tsEditState.cedEditor) &&
                           (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.csCurrentTeam.tsEditState.cedEditor) ->
                             csCurrentTeam.tsEditState.cedEditor %= applyEdit (Z.deleteChar >>> Z.deletePrevChar)
                         | otherwise -> backspace

          EvKey (KChar ch) []
            | editingPermitted st && smartBacktick && ch `elem` smartChars ->
              -- Smart char insertion:
              let doInsertChar = do
                    csCurrentTeam.tsEditState.cedEditor %= applyEdit (Z.insertChar ch)
                    sendUserTypingAction
                  curLine = Z.currentLine $ st^.csCurrentTeam.tsEditState.cedEditor.editContentsL
              -- First case: if the cursor is at the end of the current
              -- line and it contains "``" and the user entered a third
              -- "`", enable multi-line mode since they're likely typing
              -- a code block.
              in if | (cursorIsAtEnd $ st^.csCurrentTeam.tsEditState.cedEditor) &&
                         curLine == "``" &&
                         ch == '`' -> do
                        csCurrentTeam.tsEditState.cedEditor %= applyEdit (Z.insertMany (T.singleton ch))
                        csCurrentTeam.tsEditState.cedEphemeral.eesMultiline .= True
                    -- Second case: user entered some smart character
                    -- (don't care which) on an empty line or at the end
                    -- of the line after whitespace, so enter a pair of
                    -- the smart chars and put the cursor between them.
                    | (editorEmpty $ st^.csCurrentTeam.tsEditState.cedEditor) ||
                         ((cursorAtChar ' ' (applyEdit Z.moveLeft $ st^.csCurrentTeam.tsEditState.cedEditor)) &&
                          (cursorIsAtEnd $ st^.csCurrentTeam.tsEditState.cedEditor)) ->
                        csCurrentTeam.tsEditState.cedEditor %= applyEdit (Z.insertMany (T.pack $ ch:ch:[]) >>> Z.moveLeft)
                    -- Third case: the cursor is already on a smart
                    -- character and that character is the last one
                    -- on the line, so instead of inserting a new
                    -- character, just move past it.
                    | (cursorAtChar ch $ st^.csCurrentTeam.tsEditState.cedEditor) &&
                      (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.csCurrentTeam.tsEditState.cedEditor) ->
                        csCurrentTeam.tsEditState.cedEditor %= applyEdit Z.moveRight
                    -- Fall-through case: just insert one of the chars
                    -- without doing anything smart.
                    | otherwise -> doInsertChar
            | editingPermitted st -> do

              -- If the most recent editing event was a tab completion,
              -- there is a trailing space that we want to remove if the
              -- next input character is punctuation.
              when (smartEditing && justCompleted && isSmartClosingPunctuation e) $
                  csCurrentTeam.tsEditState.cedEditor %= applyEdit Z.deletePrevChar

              csCurrentTeam.tsEditState.cedEditor %= applyEdit (Z.insertMany (sanitizeUserText' $ T.singleton ch))
              sendUserTypingAction
          _ | editingPermitted st -> do
              mhHandleEventLensed (csCurrentTeam.tsEditState.cedEditor) handleEditorEvent e
              sendUserTypingAction
            | otherwise -> return ()

    let ctx = AutocompleteContext { autocompleteManual = False
                                  , autocompleteFirstMatch = False
                                  }
    checkForAutocompletion ctx
    liftIO $ resetSpellCheckTimer $ st^.csCurrentTeam.tsEditState

    -- If the preview is enabled and multi-line editing is enabled and
    -- the line count changed, we need to invalidate the rendering cache
    -- entry for the channel messages because we want to redraw them to
    -- fit in the space just changed by the size of the preview area.
    afterLineCount <- use (csCurrentTeam.tsEditState.cedEditor.to getEditContents.to length)
    isMultiline <- use (csCurrentTeam.tsEditState.cedEphemeral.eesMultiline)
    isPreviewing <- use (csResources.crConfiguration.configShowMessagePreviewL)
    when (beforeLineCount /= afterLineCount && isMultiline && isPreviewing) $ do
        tId <- use csCurrentTeamId
        cId <- use (csCurrentChannelId tId)
        mh $ invalidateCacheEntry $ ChannelMessages cId

    -- Reset the recent autocompletion flag to stop smart punctuation
    -- handling.
    when justCompleted $
        csCurrentTeam.tsEditState.cedJustCompleted .= False

-- | Send the user_typing action to the server asynchronously, over the
-- connected websocket. If the websocket is not connected, drop the
-- action silently.
sendUserTypingAction :: MH ()
sendUserTypingAction = do
  st <- use id
  when (configShowTypingIndicator (st^.csResources.crConfiguration)) $
    case st^.csConnectionStatus of
      Connected -> do
        let pId = case st^.csCurrentTeam.tsEditState.cedEditMode of
                    Replying _ post -> Just $ postId post
                    _               -> Nothing
        tId <- use csCurrentTeamId
        liftIO $ do
          now <- getCurrentTime
          let action = UserTyping now (st^.csCurrentChannelId(tId)) pId
          STM.atomically $ STM.writeTChan (st^.csResources.crWebsocketActionChan) action
      Disconnected -> return ()

-- Kick off an async request to the spell checker for the current editor
-- contents.
requestSpellCheck :: MH ()
requestSpellCheck = do
    st <- use id
    case st^.csCurrentTeam.tsEditState.cedSpellChecker of
        Nothing -> return ()
        Just (checker, _) -> do
            -- Get the editor contents.
            tId <- use csCurrentTeamId
            contents <- getEditContents <$> use (csTeam(tId).tsEditState.cedEditor)
            doAsyncWith Preempt $ do
                -- For each line in the editor, submit an aspell request.
                let query = concat <$> mapM (askAspell checker) contents
                    postMistakes :: [AspellResponse] -> MH ()
                    postMistakes responses = do
                        let getMistakes AllCorrect = []
                            getMistakes (Mistakes ms) = mistakeWord <$> ms
                            allMistakes = S.fromList $ concat $ getMistakes <$> responses
                        csTeam(tId).tsEditState.cedMisspellings .= allMistakes

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
    tId <- use csCurrentTeamId
    cId <- use (csCurrentChannelId tId)
    mh $ invalidateCacheEntry $ ChannelMessages cId
    ac <- use (csCurrentTeam.tsEditState.cedAutocomplete)
    case ac of
        Just _ -> do
            resetAutocomplete
        Nothing -> do
            mode <- use (csCurrentTeam.tsEditState.cedEditMode)
            case mode of
                NewPost -> return ()
                _ -> do
                    csCurrentTeam.tsEditState.cedEditMode .= NewPost
                    csCurrentTeam.tsEditState.cedEditor %= applyEdit Z.clearZipper
                    resetAttachmentList

replyToLatestMessage :: MH ()
replyToLatestMessage = do
  msgs <- use (csCurrentChannel . ccContents . cdMessages)
  case findLatestUserMessage isReplyable msgs of
    Just msg | isReplyable msg ->
        do rootMsg <- getReplyRootMessage msg
           setMode Main
           tId <- use csCurrentTeamId
           cId <- use (csCurrentChannelId tId)
           mh $ invalidateCacheEntry $ ChannelMessages cId
           csCurrentTeam.tsEditState.cedEditMode .= Replying rootMsg (fromJust $ rootMsg^.mOriginalPost)
    _ -> return ()

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
    csCurrentTeam.tsEditState.cedAutocomplete._Just.acCompletionList %= transform

    mac <- use (csCurrentTeam.tsEditState.cedAutocomplete)
    case mac of
        Nothing -> do
            let ctx = AutocompleteContext { autocompleteManual = True
                                          , autocompleteFirstMatch = True
                                          }
            checkForAutocompletion ctx
        Just ac -> do
            case ac^.acCompletionList.to L.listSelectedElement of
                Nothing -> return ()
                Just (_, alternative) -> do
                    let replacement = autocompleteAlternativeReplacement alternative
                        maybeEndOfWord z =
                            if maybe True isSpace (Z.currentChar z)
                            then z
                            else Z.moveWordRight z
                    csCurrentTeam.tsEditState.cedEditor %=
                        applyEdit (Z.insertChar ' ' . Z.insertMany replacement . Z.deletePrevWord .
                                   maybeEndOfWord)
                    csCurrentTeam.tsEditState.cedJustCompleted .= True

                    -- If there was only one completion alternative,
                    -- hide the autocomplete listing now that we've
                    -- completed the only completion.
                    when (ac^.acCompletionList.to L.listElements.to length == 1) resetAutocomplete
