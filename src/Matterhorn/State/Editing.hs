{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.Editing
  ( requestSpellCheck
  , editingKeybindings
  , editingKeyHandlers
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


startMultilineEditing :: Lens' ChatState EditState -> MH ()
startMultilineEditing which = do
    mh invalidateCache
    which.cedEphemeral.eesMultiline .= True

toggleMultilineEditing :: Lens' ChatState EditState -> MH ()
toggleMultilineEditing which = do
    mh invalidateCache
    which.cedEphemeral.eesMultiline %= not

    -- If multiline is now disabled and there is more than one line in
    -- the editor, that means we're showing the multiline message status
    -- (see Draw.Main.renderUserCommandBox.commandBox) so we want to be
    -- sure no autocomplete UI is present in case the cursor was left on
    -- a word that would otherwise show completion alternatives.
    multiline <- use (which.cedEphemeral.eesMultiline)
    numLines <- use (which.cedEditor.to getEditContents.to length)
    when (not multiline && numLines > 1) (resetAutocomplete which)

invokeExternalEditor :: Lens' ChatState EditState -> MH ()
invokeExternalEditor which = do
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
            getEditContents $ st^.which.cedEditor
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
                        return $ st & which.cedEditor.editContentsL .~ (Z.textZipper tmpLines Nothing)
            Sys.ExitFailure _ -> return st

handlePaste :: Lens' ChatState EditState -> BS.ByteString -> MH ()
handlePaste which bytes = do
  let pasteStr = T.pack (UTF8.toString bytes)
  which.cedEditor %= applyEdit (Z.insertMany (sanitizeUserText' pasteStr))
  contents <- use (which.cedEditor.to getEditContents)
  case length contents > 1 of
      True -> startMultilineEditing which
      False -> return ()

editingPermitted :: ChatState -> Lens' ChatState EditState -> Bool
editingPermitted st which =
    (length (getEditContents $ st^.which.cedEditor) == 1) ||
    st^.which.cedEphemeral.eesMultiline

editingKeybindings :: TeamId -> Lens' ChatState (Editor T.Text Name) -> KeyConfig -> KeyHandlerMap
editingKeybindings tId editor = mkKeybindings $ editingKeyHandlers tId editor

editingKeyHandlers :: TeamId -> Lens' ChatState (Editor T.Text Name) -> [KeyEventHandler]
editingKeyHandlers tId editor =
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
      csTeam(tId).tsGlobalEditState.gedYankBuffer .= restOfLine
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
      buf <- use (csTeam(tId).tsGlobalEditState.gedYankBuffer)
      editor %= applyEdit (Z.insertMany buf)
  ]

getEditorContent :: Lens' ChatState EditState -> MH Text
getEditorContent which = do
    cmdLine <- use (which.cedEditor)
    let (line, rest) = case getEditContents cmdLine of
            (a:as) -> (a, as)
            _ -> error "BUG: getEditorContent: got empty edit contents"
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
handleInputSubmission :: TeamId -> Lens' ChatState EditState -> ChannelId -> Text -> MH ()
handleInputSubmission tId which cId content = do
    -- We clean up before dispatching the command or sending the message
    -- since otherwise the command could change the state and then doing
    -- cleanup afterwards could clean up the wrong things.
    which.cedEditor %= applyEdit Z.clearZipper
    which.cedEphemeral.eesInputHistoryPosition .= Nothing

    csInputHistory %= addHistoryEntry content cId

    case T.uncons content of
      Just ('/', cmd) ->
          dispatchCommand tId cmd
      _ -> do
          attachments <- use (which.cedAttachmentList.L.listElementsL)
          mode <- use (which.cedEditMode)
          sendMessage cId mode content $ F.toList attachments

          -- Empty the attachment list only if a mesage is actually sent, since
          -- it's possible to /attach a file before actually sending the
          -- message
          resetAttachmentList tId which

    -- Reset the autocomplete UI
    resetAutocomplete which

    -- Reset the edit mode *after* handling the input so that the input
    -- handler can tell whether we're editing, replying, etc.
    which.cedEditMode .= NewPost

closingPunctuationMarks :: String
closingPunctuationMarks = ".,'\";:)]!?"

isSmartClosingPunctuation :: Event -> Bool
isSmartClosingPunctuation (EvKey (KChar c) []) = c `elem` closingPunctuationMarks
isSmartClosingPunctuation _ = False

handleEditingInput :: TeamId -> Lens' ChatState EditState -> Event -> MH ()
handleEditingInput tId which e = do
    -- Only handle input events to the editor if we permit editing:
    -- if multiline mode is off, or if there is only one line of text
    -- in the editor. This means we skip input this catch-all handler
    -- if we're *not* in multiline mode *and* there are multiple lines,
    -- i.e., we are showing the user the status message about the
    -- current editor state and editing is not permitted.

    -- Record the input line count before handling the editing event
    -- so we can tell whether the editing event changes the line count
    -- later.
    beforeLineCount <- use (which.cedEditor.to getEditContents.to length)

    smartBacktick <- use (csResources.crConfiguration.configSmartBacktickL)
    let smartChars = "*`_"
    st <- use id
    which.cedEphemeral.eesInputHistoryPosition .= Nothing

    smartEditing <- use (csResources.crConfiguration.configSmartEditingL)
    justCompleted <- use (which.cedJustCompleted)

    conf <- use (csResources.crConfiguration)
    let keyMap = editingKeybindings tId (which.cedEditor) (configUserKeys conf)
    case lookupKeybinding e keyMap of
      Just kb | editingPermitted st which -> (ehAction $ kehHandler $ khHandler kb)
      _ -> do
        case e of
          -- Not editing; backspace here means cancel multi-line message
          -- composition
          EvKey KBS [] | (not $ editingPermitted st which) -> do
            which.cedEditor %= applyEdit Z.clearZipper
            mh invalidateCache

          -- Backspace in editing mode with smart pair insertion means
          -- smart pair removal when possible
          EvKey KBS [] | editingPermitted st which && smartBacktick ->
              let backspace = which.cedEditor %= applyEdit Z.deletePrevChar
              in case cursorAtOneOf smartChars (st^.which.cedEditor) of
                  Nothing -> backspace
                  Just ch ->
                      -- Smart char removal:
                      if | (cursorAtChar ch $ applyEdit Z.moveLeft $ st^.which.cedEditor) &&
                           (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.which.cedEditor) ->
                             which.cedEditor %= applyEdit (Z.deleteChar >>> Z.deletePrevChar)
                         | otherwise -> backspace

          EvKey (KChar ch) []
            | editingPermitted st which && smartBacktick && ch `elem` smartChars ->
              -- Smart char insertion:
              let doInsertChar = do
                    which.cedEditor %= applyEdit (Z.insertChar ch)
                    sendUserTypingAction tId which
                  curLine = Z.currentLine $ st^.which.cedEditor.editContentsL
              -- First case: if the cursor is at the end of the current
              -- line and it contains "``" and the user entered a third
              -- "`", enable multi-line mode since they're likely typing
              -- a code block.
              in if | (cursorIsAtEnd $ st^.which.cedEditor) &&
                         curLine == "``" &&
                         ch == '`' -> do
                        which.cedEditor %= applyEdit (Z.insertMany (T.singleton ch))
                        which.cedEphemeral.eesMultiline .= True
                    -- Second case: user entered some smart character
                    -- (don't care which) on an empty line or at the end
                    -- of the line after whitespace, so enter a pair of
                    -- the smart chars and put the cursor between them.
                    | (editorEmpty $ st^.which.cedEditor) ||
                         ((cursorAtChar ' ' (applyEdit Z.moveLeft $ st^.which.cedEditor)) &&
                          (cursorIsAtEnd $ st^.which.cedEditor)) ->
                        which.cedEditor %= applyEdit (Z.insertMany (T.pack $ ch:ch:[]) >>> Z.moveLeft)
                    -- Third case: the cursor is already on a smart
                    -- character and that character is the last one
                    -- on the line, so instead of inserting a new
                    -- character, just move past it.
                    | (cursorAtChar ch $ st^.which.cedEditor) &&
                      (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.which.cedEditor) ->
                        which.cedEditor %= applyEdit Z.moveRight
                    -- Fall-through case: just insert one of the chars
                    -- without doing anything smart.
                    | otherwise -> doInsertChar
            | editingPermitted st which -> do

              -- If the most recent editing event was a tab completion,
              -- there is a trailing space that we want to remove if the
              -- next input character is punctuation.
              when (smartEditing && justCompleted && isSmartClosingPunctuation e) $
                  which.cedEditor %= applyEdit Z.deletePrevChar

              which.cedEditor %= applyEdit (Z.insertMany (sanitizeUserText' $ T.singleton ch))
              sendUserTypingAction tId which
          _ | editingPermitted st which -> do
              mhHandleEventLensed (which.cedEditor) handleEditorEvent e
              sendUserTypingAction tId which
            | otherwise -> return ()

    let ctx = AutocompleteContext { autocompleteManual = False
                                  , autocompleteFirstMatch = False
                                  }
    checkForAutocompletion tId which ctx
    liftIO $ resetSpellCheckTimer $ st^.csTeam(tId).tsGlobalEditState

    -- If the preview is enabled and multi-line editing is enabled and
    -- the line count changed, we need to invalidate the rendering cache
    -- entry for the channel messages because we want to redraw them to
    -- fit in the space just changed by the size of the preview area.
    afterLineCount <- use (which.cedEditor.to getEditContents.to length)
    isMultiline <- use (which.cedEphemeral.eesMultiline)
    isPreviewing <- use (csResources.crConfiguration.configShowMessagePreviewL)
    when (beforeLineCount /= afterLineCount && isMultiline && isPreviewing) $ do
        withCurrentChannel tId $ \cId _ -> do
            mh $ invalidateCacheEntry $ ChannelMessages cId

    -- Reset the recent autocompletion flag to stop smart punctuation
    -- handling.
    when justCompleted $
        which.cedJustCompleted .= False

-- | Send the user_typing action to the server asynchronously, over the
-- connected websocket. If the websocket is not connected, drop the
-- action silently.
sendUserTypingAction :: TeamId -> Lens' ChatState EditState -> MH ()
sendUserTypingAction tId which = do
    withCurrentChannel tId $ \cId _ -> do
        st <- use id
        when (configSendTypingNotifications (st^.csResources.crConfiguration)) $
          case st^.csConnectionStatus of
            Connected -> do
              let pId = case st^.which.cedEditMode of
                          Replying _ post -> Just $ postId post
                          _               -> Nothing
              liftIO $ do
                now <- getCurrentTime
                let action = UserTyping now cId pId
                STM.atomically $ STM.writeTChan (st^.csResources.crWebsocketActionChan) action
            Disconnected -> return ()

-- Kick off an async request to the spell checker for the current editor
-- contents.
requestSpellCheck :: TeamId -> Lens' ChatState EditState -> MH ()
requestSpellCheck tId which = do
    st <- use id
    case st^.csTeam(tId).tsGlobalEditState.gedSpellChecker of
        Nothing -> return ()
        Just (checker, _) -> do
            -- Get the editor contents.
            contents <- getEditContents <$> use (which.cedEditor)
            doAsyncWith Preempt $ do
                -- For each line in the editor, submit an aspell request.
                let query = concat <$> mapM (askAspell checker) contents
                    postMistakes :: [AspellResponse] -> MH ()
                    postMistakes responses = do
                        let getMistakes AllCorrect = []
                            getMistakes (Mistakes ms) = mistakeWord <$> ms
                            allMistakes = S.fromList $ concat $ getMistakes <$> responses
                        which.cedMisspellings .= allMistakes

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

cancelAutocompleteOrReplyOrEdit :: TeamId -> Lens' ChatState EditState -> MH ()
cancelAutocompleteOrReplyOrEdit tId which = do
    withCurrentChannel tId $ \cId _ -> do
        mh $ invalidateCacheEntry $ ChannelMessages cId
        ac <- use (which.cedAutocomplete)
        case ac of
            Just _ -> do
                resetAutocomplete which
            Nothing -> do
                mode <- use (which.cedEditMode)
                case mode of
                    NewPost -> return ()
                    _ -> do
                        which.cedEditMode .= NewPost
                        which.cedEditor %= applyEdit Z.clearZipper
                        resetAttachmentList tId which

replyToLatestMessage :: TeamId -> Lens' ChatState EditState -> MH ()
replyToLatestMessage tId which = do
    withCurrentChannel tId $ \cId chan -> do
        let msgs = chan^. ccContents . cdMessages
        case findLatestUserMessage isReplyable msgs of
          Just msg | isReplyable msg ->
              do rootMsg <- getReplyRootMessage msg
                 mh $ invalidateCacheEntry $ ChannelMessages cId
                 which.cedEditMode .= Replying rootMsg (fromJust $ rootMsg^.mOriginalPost)
          _ -> return ()

data Direction = Forwards | Backwards

tabComplete :: TeamId -> Lens' ChatState EditState -> Direction -> MH ()
tabComplete tId which dir = do
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

    which.cedAutocomplete._Just.acCompletionList %= transform

    mac <- use (which.cedAutocomplete)
    case mac of
        Nothing -> do
            let ctx = AutocompleteContext { autocompleteManual = True
                                          , autocompleteFirstMatch = True
                                          }
            checkForAutocompletion tId which ctx
        Just ac -> do
            case ac^.acCompletionList.to L.listSelectedElement of
                Nothing -> return ()
                Just (_, alternative) -> do
                    let replacement = autocompleteAlternativeReplacement alternative
                        maybeEndOfWord z =
                            if maybe True isSpace (Z.currentChar z)
                            then z
                            else Z.moveWordRight z
                    which.cedEditor %=
                        applyEdit (Z.insertChar ' ' . Z.insertMany replacement . Z.deletePrevWord .
                                   maybeEndOfWord)
                    which.cedJustCompleted .= True

                    -- If there was only one completion alternative,
                    -- hide the autocomplete listing now that we've
                    -- completed the only completion.
                    when (ac^.acCompletionList.to L.listElements.to length == 1) (resetAutocomplete which)
