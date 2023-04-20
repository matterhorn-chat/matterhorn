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

import           Brick ( BrickEvent(VtyEvent) )
import           Brick.Keybindings
import           Brick.Main ( invalidateCache )
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
import           Lens.Micro.Platform ( Traversal', Lens', (%=), (.=), (.~), to, _Just )
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.IO.Temp as Sys
import qualified System.Process as Sys
import           Text.Aspell ( Aspell, AspellResponse(..), mistakeWord, askAspell )

import           Network.Mattermost.Types ( Post(..) )

import           Matterhorn.Config
import {-# SOURCE #-} Matterhorn.Command ( dispatchCommand )
import           Matterhorn.InputHistory
import           Matterhorn.State.Common
import           Matterhorn.State.Autocomplete
import {-# SOURCE #-} Matterhorn.State.Messages
import {-# SOURCE #-} Matterhorn.State.ThreadWindow
import           Matterhorn.Types hiding ( newState )
import           Matterhorn.Types.Common ( sanitizeUserText' )


startMultilineEditing :: Lens' ChatState (EditState Name) -> MH ()
startMultilineEditing which = do
    mh invalidateCache
    which.esEphemeral.eesMultiline .= True

toggleMultilineEditing :: Lens' ChatState (EditState Name) -> MH ()
toggleMultilineEditing which = do
    mh invalidateCache
    which.esEphemeral.eesMultiline %= not

    -- If multiline is now disabled and there is more than one line in
    -- the editor, that means we're showing the multiline message status
    -- (see Draw.Main.renderUserCommandBox.commandBox) so we want to be
    -- sure no autocomplete UI is present in case the cursor was left on
    -- a word that would otherwise show completion alternatives.
    multiline <- use (which.esEphemeral.eesMultiline)
    numLines <- use (which.esEditor.to getEditContents.to length)
    when (not multiline && numLines > 1) (resetAutocomplete which)

invokeExternalEditor :: Lens' ChatState (EditState Name) -> MH ()
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
            getEditContents $ st^.which.esEditor
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
                        return $ st & which.esEditor.editContentsL .~ (Z.textZipper tmpLines Nothing)
            Sys.ExitFailure _ -> return st

handlePaste :: Lens' ChatState (EditState Name) -> BS.ByteString -> MH ()
handlePaste which bytes = do
  let pasteStr = T.pack (UTF8.toString bytes)
  which.esEditor %= applyEdit (Z.insertMany (sanitizeUserText' pasteStr))
  contents <- use (which.esEditor.to getEditContents)
  case length contents > 1 of
      True -> startMultilineEditing which
      False -> return ()

editingPermitted :: ChatState -> Lens' ChatState (EditState Name) -> Bool
editingPermitted st which =
    (length (getEditContents $ st^.which.esEditor) == 1) ||
    st^.which.esEphemeral.eesMultiline

editingKeybindings :: Lens' ChatState (Editor T.Text Name) -> KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH
editingKeybindings editor kc = unsafeKeyDispatcher kc $ editingKeyHandlers editor

editingKeyHandlers :: Lens' ChatState (Editor T.Text Name) -> [MHKeyEventHandler]
editingKeyHandlers editor =
  [ onEvent EditorTransposeCharsEvent
    "Transpose the final two characters"
    (editor %= applyEdit Z.transposeChars)
  , onEvent EditorBolEvent
    "Go to the start of the current line"
    (editor %= applyEdit Z.gotoBOL)
  , onEvent EditorEolEvent
    "Go to the end of the current line"
    (editor %= applyEdit Z.gotoEOL)
  , onEvent EditorDeleteCharacter
    "Delete the character at the cursor"
    (editor %= applyEdit Z.deleteChar)
  , onEvent EditorKillToBolEvent
    "Delete from the cursor to the start of the current line"
    (editor %= applyEdit Z.killToBOL)
  , onEvent EditorNextCharEvent
    "Move one character to the right"
    (editor %= applyEdit Z.moveRight)
  , onEvent EditorPrevCharEvent
    "Move one character to the left"
    (editor %= applyEdit Z.moveLeft)
  , onEvent EditorNextWordEvent
    "Move one word to the right"
    (editor %= applyEdit Z.moveWordRight)
  , onEvent EditorPrevWordEvent
    "Move one word to the left"
    (editor %= applyEdit Z.moveWordLeft)
  , onEvent EditorDeletePrevWordEvent
    "Delete the word to the left of the cursor" $ do
    editor %= applyEdit Z.deletePrevWord
  , onEvent EditorDeleteNextWordEvent
    "Delete the word to the right of the cursor" $ do
    editor %= applyEdit Z.deleteWord
  , onEvent EditorHomeEvent
    "Move the cursor to the beginning of the input" $ do
    editor %= applyEdit gotoHome
  , onEvent EditorEndEvent
    "Move the cursor to the end of the input" $ do
    editor %= applyEdit gotoEnd
  , onEvent EditorKillToEolEvent
    "Kill the line to the right of the current position and copy it" $ do
      z <- use (editor.editContentsL)
      let restOfLine = Z.currentLine (Z.killToBOL z)
      csGlobalEditState.gedYankBuffer .= restOfLine
      editor %= applyEdit Z.killToEOL
  , onEvent EditorYankEvent
    "Paste the current buffer contents at the cursor" $ do
        buf <- use (csGlobalEditState.gedYankBuffer)
        editor %= applyEdit (Z.insertMany buf)
  ]

getEditorContent :: Lens' ChatState (EditState Name) -> MH Text
getEditorContent which = do
    cmdLine <- use (which.esEditor)
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
handleInputSubmission :: Lens' ChatState (EditState Name)
                      -> Text
                      -> MH ()
handleInputSubmission editWhich content = do
    cId <- use (editWhich.esChannelId)

    -- We clean up before dispatching the command or sending the message
    -- since otherwise the command could change the state and then doing
    -- cleanup afterwards could clean up the wrong things.
    editWhich.esEditor %= applyEdit Z.clearZipper
    editWhich.esEphemeral.eesInputHistoryPosition .= Nothing

    csInputHistory %= addHistoryEntry content cId

    case T.uncons content of
      Just ('/', cmd) -> do
          tId <- do
              mTid <- use (editWhich.esTeamId)
              curTid <- fromJust <$> use csCurrentTeamId
              return $ fromMaybe curTid mTid
          dispatchCommand tId cmd
      _ -> do
          attachments <- use (editWhich.esAttachmentList.L.listElementsL)
          mode <- use (editWhich.esEditMode)
          sendMessage cId mode content $ F.toList attachments

          -- Empty the attachment list only if a mesage is
          -- actually sent, since it's possible to /attach a
          -- file before actually sending the message
          resetAttachmentList editWhich

    -- Reset the autocomplete UI
    resetAutocomplete editWhich

    -- Reset the edit mode *after* handling the input so that the input
    -- handler can tell whether we're editing, replying, etc.
    resetEditMode <- use (editWhich.esResetEditMode)
    editWhich.esEditMode .= resetEditMode

closingPunctuationMarks :: String
closingPunctuationMarks = ".,'\";:)]!?"

isSmartClosingPunctuation :: Event -> Bool
isSmartClosingPunctuation (EvKey (KChar c) []) = c `elem` closingPunctuationMarks
isSmartClosingPunctuation _ = False

handleEditingInput :: Lens' ChatState (EditState Name)
                   -> Event
                   -> MH ()
handleEditingInput which e = do
    cId <- use (which.esChannelId)

    -- Only handle input events to the editor if we permit editing:
    -- if multiline mode is off, or if there is only one line of text
    -- in the editor. This means we skip input this catch-all handler
    -- if we're *not* in multiline mode *and* there are multiple lines,
    -- i.e., we are showing the user the status message about the
    -- current editor state and editing is not permitted.

    -- Record the input line count before handling the editing event
    -- so we can tell whether the editing event changes the line count
    -- later.
    beforeLineCount <- use (which.esEditor.to getEditContents.to length)

    smartBacktick <- use (csResources.crConfiguration.configSmartBacktickL)
    let smartChars = "*`_"
    st <- use id
    which.esEphemeral.eesInputHistoryPosition .= Nothing

    smartEditing <- use (csResources.crConfiguration.configSmartEditingL)
    justCompleted <- use (which.esJustCompleted)

    conf <- use (csResources.crConfiguration)
    let keyMap = editingKeybindings (which.esEditor) (configUserKeys conf)
    case e of
        EvKey k mods -> do
            case lookupVtyEvent k mods keyMap of
              Just kb | editingPermitted st which -> (handlerAction $ kehHandler $ khHandler kb)
              _ -> do
                case (k, mods) of
                  -- Not editing; backspace here means cancel multi-line message
                  -- composition
                  (KBS, []) | (not $ editingPermitted st which) -> do
                    which.esEditor %= applyEdit Z.clearZipper
                    mh invalidateCache

                  -- Backspace in editing mode with smart pair insertion means
                  -- smart pair removal when possible
                  (KBS, []) | editingPermitted st which && smartBacktick ->
                      let backspace = which.esEditor %= applyEdit Z.deletePrevChar
                      in case cursorAtOneOf smartChars (st^.which.esEditor) of
                          Nothing -> backspace
                          Just ch ->
                              -- Smart char removal:
                              if | (cursorAtChar ch $ applyEdit Z.moveLeft $ st^.which.esEditor) &&
                                   (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.which.esEditor) ->
                                     which.esEditor %= applyEdit (Z.deleteChar >>> Z.deletePrevChar)
                                 | otherwise -> backspace

                  (KChar ch, [])
                    | editingPermitted st which && smartBacktick && ch `elem` smartChars ->
                      -- Smart char insertion:
                      let doInsertChar = do
                            which.esEditor %= applyEdit (Z.insertChar ch)
                            sendUserTypingAction which
                          curLine = Z.currentLine $ st^.which.esEditor.editContentsL
                      -- First case: if the cursor is at the end of the current
                      -- line and it contains "``" and the user entered a third
                      -- "`", enable multi-line mode since they're likely typing
                      -- a code block.
                      in if | (cursorIsAtEnd $ st^.which.esEditor) &&
                                 curLine == "``" &&
                                 ch == '`' -> do
                                which.esEditor %= applyEdit (Z.insertMany (T.singleton ch))
                                which.esEphemeral.eesMultiline .= True
                            -- Second case: user entered some smart character
                            -- (don't care which) on an empty line or at the end
                            -- of the line after whitespace, so enter a pair of
                            -- the smart chars and put the cursor between them.
                            | (editorEmpty $ st^.which.esEditor) ||
                                 ((cursorAtChar ' ' (applyEdit Z.moveLeft $ st^.which.esEditor)) &&
                                  (cursorIsAtEnd $ st^.which.esEditor)) ->
                                which.esEditor %= applyEdit (Z.insertMany (T.pack $ ch:ch:[]) >>> Z.moveLeft)
                            -- Third case: the cursor is already on a smart
                            -- character and that character is the last one
                            -- on the line, so instead of inserting a new
                            -- character, just move past it.
                            | (cursorAtChar ch $ st^.which.esEditor) &&
                              (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.which.esEditor) ->
                                which.esEditor %= applyEdit Z.moveRight
                            -- Fall-through case: just insert one of the chars
                            -- without doing anything smart.
                            | otherwise -> doInsertChar
                    | editingPermitted st which -> do

                      -- If the most recent editing event was a tab completion,
                      -- there is a trailing space that we want to remove if the
                      -- next input character is punctuation.
                      when (smartEditing && justCompleted && isSmartClosingPunctuation e) $
                          which.esEditor %= applyEdit Z.deletePrevChar

                      which.esEditor %= applyEdit (Z.insertMany (sanitizeUserText' $ T.singleton ch))
                      sendUserTypingAction which
                  _ | editingPermitted st which -> do
                      mhZoom (which.esEditor) handleEditorEvent (VtyEvent e)
                      sendUserTypingAction which
                    | otherwise -> return ()
        _ -> return ()

    let ctx = AutocompleteContext { autocompleteManual = False
                                  , autocompleteFirstMatch = False
                                  }
    checkForAutocompletion which ctx

    -- Reset the spell check timer for this editor
    mReset <- use (which.esSpellCheckTimerReset)
    case mReset of
        Nothing -> return ()
        Just reset -> liftIO reset

    -- If the preview is enabled and multi-line editing is enabled and
    -- the line count changed, we need to invalidate the rendering cache
    -- entry for the channel messages because we want to redraw them to
    -- fit in the space just changed by the size of the preview area.
    afterLineCount <- use (which.esEditor.to getEditContents.to length)
    isMultiline <- use (which.esEphemeral.eesMultiline)
    isPreviewing <- use (csResources.crConfiguration.configShowMessagePreviewL)
    when (beforeLineCount /= afterLineCount && isMultiline && isPreviewing) $ do
        invalidateChannelRenderingCache cId

    -- Reset the recent autocompletion flag to stop smart punctuation
    -- handling.
    when justCompleted $
        which.esJustCompleted .= False

-- | Send the user_typing action to the server asynchronously, over the
-- connected websocket. If the websocket is not connected, drop the
-- action silently.
sendUserTypingAction :: Lens' ChatState (EditState Name)
                     -> MH ()
sendUserTypingAction which = do
    st <- use id
    when (configSendTypingNotifications (st^.csResources.crConfiguration)) $
      case st^.csConnectionStatus of
        Connected -> do
          let pId = case st^.which.esEditMode of
                      Replying _ post -> Just $ postId post
                      _               -> Nothing
          cId <- use (which.esChannelId)
          liftIO $ do
            now <- getCurrentTime
            let action = UserTyping now cId pId
            STM.atomically $ STM.writeTChan (st^.csResources.crWebsocketActionChan) action
        Disconnected -> return ()

-- Kick off an async request to the spell checker for the current editor
-- contents.
requestSpellCheck :: Aspell -> MessageInterfaceTarget -> MH ()
requestSpellCheck checker target = do
    -- Get the editor contents.
    mContents <- case target of
        MITeamThread tId -> do
            mTi <- use (maybeThreadInterface(tId))
            case mTi of
                Nothing -> return Nothing
                Just ti -> return $ Just $ getEditContents $ ti^.miEditor.esEditor
        MIChannel cId -> do
            mMi <- preuse (maybeChannelMessageInterface(cId))
            case mMi of
                Nothing -> return Nothing
                Just mi -> return $ Just $ getEditContents $ mi^.miEditor.esEditor

    case mContents of
        Nothing -> return ()
        Just contents ->
            doAsyncWith Preempt $ do
                -- For each line in the editor, submit an aspell request.
                let query = concat <$> mapM (askAspell checker) contents
                    postMistakes :: [AspellResponse] -> MH ()
                    postMistakes responses = do
                        let getMistakes AllCorrect = []
                            getMistakes (Mistakes ms) = mistakeWord <$> ms
                            allMistakes = S.fromList $ concat $ getMistakes <$> responses

                        case target of
                            MITeamThread tId ->
                                maybeThreadInterface(tId)._Just.miEditor.esMisspellings .= allMistakes
                            MIChannel cId ->
                                maybeChannelMessageInterface(cId).miEditor.esMisspellings .= allMistakes

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

-- Cancels the following states in this order, as appropriate based on
-- context:
-- * Autocomplete UI display
-- * Reply
-- * Edit
-- * Close current team's thread window if open
cancelAutocompleteOrReplyOrEdit :: Lens' ChatState (EditState Name) -> MH ()
cancelAutocompleteOrReplyOrEdit which = do
    cId <- use (which.esChannelId)
    invalidateChannelRenderingCache cId
    ac <- use (which.esAutocomplete)
    case ac of
        Just _ -> do
            resetAutocomplete which
        Nothing -> do
            resetEditMode <- use (which.esResetEditMode)

            let resetEditor = do
                    which.esEditMode .= resetEditMode
                    which.esEditor %= applyEdit Z.clearZipper
                    resetAttachmentList which

            curEditMode <- use (which.esEditMode)
            case curEditMode of
                NewPost -> return ()
                Editing {} -> resetEditor
                Replying {} -> do
                    prevMode <- use (which.esEditMode)
                    resetEditor
                    newMode <- use (which.esEditMode)
                    when (newMode == prevMode) $
                        withCurrentTeam $ \tId -> do
                            ti <- use (csTeam(tId).tsThreadInterface)
                            foc <- use (csTeam(tId).tsMessageInterfaceFocus)
                            when (isJust ti && foc == FocusThread) $
                                closeThreadWindow tId

replyToLatestMessage :: Lens' ChatState (MessageInterface n i) -> MH ()
replyToLatestMessage which = do
    msgs <- use (which.miMessages)
    cId <- use (which.miChannelId)
    case findLatestUserMessage isReplyable msgs of
      Just msg | isReplyable msg ->
          do rootMsg <- getReplyRootMessage msg
             invalidateChannelRenderingCache cId
             which.miEditor.esEditMode .= Replying rootMsg (fromJust $ rootMsg^.mOriginalPost)
      _ -> return ()

data Direction = Forwards | Backwards

tabComplete :: Traversal' ChatState (EditState Name) -> Direction -> MH ()
tabComplete which dir = do
    searchStr <- use (which.esAutocomplete._Just.acPreviousSearchString)

    let transform list =
            let len = list^.L.listElementsL.to length
                prefixMatch alt =
                    T.toLower searchStr `T.isPrefixOf` (T.toLower $ autocompleteAlternativeText alt)
            in case dir of
                Forwards ->
                    if L.listSelected list == Just (len - 1)
                       then L.listMoveTo 0 list
                       else if L.listSelected list == Nothing && len > 0
                            -- If the list has nothing selected, then
                            -- make the initial selection the best match
                            -- by prefix rather than always selecting
                            -- the first entry. If there is no match
                            -- based on prefix, then select the first
                            -- entry.
                            --
                            -- Note that we only bother with this
                            -- behavior in the Forward case because in
                            -- the Backwards case we don't want the
                            -- first selection to be based on prefix
                            -- match since that doesn't make sense.
                            then let new = L.listFindBy prefixMatch list
                                 in if L.listSelected new == Nothing
                                    -- If, after attempting to select
                                    -- by prefix, nothing matched, then
                                    -- there is still no autocomplete
                                    -- alternative selected, so move to
                                    -- the selection to the first entry
                                    -- in the list.
                                    then L.listMoveBy 1 new
                                    else new
                    else L.listMoveBy 1 list
                Backwards ->
                    if (L.listSelected list == Just 0) ||
                       (L.listSelected list == Nothing && len > 0)
                    then L.listMoveTo (len - 1) list
                    else L.listMoveBy (-1) list

    which.esAutocomplete._Just.acCompletionList %= transform

    mac <- join <$> preuse (which.esAutocomplete)
    case mac of
        Nothing -> do
            let ctx = AutocompleteContext { autocompleteManual = True
                                          , autocompleteFirstMatch = True
                                          }
            checkForAutocompletion which ctx
        Just ac -> do
            case ac^.acCompletionList.to L.listSelectedElement of
                Nothing -> return ()
                Just (_, alternative) -> do
                    let replacement = autocompleteAlternativeReplacement alternative
                        maybeEndOfWord z =
                            if maybe True isSpace (Z.currentChar z)
                            then z
                            else Z.moveWordRight z
                    which.esEditor %=
                        applyEdit (Z.insertChar ' ' . Z.insertMany replacement . Z.deletePrevWord .
                                   maybeEndOfWord)
                    which.esJustCompleted .= True

                    -- If there was only one completion alternative,
                    -- hide the autocomplete listing now that we've
                    -- completed the only completion.
                    when (ac^.acCompletionList.to L.listElements.to length == 1) (resetAutocomplete which)

resetAttachmentList :: Lens' ChatState (EditState Name) -> MH ()
resetAttachmentList which = do
    which.esAttachmentList %= L.listClear
