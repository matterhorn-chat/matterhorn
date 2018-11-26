{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module State.Editing
  ( requestSpellCheck
  , editingKeybindings
  , toggleMultilineEditing
  , invokeExternalEditor
  , handlePaste
  , handleEditingInput
  , cancelAutocompleteOrReplyOrEdit
  , replyToLatestMessage
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( viewportScroll, vScrollToBeginning, invalidateCache )
import           Brick.Widgets.Edit ( Editor, applyEdit , handleEditorEvent
                                    , getEditContents, editContentsL )
import qualified Brick.Widgets.List as L
import qualified Codec.Binary.UTF8.Generic as UTF8
import           Control.Arrow
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as BS
import           Data.Char ( isSpace )
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import           Data.List ( sort, sortBy )
import           Data.Ord ( comparing )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import           Data.Time ( getCurrentTime )
import qualified Data.Vector as V
import           Graphics.Vty ( Event(..), Key(..), Modifier(..) )
import           Lens.Micro.Platform ( (%=), (.=), (.~), to, _Just, preuse )
import qualified Skylighting.Types as Sky
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.IO.Temp as Sys
import qualified System.Process as Sys
import           Text.Aspell ( AspellResponse(..), mistakeWord, askAspell )

import           Network.Mattermost.Types (Post(..), userId)
import qualified Network.Mattermost.Endpoints as MM

import           Command ( commandList, printArgSpec )
import           Config
import           Events.Keybindings
import           State.Common
import           Types hiding ( newState )
import           Types.Common ( sanitizeChar, sanitizeUserText' )


startMultilineEditing :: MH ()
startMultilineEditing = do
    mh invalidateCache
    csEditState.cedMultiline .= True

toggleMultilineEditing :: MH ()
toggleMultilineEditing = do
    mh invalidateCache
    csEditState.cedMultiline %= not

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
    st^.csEditState.cedMultiline

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
              in if | (editorEmpty $ st^.csEditState.cedEditor) ||
                         ((cursorAtChar ' ' (applyEdit Z.moveLeft $ st^.csEditState.cedEditor)) &&
                          (cursorIsAtEnd $ st^.csEditState.cedEditor)) ->
                        csEditState.cedEditor %= applyEdit (Z.insertMany (T.pack $ ch:ch:[]) >>> Z.moveLeft)
                    | (cursorAtChar ch $ st^.csEditState.cedEditor) &&
                      (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.csEditState.cedEditor) ->
                        csEditState.cedEditor %= applyEdit Z.moveRight
                    | otherwise -> doInsertChar
            | editingPermitted st -> do
              csEditState.cedEditor %= applyEdit (Z.insertMany (sanitizeChar ch))
              sendUserTypingAction
          _ | editingPermitted st -> do
              mhHandleEventLensed (csEditState.cedEditor) handleEditorEvent e
              sendUserTypingAction
            | otherwise -> return ()

    checkForAutocompletion
    liftIO $ resetSpellCheckTimer $ st^.csEditState

checkForAutocompletion :: MH ()
checkForAutocompletion = do
    result <- getCompleterForInput
    case result of
        Nothing -> resetAutocomplete
        Just (runUpdater, searchString) -> do
            prevResult <- use (csEditState.cedAutocomplete)
            let shouldUpdate = maybe True ((/= searchString) . _acPreviousSearchString)
                               prevResult
            when shouldUpdate $ do
                csEditState.cedAutocompletePending .= Just searchString
                runUpdater searchString

getCompleterForInput :: MH (Maybe (Text -> MH (), Text))
getCompleterForInput = do
    z <- use (csEditState.cedEditor.editContentsL)

    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z

    return $ case wordAtColumn col curLine of
        Just w | userSigil `T.isPrefixOf` w ->
                   Just (doUserAutoCompletion, T.tail w)
               | normalChannelSigil `T.isPrefixOf` w ->
                   Just (doChannelAutoCompletion, T.tail w)
               | "```" `T.isPrefixOf` w ->
                   Just (doSyntaxAutoCompletion, T.drop 3 w)
               | "/" `T.isPrefixOf` w ->
                   Just (doCommandAutoCompletion, T.tail w)
        _ -> Nothing

doSyntaxAutoCompletion :: Text -> MH ()
doSyntaxAutoCompletion searchString = do
    mapping <- use (csResources.crSyntaxMap)
    let allNames = Sky.sShortname <$> M.elems mapping
        match = (((T.toLower searchString) `T.isInfixOf`) . T.toLower)
        alts = SyntaxCompletion <$> (sort $ filter match allNames)
    setCompletionAlternatives searchString alts "Languages"

doCommandAutoCompletion :: Text -> MH ()
doCommandAutoCompletion searchString = do
    let alts = mkAlt <$> sortBy (comparing cmdName) (filter matches commandList)
        lowerSearch = T.toLower searchString
        matches c = lowerSearch `T.isInfixOf` (cmdName c) ||
                    lowerSearch `T.isInfixOf` (T.toLower $ cmdDescr c)
        mkAlt (Cmd name desc args _) =
            CommandCompletion name (printArgSpec args) desc
    setCompletionAlternatives searchString alts "Commands"

withCachedAutocompleteResults :: Text -> Text -> MH () -> MH ()
withCachedAutocompleteResults label searchString act = do
    mCache <- preuse (csEditState.cedAutocomplete._Just.acCachedResponses)

    -- Does the cache have results for this search string? If so, use
    -- them; otherwise invoke the specified action.
    case HM.lookup searchString =<< mCache of
        Just alts -> setCompletionAlternatives searchString alts label
        Nothing -> act

doUserAutoCompletion :: Text -> MH ()
doUserAutoCompletion searchString = do
    session <- getSession
    myTid <- gets myTeamId
    myUid <- gets myUserId
    cId <- use csCurrentChannelId
    let label = "Users"

    withCachedAutocompleteResults label searchString $
        doAsyncWith Preempt $ do
            ac <- MM.mmAutocompleteUsers (Just myTid) (Just cId) searchString session

            let active = Seq.filter (\u -> userId u /= myUid && (not $ userDeleted u))
                alts = F.toList $
                       ((\u -> UserCompletion u True) <$> (active $ MM.userAutocompleteUsers ac)) <>
                       (maybe mempty (fmap (\u -> UserCompletion u False) . active) $
                              MM.userAutocompleteOutOfChannel ac)

            return $ Just $ setCompletionAlternatives searchString alts label

doChannelAutoCompletion :: Text -> MH ()
doChannelAutoCompletion searchString = do
    session <- getSession
    tId <- gets myTeamId
    let label = "Channels"

    withCachedAutocompleteResults label searchString $ do
        doAsyncWith Preempt $ do
            results <- MM.mmAutocompleteChannels tId searchString session
            let alts = F.toList $ ChannelCompletion <$> results
            return $ Just $ setCompletionAlternatives searchString alts label

setCompletionAlternatives :: Text -> [AutocompleteAlternative] -> Text -> MH ()
setCompletionAlternatives searchString alts ty = do
    let list = L.list CompletionList (V.fromList $ F.toList alts) 1
        state = AutocompleteState { _acPreviousSearchString = searchString
                                  , _acCompletionList =
                                      list & L.listSelectedL .~ Nothing
                                  , _acListElementType = ty
                                  , _acCachedResponses = HM.fromList [(searchString, alts)]
                                  }

    pending <- use (csEditState.cedAutocompletePending)
    case pending of
        Just val | val == searchString -> do

            -- If there is already state, update it, but also cache the
            -- search results.
            csEditState.cedAutocomplete %= \prev ->
                let newState = case prev of
                        Nothing ->
                            state
                        Just oldState ->
                            state & acCachedResponses .~
                                HM.insert searchString alts (oldState^.acCachedResponses)
                in Just newState

            mh $ vScrollToBeginning $ viewportScroll CompletionList
        _ ->
            -- Do not update the state if this result does not
            -- correspond to the search string we used most recently.
            -- This happens when the editor changes faster than the
            -- async completion responses arrive from the server. If we
            -- don't check this, we show completion results that are
            -- wrong for the editor state.
            return ()

wordAtColumn :: Int -> Text -> Maybe Text
wordAtColumn i t =
    let tokens = T.groupBy (\a b -> isSpace a == isSpace b) t
        go j _ | j < 0 = Nothing
        go j ts = case ts of
            [] -> Nothing
            (w:rest) | j <= T.length w && not (isSpace $ T.head w) -> Just w
                     | otherwise -> go (j - T.length w) rest
    in go i tokens

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
    ac <- use (csEditState.cedAutocomplete)
    case ac of
        Just _ -> do
            csEditState.cedAutocomplete .= Nothing
        Nothing -> do
            mode <- use (csEditState.cedEditMode)
            case mode of
                NewPost -> return ()
                _ -> do
                    csEditState.cedEditMode .= NewPost
                    csEditState.cedEditor %= applyEdit Z.clearZipper

replyToLatestMessage :: MH ()
replyToLatestMessage = do
  msgs <- use (csCurrentChannel . ccContents . cdMessages)
  case findLatestUserMessage isReplyable msgs of
    Just msg | isReplyable msg ->
        do let Just p = msg^.mOriginalPost
           setMode Main
           csEditState.cedEditMode .= Replying msg p
    _ -> return ()
