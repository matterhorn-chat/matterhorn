{-# LANGUAGE MultiWayIf #-}

module State.Editing where

import           Brick (EventM, Next, suspendAndResume, handleEventLensed)
import           Brick.Widgets.Edit (Editor, handleEditorEvent, getEditContents, editContentsL)
import           Brick.Widgets.Edit (applyEdit)
import qualified Codec.Binary.UTF8.Generic as UTF8
import           Control.Arrow
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text.Zipper (textZipper, insertMany,
                                   moveRight, moveLeft, cursorPosition, currentLine,
                                   transposeChars, deleteChar, deletePrevChar,
                                   insertChar, clearZipper)
import           Graphics.Vty (Event(..), Key(..), Modifier(..))
import           Lens.Micro.Platform
import           System.Environment (lookupEnv)
import           System.Exit (ExitCode(..))
import           System.IO (hPutStr, hClose)
import           System.IO.Temp (withSystemTempFile)
import           System.Process (system)

import           Network.Mattermost
import           Network.Mattermost.Lenses

import           Config
import           Types

import           State.Common

startMultilineEditing :: ChatState -> ChatState
startMultilineEditing = csEditState.cedMultiline .~ True

stopMultilineEditing :: ChatState -> ChatState
stopMultilineEditing = csEditState.cedMultiline .~ False

invokeExternalEditor :: ChatState -> EventM Name (Next ChatState)
invokeExternalEditor st = do
    -- If EDITOR is in the environment, write the current message to a
    -- temp file, invoke EDITOR on it, read the result, remove the temp
    -- file, and update the program state.
    --
    -- If EDITOR is not present, fall back to 'vi'.
    mEnv <- liftIO $ lookupEnv "EDITOR"
    let editorProgram = maybe "vi" id mEnv

    suspendAndResume $
      withSystemTempFile "matterhorn_editor.tmp" $ \tmpFileName tmpFileHandle -> do
        -- Write the current message to the temp file
        hPutStr tmpFileHandle $ T.unpack $ T.intercalate "\n" $ getEditContents $ st^.cmdLine
        hClose tmpFileHandle

        -- Run the editor
        status <- system (editorProgram <> " " <> tmpFileName)

        -- On editor exit, if exited with zero status, read temp file.
        -- If non-zero status, skip temp file read.
        case status of
            ExitSuccess -> do
                tmpLines <- T.lines <$> T.pack <$> readFile tmpFileName
                return $ st & cmdLine.editContentsL .~ (textZipper tmpLines Nothing)
                            & csEditState.cedMultiline .~ (length tmpLines > 1)
            ExitFailure _ -> return st

toggleMessagePreview :: ChatState -> ChatState
toggleMessagePreview = csShowMessagePreview %~ not

addUserToCurrentChannel :: T.Text -> ChatState -> EventM Name ChatState
addUserToCurrentChannel uname st = do
    -- First: is this a valid username?
    let results = filter ((== uname) . _uiName . snd) $ HM.toList $ st^.usrMap
    case results of
        [(uid, _)] -> do
            liftIO $ doAsyncWith st $ do
                let cId = st^.csCurrentChannelId
                tryMM (void $ mmChannelAddUser (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId uid)
                      (const $ return return)
            return st
        _ -> do
            postErrorMessage ("No such user: " <> uname) st

handlePaste :: BS.ByteString -> ChatState -> ChatState
handlePaste bytes st = do
  let pasteStr = T.pack (UTF8.toString bytes)
      st' = st & cmdLine %~ applyEdit (insertMany pasteStr)
  case length (getEditContents $ st'^.cmdLine) > 1 of
      True -> startMultilineEditing st'
      False -> st'

editingPermitted :: ChatState -> Bool
editingPermitted st =
    (length (getEditContents $ st^.cmdLine) == 1) ||
    st^.csEditState.cedMultiline

handleEditingInput :: Event -> ChatState -> EventM Name ChatState
handleEditingInput e st = do
    -- Only handle input events to the editor if we permit editing:
    -- if multiline mode is off, or if there is only one line of text
    -- in the editor. This means we skip input this catch-all handler
    -- if we're *not* in multiline mode *and* there are multiple lines,
    -- i.e., we are showing the user the status message about the
    -- current editor state and editing is not permitted.

    let smartBacktick = st^.csResources.crConfiguration.to configSmartBacktick
        smartChars = "*`_"
    st' <- case e of
        EvKey (KChar 't') [MCtrl] | editingPermitted st ->
            return $ st & cmdLine %~ applyEdit transposeChars

        -- Not editing; backspace here means cancel multi-line message
        -- composition
        EvKey KBS [] | (not $ editingPermitted st) ->
            return $ st & cmdLine %~ applyEdit clearZipper

        -- Backspace in editing mode with smart pair insertion means
        -- smart pair removal when possible
        EvKey KBS [] | editingPermitted st && smartBacktick ->
            let backspace = return $ st & cmdLine %~ applyEdit deletePrevChar
            in case cursorAtOneOf smartChars (st^.cmdLine) of
                Nothing -> backspace
                Just ch ->
                    -- Smart char removal:
                    if | (cursorAtChar ch $ applyEdit moveLeft $ st^.cmdLine) &&
                         (cursorIsAtEnd $ applyEdit moveRight $ st^.cmdLine) ->
                           return $ st & cmdLine %~ applyEdit (deleteChar >>> deletePrevChar)
                       | otherwise -> backspace

        EvKey (KChar ch) [] | editingPermitted st && smartBacktick && ch `elem` smartChars ->
            -- Smart char insertion:
            let doInsertChar = return $ st & cmdLine %~ applyEdit (insertChar ch)
            in if | (editorEmpty $ st^.cmdLine) ||
                       ((cursorAtChar ' ' (applyEdit moveLeft $ st^.cmdLine)) &&
                        (cursorIsAtEnd $ st^.cmdLine)) ->
                      return $ st & cmdLine %~ applyEdit (insertMany (T.pack $ ch:ch:[]) >>> moveLeft)
                  | (cursorAtChar ch $ st^.cmdLine) &&
                    (cursorIsAtEnd $ applyEdit moveRight $ st^.cmdLine) ->
                      return $ st & cmdLine %~ applyEdit moveRight
                  | otherwise -> doInsertChar

        EvKey (KChar 'f') [MCtrl] | editingPermitted st ->
            return $ st & cmdLine %~ applyEdit moveRight

        EvKey (KChar 'b') [MCtrl] | editingPermitted st ->
            return $ st & cmdLine %~ applyEdit moveLeft

        _ | editingPermitted st -> handleEventLensed st cmdLine handleEditorEvent e
          | otherwise -> return st

    return $ st' & csCurrentCompletion .~ Nothing

editorEmpty :: Editor T.Text a -> Bool
editorEmpty e = cursorIsAtEnd e &&
                cursorIsAtBeginning e

cursorIsAtEnd :: Editor T.Text a -> Bool
cursorIsAtEnd e =
    let col = snd $ cursorPosition z
        curLine = currentLine z
        z = e^.editContentsL
    in col == T.length curLine

cursorIsAtBeginning :: Editor T.Text a -> Bool
cursorIsAtBeginning e =
    let col = snd $ cursorPosition z
        z = e^.editContentsL
    in col == 0

cursorAtOneOf :: [Char] -> Editor T.Text a -> Maybe Char
cursorAtOneOf [] _ = Nothing
cursorAtOneOf (c:cs) e =
    if cursorAtChar c e
    then Just c
    else cursorAtOneOf cs e

cursorAtChar :: Char -> Editor T.Text a -> Bool
cursorAtChar ch e =
    let col = snd $ cursorPosition z
        curLine = currentLine z
        z = e^.editContentsL
    in (T.singleton ch) `T.isPrefixOf` T.drop col curLine
