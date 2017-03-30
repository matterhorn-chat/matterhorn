{-# LANGUAGE MultiWayIf #-}

module State.Editing where

import           Prelude ()
import           Prelude.Compat

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
import qualified Data.Text.Encoding as T
import qualified Data.Text.Zipper as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import           Graphics.Vty (Event(..), Key(..), Modifier(..))
import           Lens.Micro.Platform
import qualified System.Environment as Sys
import qualified System.Exit as Sys
import qualified System.IO as Sys
import qualified System.IO.Temp as Sys
import qualified System.Process as Sys

import           Network.Mattermost
import           Network.Mattermost.Lenses

import           Config
import           Types

import           State.Common

startMultilineEditing :: MH ()
startMultilineEditing = csEditState.cedMultiline .= True

toggleMultilineEditing :: MH ()
toggleMultilineEditing = csEditState.cedMultiline %= not

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
            getEditContents $ st^.csCmdLine
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
                        let tmpLines = T.lines t
                        return $ st & csCmdLine.editContentsL .~ (Z.textZipper tmpLines Nothing)
                                    & csEditState.cedMultiline .~ (length tmpLines > 1)
            Sys.ExitFailure _ -> return st

toggleMessagePreview :: MH ()
toggleMessagePreview = csShowMessagePreview %= not

addUserToCurrentChannel :: T.Text -> MH ()
addUserToCurrentChannel uname = do
    -- First: is this a valid username?
    usrs <- use usrMap
    let results = filter ((== uname) . _uiName . snd) $ HM.toList usrs
    case results of
        [(uid, _)] -> do
            cId <- use csCurrentChannelId
            session <- use csSession
            myTeamId <- use (csMyTeam.teamIdL)
            doAsyncWith Normal $ do
                tryMM (void $ mmChannelAddUser session myTeamId cId uid)
                      (const $ return (return ()))
        _ -> do
            postErrorMessage ("No such user: " <> uname)

handlePaste :: BS.ByteString -> MH ()
handlePaste bytes = do
  let pasteStr = T.pack (UTF8.toString bytes)
  csCmdLine %= applyEdit (Z.insertMany pasteStr)
  contents <- use (csCmdLine.to getEditContents)
  case length contents > 1 of
      True -> startMultilineEditing
      False -> return ()

editingPermitted :: ChatState -> Bool
editingPermitted st =
    (length (getEditContents $ st^.csCmdLine) == 1) ||
    st^.csEditState.cedMultiline

editingKeybindings :: [Keybinding]
editingKeybindings =
  [ KB "Transpose the final two characters"
    (EvKey (KChar 't') [MCtrl]) $ do
    csCmdLine %= applyEdit Z.transposeChars
  , KB "Move one character to the right"
    (EvKey (KChar 'f') [MCtrl]) $ do
    csCmdLine %= applyEdit Z.moveRight
  , KB "Move one character to the left"
    (EvKey (KChar 'b') [MCtrl]) $ do
    csCmdLine %= applyEdit Z.moveLeft
  , KB "Move one word to the right"
    (EvKey (KChar 'f') [MMeta]) $ do
    csCmdLine %= applyEdit Z.moveWordRight
  , KB "Move one word to the left"
    (EvKey (KChar 'b') [MMeta]) $ do
    csCmdLine %= applyEdit Z.moveWordLeft
  , KB "Delete the word to the left of the cursor"
    (EvKey (KBS) [MMeta]) $ do
    csCmdLine %= applyEdit Z.deletePrevWord
  , KB "Delete the word to the left of the cursor"
    (EvKey (KChar 'w') [MCtrl]) $ do
    csCmdLine %= applyEdit Z.deletePrevWord
  , KB "Delete the word to the right of the cursor"
    (EvKey (KChar 'd') [MMeta]) $ do
    csCmdLine %= applyEdit Z.deleteWord
  , KB "Kill the line to the right of the current position and copy it"
    (EvKey (KChar 'k') [MCtrl]) $ do
      z <- use (csCmdLine.editContentsL)
      let restOfLine = Z.currentLine (Z.killToBOL z)
      csEditState.cedYankBuffer .= restOfLine
      csCmdLine %= applyEdit Z.killToEOL
  , KB "Paste the current buffer contents at the cursor"
    (EvKey (KChar 'y') [MCtrl]) $ do
      buf <- use (csEditState.cedYankBuffer)
      csCmdLine %= applyEdit (Z.insertMany buf)
  ]

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
            csCmdLine %= applyEdit Z.clearZipper

          -- Backspace in editing mode with smart pair insertion means
          -- smart pair removal when possible
          EvKey KBS [] | editingPermitted st && smartBacktick ->
              let backspace = csCmdLine %= applyEdit Z.deletePrevChar
              in case cursorAtOneOf smartChars (st^.csCmdLine) of
                  Nothing -> backspace
                  Just ch ->
                      -- Smart char removal:
                      if | (cursorAtChar ch $ applyEdit Z.moveLeft $ st^.csCmdLine) &&
                           (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.csCmdLine) ->
                             csCmdLine %= applyEdit (Z.deleteChar >>> Z.deletePrevChar)
                         | otherwise -> backspace

          EvKey (KChar ch) [] | editingPermitted st && smartBacktick && ch `elem` smartChars ->
              -- Smart char insertion:
              let doInsertChar = csCmdLine %= applyEdit (Z.insertChar ch)
              in if | (editorEmpty $ st^.csCmdLine) ||
                         ((cursorAtChar ' ' (applyEdit Z.moveLeft $ st^.csCmdLine)) &&
                          (cursorIsAtEnd $ st^.csCmdLine)) ->
                        csCmdLine %= applyEdit (Z.insertMany (T.pack $ ch:ch:[]) >>> Z.moveLeft)
                    | (cursorAtChar ch $ st^.csCmdLine) &&
                      (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.csCmdLine) ->
                        csCmdLine %= applyEdit Z.moveRight
                    | otherwise -> doInsertChar

          _ | editingPermitted st -> mhHandleEventLensed csCmdLine handleEditorEvent e
            | otherwise -> return ()

        csCurrentCompletion .= Nothing

editorEmpty :: Editor T.Text a -> Bool
editorEmpty e = cursorIsAtEnd e &&
                cursorIsAtBeginning e

cursorIsAtEnd :: Editor T.Text a -> Bool
cursorIsAtEnd e =
    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z
        z = e^.editContentsL
    in col == T.length curLine

cursorIsAtBeginning :: Editor T.Text a -> Bool
cursorIsAtBeginning e =
    let col = snd $ Z.cursorPosition z
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
    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z
        z = e^.editContentsL
    in (T.singleton ch) `T.isPrefixOf` T.drop col curLine
