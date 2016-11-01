{-# LANGUAGE MultiWayIf #-}
module Events.Main where

import Brick
import Brick.Widgets.Edit
import qualified Codec.Binary.UTF8.Generic as UTF8
import Control.Arrow
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Network.Mattermost
import Network.Mattermost.Lenses

import Config
import Types
import State
import Command
import Completion
import InputHistory

onEventMain :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventMain st (Vty.EvResize _ _) = do
  -- On resize we need to update the current channel message area so
  -- that the most recent message is at the bottom. We have to do this
  -- on a resize because brick only guarantees that the message is
  -- visible, not that it is at the bottom, so after a resize we can end
  -- up with lots of whitespace at the bottom of the message area. This
  -- whitespace is created when the window gets bigger. We only need to
  -- worry about the current channel's viewport because that's the one
  -- that is about to be redrawn.
  continue =<< updateChannelScrollState st
onEventMain st e | Just kb <- lookupKeybinding e mainKeybindings = kbAction kb st
onEventMain st (Vty.EvPaste bytes) = do
  let pasteStr = T.pack (UTF8.toString bytes)
      st' = st & cmdLine %~ applyEdit (Z.insertMany pasteStr)
  case length (getEditContents $ st'^.cmdLine) > 1 of
      True -> continue $ startMultilineEditing st'
      False -> continue st'
onEventMain st e
  | (length (getEditContents $ st^.cmdLine) == 1) || st^.csEditState.cedMultiline = do

    let smartBacktick = st^.csResources.crConfiguration.to configSmartBacktick
        smartChars = "*`_"
    st' <- case e of
        Vty.EvKey (Vty.KChar 't') [Vty.MCtrl] ->
            return $ st & cmdLine %~ applyEdit Z.transposeChars

        Vty.EvKey Vty.KBS [] | smartBacktick ->
            let backspace = return $ st & cmdLine %~ applyEdit Z.deletePrevChar
            in case cursorAtOneOf smartChars (st^.cmdLine) of
                Nothing -> backspace
                Just ch ->
                    -- Smart char removal:
                    if | (cursorAtChar ch $ applyEdit Z.moveLeft $ st^.cmdLine) &&
                         (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.cmdLine) ->
                           return $ st & cmdLine %~ applyEdit (Z.deleteChar >>> Z.deletePrevChar)
                       | otherwise -> backspace

        Vty.EvKey (Vty.KChar ch) [] | smartBacktick && ch `elem` smartChars ->
            -- Smart char insertion:
            let insertChar = return $ st & cmdLine %~ applyEdit (Z.insertChar ch)
            in if | (editorEmpty $ st^.cmdLine) ||
                       ((cursorAtChar ' ' (applyEdit Z.moveLeft $ st^.cmdLine)) &&
                        (cursorIsAtEnd $ st^.cmdLine)) ->
                      return $ st & cmdLine %~ applyEdit (Z.insertMany (T.pack $ ch:ch:[]) >>> Z.moveLeft)
                  -- Note that this behavior will have to improve once we
                  -- support multi-line editing because in that context
                  -- ```...``` is something people will want to type.
                  | (cursorAtChar ch $ st^.cmdLine) &&
                    (cursorIsAtEnd $ applyEdit Z.moveRight $ st^.cmdLine) ->
                      return $ st & cmdLine %~ applyEdit Z.moveRight
                  | otherwise -> insertChar

        _ -> handleEventLensed st cmdLine handleEditorEvent e

    continue $ st' & csCurrentCompletion .~ Nothing
onEventMain st _ = continue st

mainKeybindings :: [Keybinding]
mainKeybindings =
    [ KB "Show this help screen"
         (Vty.EvKey (Vty.KFun 1) []) $
         showHelpScreen >=> continue

    , KB "Toggle message preview"
         (Vty.EvKey (Vty.KChar 'p') [Vty.MMeta]) $
         continue . toggleMessagePreview

    , KB "Invoke $EDITOR to edit current message"
         (Vty.EvKey (Vty.KChar 'k') [Vty.MMeta]) $
         invokeExternalEditor

    , KB "Enter fast channel selection mode"
         (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) $
         continue . beginChannelSelect

    , KB "Quit"
         (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) halt

    , KB "Tab-complete forward"
         (Vty.EvKey (Vty.KChar '\t') []) $
         tabComplete Forwards

    , KB "Tab-complete backward"
         (Vty.EvKey (Vty.KBackTab) []) $
         tabComplete Backwards

    , KB "Scroll up in the channel input history"
         (Vty.EvKey Vty.KUp []) $ \st ->
             -- Up in multiline mode does the usual thing; otherwise we
             -- navigate the history.
             case st^.csEditState.cedMultiline of
                 True -> continue =<< handleEventLensed st cmdLine handleEditorEvent
                                           (Vty.EvKey Vty.KUp [])
                 False -> continue $ channelHistoryBackward st

    , KB "Scroll down in the channel input history"
         (Vty.EvKey Vty.KDown []) $ \st ->
             -- Down in multiline mode does the usual thing; otherwise
             -- we navigate the history.
             case st^.csEditState.cedMultiline of
                 True -> continue =<< handleEventLensed st cmdLine handleEditorEvent
                                           (Vty.EvKey Vty.KDown [])
                 False -> continue $ channelHistoryForward st

    , KB "Page up in the channel message list"
         (Vty.EvKey Vty.KPageUp []) $ \st -> do
             let cId = currentChannelId st
                 vp = ChannelMessages cId
             invalidateCacheEntry vp
             vScrollToEnd $ viewportScroll vp
             vScrollBy (viewportScroll vp) (-1 * pageAmount)
             continue $ st & csMode .~ ChannelScroll

    , KB "Change to the next channel in the channel list"
         (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) $
         nextChannel >=> continue

    , KB "Change to the previous channel in the channel list"
         (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) $
         prevChannel >=> continue

    , KB "Change to the next channel with unread messages"
         (Vty.EvKey (Vty.KChar 'a') [Vty.MMeta]) $
         nextUnreadChannel >=> continue

    , KB "Change to the most recently-focused channel"
         (Vty.EvKey (Vty.KChar 's') [Vty.MMeta]) $
         recentChannel >=> continue

    , KB "Send the current message"
         (Vty.EvKey Vty.KEnter []) $ \st -> do
             case st^.csEditState.cedMultiline of
                 -- Enter in multiline mode does the usual thing; we
                 -- only send on Enter when we're outside of multiline
                 -- mode.
                 True -> continue =<< handleEventLensed st cmdLine handleEditorEvent
                                           (Vty.EvKey Vty.KEnter [])
                 False -> handleInputSubmission $ st & csCurrentCompletion .~ Nothing

    , KB "Delete the current multi-line message, if any"
         (Vty.EvKey Vty.KBS []) $ \st -> do
             case st^.csEditState.cedMultiline of
                 -- Backspace in multiline mode does the usual thing.
                 True -> continue =<< handleEventLensed st cmdLine handleEditorEvent
                                           (Vty.EvKey Vty.KBS [])
                 False ->
                     -- Backspace outside multiline mode means delete
                     -- the whole message if it has more than one line,
                     -- or do the usual thing otherwise.
                     case length (getEditContents $ st^.cmdLine) == 1 of
                         False -> continue $ st & cmdLine %~ applyEdit Z.clearZipper
                         True -> continue =<< handleEventLensed st cmdLine handleEditorEvent
                                           (Vty.EvKey Vty.KBS [])

    , KB "Open the most recently-posted URL"
         (Vty.EvKey (Vty.KChar 'o') [Vty.MCtrl]) $
           openMostRecentURL >=> continue

    , KB "Clear the current channel's unread message indicator"
         (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl]) $ \st ->
           continue =<< clearNewMessageCutoff (currentChannelId st) st

    , KB "Switch to multi-line message compose mode"
         (Vty.EvKey (Vty.KChar 'e') [Vty.MMeta]) $
           continue . startMultilineEditing

    , KB "Leave multi-line message compose mode"
         (Vty.EvKey Vty.KEsc []) $
           continue . stopMultilineEditing
    ]

handleInputSubmission :: ChatState -> EventM Name (Next ChatState)
handleInputSubmission st = do
  let (line:rest) = getEditContents (st^.cmdLine)
      allLines = T.intercalate "\n" $ line : rest
      cId = currentChannelId st
      st' = st & cmdLine %~ applyEdit Z.clearZipper
               & csInputHistory %~ addHistoryEntry allLines cId
               & csInputHistoryPosition.at cId .~ Nothing
  case T.uncons line of
    Just ('/',cmd) -> dispatchCommand cmd st'
    _              -> do
      liftIO (sendMessage st' allLines)
      continue st'

shouldSkipMessage :: T.Text -> Bool
shouldSkipMessage "" = True
shouldSkipMessage s = T.all (`elem` (" \t"::String)) s

sendMessage :: ChatState -> T.Text -> IO ()
sendMessage st msg =
    case shouldSkipMessage msg of
        True -> return ()
        False -> do
            let myId   = st^.csMe.userIdL
                chanId = currentChannelId st
                theTeamId = st^.csMyTeam.teamIdL
            doAsync st $ do
              pendingPost <- mkPendingPost msg myId chanId
              doAsync st $ do
                _ <- mmPost (st^.csConn) (st^.csTok) theTeamId pendingPost
                return ()

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

lastIsBacktick :: Editor T.Text a -> Bool
lastIsBacktick e =
    let curLine = Z.currentLine z
        z = e^.editContentsL
    in T.length curLine > 0 && T.last curLine == '`'

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

tabComplete :: Completion.Direction
            -> ChatState -> EventM Name (Next ChatState)
tabComplete dir st = do
  let priorities  = [] :: [T.Text]-- XXX: add recent completions to this
      completions = Set.fromList (st^.csNames.cnUsers ++
                                  st^.csNames.cnChans ++
                                  map ("@" <>) (st^.csNames.cnUsers) ++
                                  map ("#" <>) (st^.csNames.cnChans) ++
                                  map ("/" <>) (map commandName commandList))

      (line:_)    = getEditContents (st^.cmdLine)
      curComp     = st^.csCurrentCompletion
      (nextComp, alts) = case curComp of
          Nothing -> let cw = currentWord line
                     in (Just cw, filter (cw `T.isPrefixOf`) $ Set.toList completions)
          Just cw -> (Just cw, filter (cw `T.isPrefixOf`) $ Set.toList completions)

      mb_word     = wordComplete dir priorities completions line curComp
      st' = st & csCurrentCompletion .~ nextComp
               & csEditState.cedCompletionAlternatives .~ alts
      (edit, curAlternative) = case mb_word of
          Nothing -> (id, "")
          Just w -> (Z.insertMany w . killWordBackward, w)

  continue $ st' & cmdLine %~ (applyEdit edit)
                 & csEditState.cedCurrentAlternative .~ curAlternative

-- XXX: killWordBackward, and delete could probably all
-- be moved to the text zipper package (after some generalization and cleanup)
-- for example, we should look up the standard unix word break characters
-- and use those in killWordBackward.
killWordBackward :: Z.TextZipper T.Text -> Z.TextZipper T.Text
killWordBackward z =
    let n = T.length
          $ T.takeWhile (/= ' ')
          $ T.reverse line
        delete n' z' | n' <= 0 = z'
        delete n' z' = delete (n'-1) (Z.deletePrevChar z')
        (line:_) = Z.getText z
    in delete n z
