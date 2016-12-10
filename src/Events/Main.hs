{-# LANGUAGE MultiWayIf #-}
module Events.Main where

import Brick
import Brick.Widgets.Edit
import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Zipper as Z
import qualified Graphics.Vty as Vty
import Lens.Micro.Platform

import Types
import State
import State.Editing
import Command
import Completion
import InputHistory

onEventMain :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventMain st e | Just kb <- lookupKeybinding e mainKeybindings = kbAction kb st
onEventMain st (Vty.EvPaste bytes) = continue $ handlePaste bytes st
onEventMain st e = continue =<< handleEditingInput e st

mainKeybindings :: [Keybinding]
mainKeybindings =
    [ KB "Show this help screen"
         (Vty.EvKey (Vty.KFun 1) []) $
         showHelpScreen MainHelp >=> continue

    , KB "Toggle message preview"
         (Vty.EvKey (Vty.KChar 'p') [Vty.MMeta]) $
         continue . toggleMessagePreview

    , KB "Invoke *$EDITOR* to edit the current message"
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
             let cId = st^.csCurrentChannelId
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

    , KB "Select and open a URL posted to the current channel"
         (Vty.EvKey (Vty.KChar 'o') [Vty.MCtrl]) $
           continue . startUrlSelect

    , KB "Clear the current channel's unread message indicator"
         (Vty.EvKey (Vty.KChar 'l') [Vty.MMeta]) $ \st ->
           continue =<< clearNewMessageCutoff (st^.csCurrentChannelId) st

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
      cId = st^.csCurrentChannelId
      st' = st & cmdLine %~ applyEdit Z.clearZipper
               & csInputHistory %~ addHistoryEntry allLines cId
               & csInputHistoryPosition.at cId .~ Nothing
  case T.uncons line of
    Just ('/',cmd) -> dispatchCommand cmd st'
    _              -> do
      liftIO (sendMessage st' allLines)
      continue st'

tabComplete :: Completion.Direction
            -> ChatState -> EventM Name (Next ChatState)
tabComplete dir st = do
  let priorities  = [] :: [T.Text]-- XXX: add recent completions to this
      completions = Set.fromList (st^.csNames.cnUsers ++
                                  st^.csNames.cnChans ++
                                  map ("@" <>) (st^.csNames.cnUsers) ++
                                  map ("#" <>) (st^.csNames.cnChans) ++
                                  map ("/" <>) (map commandName commandList))

      line        = Z.currentLine $ st^.cmdLine.editContentsL
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
        line = Z.currentLine z
    in delete n z
