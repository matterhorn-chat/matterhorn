module Events where

import           Brick
import           Brick.Widgets.Edit ( getEditContents
                                    , handleEditorEvent
                                    , applyEdit
                                    , editContentsL
                                    , editContents
                                    )
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import           Data.Text.Zipper ( stringZipper
                                  , clearZipper
                                  , gotoEOL
                                  , insertMany
                                  , deletePrevChar )
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform
import qualified Codec.Binary.UTF8.Generic as UTF8

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket.Types

import           Command
import           Completion
import           State
import           Types
import           InputHistory

onEvent :: ChatState -> Event -> EventM Name (Next ChatState)
onEvent st (VtyEvent (Vty.EvResize _ _)) = do
  -- On resize we need to update the current channel message area so
  -- that the most recent message is at the bottom. We have to do this
  -- on a resize because brick only guarantees that the message is
  -- visible, not that it is at the bottom, so after a resize we can end
  -- up with lots of whitespace at the bottom of the message area. This
  -- whitespace is created when the window gets bigger. We only need to
  -- worry about the current channel's viewport because that's the one
  -- that is about to be redrawn.
  continue =<< updateChannelScrollState st
onEvent st (VtyEvent (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl])) =
  halt st
onEvent st (VtyEvent (Vty.EvKey (Vty.KChar '\t') [])) =
  tabComplete Forwards st
onEvent st (VtyEvent (Vty.EvKey (Vty.KBackTab) [])) =
  tabComplete Backwards st
onEvent st (VtyEvent (Vty.EvKey Vty.KUp [])) =
  continue $ channelHistoryBackward st
onEvent st (VtyEvent (Vty.EvKey Vty.KDown [])) =
  continue $ channelHistoryForward st
onEvent st (VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl])) =
  continue =<< updateChannelScrollState =<< nextChannel st
onEvent st (VtyEvent (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl])) =
  continue =<< updateChannelScrollState =<< prevChannel st
onEvent st (VtyEvent (Vty.EvKey Vty.KEnter [])) = do
  let st' = st & csCurrentCompletion .~ Nothing
  handleInputSubmission st'
onEvent st (VtyEvent (Vty.EvPaste bytes)) = do
  -- For now, only support single-line pastes. If you paste a multi-line
  -- thing, it'll only insert up to the first line ending. Once we add
  -- support for multi-line editing, this should change the editing mode
  -- to multi-line editing so you can adjust the paste before submitting
  -- it.
  let pasteStr = UTF8.toString bytes
      ls = lines pasteStr
  continue $ st & cmdLine %~ applyEdit (insertMany $ head ls)
onEvent st (VtyEvent e) = do
  let st' = case e of
            -- XXX: not 100% certain we need to special case these
            -- the intention is to notice when the user has finished word completion
            -- and moved on. Needs more testing.
            Vty.EvKey (Vty.KChar ' ') [] -> st & csCurrentCompletion .~ Nothing
            Vty.EvKey Vty.KBS         [] -> st & csCurrentCompletion .~ Nothing
            _ -> st
  continue =<< handleEventLensed st' cmdLine handleEditorEvent e
onEvent st (WSEvent we) =
  handleWSEvent st we
onEvent st (RespEvent f) =
  continue (f st)

channelHistoryForward :: ChatState -> ChatState
channelHistoryForward st =
  let cId = currentChannelId st
  in case st^.csInputHistoryPosition.at cId of
      Just (Just i)
        | i == 0 ->
          -- Transition out of history navigation
          st & cmdLine %~ applyEdit clearZipper
             & csInputHistoryPosition.at cId .~ Just Nothing
        | otherwise ->
          let Just entry = getHistoryEntry cId newI (st^.csInputHistory)
              newI = i - 1
          in st & cmdLine.editContentsL .~ (gotoEOL $ stringZipper [entry] (Just 1))
                & csInputHistoryPosition.at cId .~ (Just $ Just newI)
      _ -> st

channelHistoryBackward :: ChatState -> ChatState
channelHistoryBackward st =
  let cId = currentChannelId st
  in case st^.csInputHistoryPosition.at cId of
      Just (Just i) ->
          let newI = i + 1
          in case getHistoryEntry cId newI (st^.csInputHistory) of
              Nothing -> st
              Just entry ->
                  st & cmdLine.editContentsL .~ (gotoEOL $ stringZipper [entry] (Just 1))
                     & csInputHistoryPosition.at cId .~ (Just $ Just newI)
      _ ->
          let newI = 0
          in case getHistoryEntry cId newI (st^.csInputHistory) of
              Nothing -> st
              Just entry ->
                  st & cmdLine.editContentsL .~ (gotoEOL $ stringZipper [entry] (Just 1))
                     & csInputHistoryPosition.at cId .~ (Just $ Just newI)

tabComplete :: Completion.Direction
            -> ChatState -> EventM Name (Next ChatState)
tabComplete dir st = do
  -- XXX: killWordBackward, and delete could probably all
  -- be moved to the text zipper package (after some generalization and cleanup)
  -- for example, we should look up the standard unix word break characters
  -- and use those in killWordBackward.
  let killWordBackward z =
        let n = length
              $ takeWhile (/= ' ')
              $ reverse
              $ line
        in delete n z
      delete n z | n <= 0 = z
      delete n z = delete (n-1) (deletePrevChar z)

      priorities  = [] :: [String]-- XXX: add recent completions to this
      completions = Set.fromList (st^.csNames.cnUsers ++
                                  st^.csNames.cnChans ++
                                  map ("@" ++) (st^.csNames.cnUsers) ++
                                  map ("#" ++) (st^.csNames.cnChans) ++
                                  map ("/" ++) (map commandName commandList))

      (line:_)    = getEditContents (st^.cmdLine)
      curComp     = st^.csCurrentCompletion
      nextComp    = case curComp of
                    Nothing -> Just (currentWord line)
                    _       -> curComp

      mb_word     = wordComplete dir priorities completions line curComp

      st'         = st & csCurrentCompletion .~ nextComp
      st''        = case mb_word of
                      Nothing -> st'
                      Just w  ->
                        -- JED: my lens-fu is not so great, but I know this could be
                        -- more succinct.
                        let contents  = editContents (st' ^. cmdLine)
                            backup    = st' & cmdLine.editContentsL .~ killWordBackward contents
                            contents' = editContents (backup ^. cmdLine)
                        in backup & cmdLine.editContentsL .~ insertMany w contents'
  continue st''

handleInputSubmission :: ChatState -> EventM Name (Next ChatState)
handleInputSubmission st = do
  let (line:_) = getEditContents (st^.cmdLine)
      cId = currentChannelId st
      st' = st & cmdLine %~ applyEdit clearZipper
               & csInputHistory %~ addHistoryEntry line cId
               & csInputHistoryPosition.at cId .~ Nothing
  case line of
    ('/':cmd) -> dispatchCommand cmd st'
    _         -> do
      liftIO (sendMessage st' line)
      continue st'

handleWSEvent :: ChatState -> WebsocketEvent -> EventM Name (Next ChatState)
handleWSEvent st we =
  case weEvent we of
    WMPosted -> case wepPost (weData we) of
      Just p  -> addMessage p st >>= continue
      Nothing -> continue st
    WMPostEdited -> case wepPost (weData we) of
      Just p  -> editMessage p st >>= continue
      Nothing -> continue st
    WMPostDeleted -> case wepPost (weData we) of
      Just p  -> deleteMessage p st >>= continue
      Nothing -> continue st
    _ -> continue st

shouldSkipMessage :: String -> Bool
shouldSkipMessage "" = True
shouldSkipMessage s = and $ (`elem` (" \t"::String)) <$> s

sendMessage :: ChatState -> String -> IO ()
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
