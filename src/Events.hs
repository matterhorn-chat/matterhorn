module Events where

import           Brick
import           Brick.Widgets.Edit ( getEditContents
                                    , handleEditorEvent
                                    , applyEdit
                                    , editContentsL
                                    )
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import           Data.Text.Zipper ( TextZipper
                                  , getText
                                  , stringZipper
                                  , clearZipper
                                  , gotoEOL
                                  , insertMany
                                  , deletePrevChar )
import qualified Data.Text as T
import Data.Monoid ((<>))
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
onEvent st (WSEvent we) =
  handleWSEvent st we
onEvent st (RespEvent f) =
  continue =<< f st
onEvent st (VtyEvent e) = do
    case st^.csMode of
        Main          -> onEventMain st e
        ShowHelp      -> onEventShowHelp st e
        ChannelSelect -> onEventChannelSelect st e

onEventShowHelp :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventShowHelp st (Vty.EvKey Vty.KLeft []) = do
  hScrollBy (viewportScroll HelpViewport) (-1)
  continue st
onEventShowHelp st (Vty.EvKey Vty.KRight []) = do
  hScrollBy (viewportScroll HelpViewport) 1
  continue st
onEventShowHelp st (Vty.EvKey Vty.KUp []) = do
  vScrollBy (viewportScroll HelpViewport) (-1)
  continue st
onEventShowHelp st (Vty.EvKey Vty.KDown []) = do
  vScrollBy (viewportScroll HelpViewport) 1
  continue st
onEventShowHelp st (Vty.EvKey Vty.KPageUp []) = do
  vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
  continue st
onEventShowHelp st (Vty.EvKey Vty.KPageDown []) = do
  vScrollBy (viewportScroll HelpViewport) pageAmount
  continue st
onEventShowHelp st (Vty.EvKey (Vty.KChar ' ') []) = do
  vScrollPage (viewportScroll HelpViewport) Down
  continue st
onEventShowHelp st (Vty.EvKey _ _) = do
  continue $ st & csMode .~ Main
onEventShowHelp st _ = continue st

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
onEventMain st (Vty.EvKey (Vty.KFun 1) []) =
  continue $ st & csMode .~ ShowHelp
onEventMain st (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) =
  continue $ st & csMode          .~ ChannelSelect
                & csChannelSelect .~ ""
onEventMain st (Vty.EvKey (Vty.KChar 'q') [Vty.MCtrl]) =
  halt st
onEventMain st (Vty.EvKey (Vty.KChar '\t') []) =
  tabComplete Forwards st
onEventMain st (Vty.EvKey (Vty.KBackTab) []) =
  tabComplete Backwards st
onEventMain st (Vty.EvKey Vty.KUp []) =
  continue $ channelHistoryBackward st
onEventMain st (Vty.EvKey Vty.KDown []) =
  continue $ channelHistoryForward st
onEventMain st (Vty.EvKey Vty.KPageUp []) =
  continue =<< channelPageUp st
onEventMain st (Vty.EvKey Vty.KPageDown []) =
  continue =<< channelPageDown st
onEventMain st (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl]) =
  continue =<< updateChannelScrollState =<< nextChannel st
onEventMain st (Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl]) =
  continue =<< updateChannelScrollState =<< prevChannel st
onEventMain st (Vty.EvKey Vty.KEnter []) = do
  let st' = st & csCurrentCompletion .~ Nothing
  handleInputSubmission st'
onEventMain st (Vty.EvPaste bytes) = do
  -- If you paste a multi-line thing, it'll only insert up to the first
  -- line ending because the zipper will respect the line limit when
  -- inserting the paste. Once we add support for multi-line editing,
  -- this will Just Work once the editor's line limit is set to Nothing.
  let pasteStr = UTF8.toString bytes
  continue $ st & cmdLine %~ applyEdit (insertMany pasteStr)
onEventMain st e = do
  let st' = case e of
            -- XXX: not 100% certain we need to special case these
            -- the intention is to notice when the user has finished word completion
            -- and moved on. Needs more testing.
            Vty.EvKey (Vty.KChar ' ') [] -> st & csCurrentCompletion .~ Nothing
            Vty.EvKey Vty.KBS         [] -> st & csCurrentCompletion .~ Nothing
            _ -> st
  continue =<< handleEventLensed st' cmdLine handleEditorEvent e

onEventChannelSelect :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventChannelSelect st (Vty.EvKey Vty.KEnter []) = do
    -- If the text entered matches only one channel, switch to it
    let matches = filter (s `T.isInfixOf`) $ (st^.csNames.cnChans <> st^.csNames.cnUsers)
        s = st^.csChannelSelect
    case matches of
        [single] ->
            case channelByName st single of
              Just cId -> continue =<< (setFocus cId $ st & csMode .~ Main)
              Nothing  -> continue st
        _ -> continue st

onEventChannelSelect st (Vty.EvKey Vty.KBS []) = do
    continue $ st & csChannelSelect %~ (\s -> if T.null s then s else T.init s)
onEventChannelSelect st (Vty.EvKey (Vty.KChar c) []) | c /= '\t' = do
    continue $ st & csChannelSelect %~ (flip T.snoc c)
onEventChannelSelect st (Vty.EvKey Vty.KEsc []) = do
    continue $ st & csMode .~ Main
onEventChannelSelect st _ = do
    continue st

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

-- XXX: killWordBackward, and delete could probably all
-- be moved to the text zipper package (after some generalization and cleanup)
-- for example, we should look up the standard unix word break characters
-- and use those in killWordBackward.
killWordBackward :: TextZipper String -> TextZipper String
killWordBackward z =
    let n = length
          $ takeWhile (/= ' ')
          $ reverse line
        delete n' z' | n' <= 0 = z'
        delete n' z' = delete (n'-1) (deletePrevChar z')
        (line:_) = getText z
    in delete n z

tabComplete :: Completion.Direction
            -> ChatState -> EventM Name (Next ChatState)
tabComplete dir st = do
  let priorities  = [] :: [T.Text]-- XXX: add recent completions to this
      completions = Set.fromList (st^.csNames.cnUsers ++
                                  st^.csNames.cnChans ++
                                  map ("@" <>) (st^.csNames.cnUsers) ++
                                  map ("#" <>) (st^.csNames.cnChans) ++
                                  map ("/" <>) (map commandName commandList))

      (line:_)    = T.pack <$> getEditContents (st^.cmdLine)
      curComp     = st^.csCurrentCompletion
      nextComp    = case curComp of
                    Nothing -> Just (currentWord line)
                    _       -> curComp

      mb_word     = wordComplete dir priorities completions line curComp
      st' = st & csCurrentCompletion .~ nextComp
      edit = case mb_word of
          Nothing -> id
          Just w -> insertMany (T.unpack w) . killWordBackward

  continue $ st' & cmdLine %~ (applyEdit edit)

handleInputSubmission :: ChatState -> EventM Name (Next ChatState)
handleInputSubmission st = do
  let (line:_) = getEditContents (st^.cmdLine)
      cId = currentChannelId st
      st' = st & cmdLine %~ applyEdit clearZipper
               & csInputHistory %~ addHistoryEntry line cId
               & csInputHistoryPosition.at cId .~ Nothing
  case line of
    ('/':cmd) -> dispatchCommand (T.pack cmd) st'
    _         -> do
      liftIO (sendMessage st' $ T.pack line)
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
    WMStatusChange -> case wepStatus (weData we) of
      Just status -> updateStatus (weUserId we) status st >>= continue
      Nothing -> continue st
    _ -> continue st

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
