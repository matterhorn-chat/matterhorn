{-# LANGUAGE MultiWayIf #-}
module Events where

import           Brick
import           Brick.Widgets.Edit ( Editor
                                    , getEditContents
                                    , handleEditorEvent
                                    , applyEdit
                                    , editContentsL
                                    )
import           Brick.Widgets.List (handleListEvent, listSelectedElement)
import           Control.Applicative
import           Control.Monad ((>=>))
import           Control.Monad.IO.Class (liftIO)
import           Control.Arrow
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Text.Zipper as Z
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform
import qualified Codec.Binary.UTF8.Generic as UTF8

import           Prelude

import           Network.Mattermost
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket.Types

import           Command
import           Connection
import           Completion
import           State
import           Types
import           InputHistory
import           Config

onEvent :: ChatState -> BrickEvent Name MHEvent -> EventM Name (Next ChatState)
onEvent st (AppEvent e) = onAppEvent st e
onEvent st (VtyEvent e) = onVtyEvent st e
onEvent st _ = continue st

onAppEvent :: ChatState -> MHEvent -> EventM Name (Next ChatState)
onAppEvent st RefreshWebsocketEvent = do
  liftIO $ connectWebsockets st
  msg <- newClientMessage Informative "Websocket connecting..."
  continue =<< addClientMessage msg st
onAppEvent st WebsocketDisconnect = do
  msg <- newClientMessage Informative "Websocket disconnected."
  continue =<< (addClientMessage msg $ st & csConnectionStatus .~ Disconnected)
onAppEvent st WebsocketConnect = do
  msg <- newClientMessage Informative "Websocket reconnected."
  continue =<< (addClientMessage msg $ st & csConnectionStatus .~ Connected)
onAppEvent st (WSEvent we) =
  handleWSEvent st we
onAppEvent st (RespEvent f) =
  continue =<< f st
onAppEvent st (AsyncErrEvent e) = do
  msg <- newClientMessage Error $
    "An unexpected error has occurred! The exception encountered was:\n  " <>
    T.pack (show e) <>
    "\nPlease report this error at https://github.com/aisamanra/matterhorn/issues"
  continue =<< addClientMessage msg st

onVtyEvent :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onVtyEvent st e = do
    case st^.csMode of
        Main                -> onEventMain st e
        ShowHelp            -> onEventShowHelp st e
        ChannelSelect       -> onEventChannelSelect st e
        LeaveChannelConfirm -> onEventLeaveChannelConfirm st e
        JoinChannel         -> onEventJoinChannel st e
        ChannelScroll       -> onEventChannelScroll st e

onEventShowHelp :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventShowHelp st e | Just kb <- lookupKeybinding e helpKeybindings = kbAction kb st
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
onEventMain st e | Just kb <- lookupKeybinding e mainKeybindings = kbAction kb st
onEventMain st (Vty.EvPaste bytes) = do
  let pasteStr = T.pack (UTF8.toString bytes)
      st' = st & cmdLine %~ applyEdit (Z.insertMany pasteStr)
  case length (getEditContents $ st'^.cmdLine) > 1 of
      True -> continue =<< startMultilineEditing st'
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

joinChannelListKeys :: [Vty.Key]
joinChannelListKeys =
    [ Vty.KUp
    , Vty.KDown
    , Vty.KPageUp
    , Vty.KPageDown
    , Vty.KHome
    , Vty.KEnd
    ]

onEventJoinChannel :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventJoinChannel st e@(Vty.EvKey k []) | k `elem` joinChannelListKeys = do
    result <- case st^.csJoinChannelList of
        Nothing -> return Nothing
        Just l -> Just <$> handleListEvent e l
    continue $ st & csJoinChannelList .~ result
onEventJoinChannel st (Vty.EvKey Vty.KEnter []) = do
    case st^.csJoinChannelList of
        Nothing -> continue st
        Just l -> case listSelectedElement l of
            Nothing -> continue st
            Just (_, chan) -> joinChannel chan st >>= continue
onEventJoinChannel st (Vty.EvKey Vty.KEsc []) = do
    continue $ st & csMode .~ Main
onEventJoinChannel st _ = do
    continue st

onEventChannelScroll :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventChannelScroll st (Vty.EvResize _ _) = invalidateCache >> continue st
onEventChannelScroll st (Vty.EvKey Vty.KPageUp []) = channelPageUp st >>= continue
onEventChannelScroll st (Vty.EvKey Vty.KPageDown []) = channelPageDown st >>= continue
onEventChannelScroll st (Vty.EvKey Vty.KEsc []) = do
    continue $ st & csMode .~ Main
onEventChannelScroll st _ = do
    continue st

onEventLeaveChannelConfirm :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventLeaveChannelConfirm st (Vty.EvKey k []) = do
    st' <- case k of
        Vty.KChar c | c `elem` ("yY"::String) ->
            leaveCurrentChannel st
        _ -> return st
    continue $ st' & csMode .~ Main
onEventLeaveChannelConfirm st _ = do
    continue st

onEventChannelSelect :: ChatState -> Vty.Event -> EventM Name (Next ChatState)
onEventChannelSelect st e | Just kb <- lookupKeybinding e channelSelectKeybindings = kbAction kb st
onEventChannelSelect st (Vty.EvKey Vty.KBS []) = do
    continue $ updateChannelSelectMatches $ st & csChannelSelectString %~ (\s -> if T.null s then s else T.init s)
onEventChannelSelect st (Vty.EvKey (Vty.KChar c) []) | c /= '\t' = do
    continue $ updateChannelSelectMatches $ st & csChannelSelectString %~ (flip T.snoc c)
onEventChannelSelect st _ = do
    continue st

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
    WMChannelViewed -> case wepChannelId (weData we) of
      Just cId -> setLastViewedFor st cId >>= continue
      Nothing -> continue st
    _ -> continue st

lookupKeybinding :: Vty.Event -> [Keybinding] -> Maybe Keybinding
lookupKeybinding e kbs = listToMaybe $ filter ((== e) . kbEvent) kbs

channelSelectKeybindings :: [Keybinding]
channelSelectKeybindings =
    [ KB "Select matching channel"
         (Vty.EvKey Vty.KEnter []) $
         \st -> do
             -- If there is only one channel selection match, switch to
             -- it
             let allMatches = (HM.elems $ st^.csChannelSelectChannelMatches) <>
                              (HM.elems $ st^.csChannelSelectUserMatches)
             continue =<< case allMatches of
                 [single] -> changeChannel (channelNameFromMatch single) $ st & csMode .~ Main
                 _        -> return st

    , KB "Cancel channel selection"
         (Vty.EvKey Vty.KEsc []) $
         \st -> continue $ st & csMode .~ Main
    ]

helpKeybindings :: [Keybinding]
helpKeybindings =
    [ KB "Scroll up"
         (Vty.EvKey Vty.KUp []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) (-1)
             continue st
    , KB "Scroll down"
         (Vty.EvKey Vty.KDown []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) 1
             continue st
    , KB "Page up"
         (Vty.EvKey Vty.KPageUp []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) (-1 * pageAmount)
             continue st
    , KB "Page down"
         (Vty.EvKey Vty.KPageDown []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) pageAmount
             continue st
    , KB "Page down"
         (Vty.EvKey (Vty.KChar ' ') []) $
         \st -> do
             vScrollBy (viewportScroll HelpViewport) pageAmount
             continue st
    , KB "Return to the main interface"
         (Vty.EvKey Vty.KEsc []) $
         \st -> continue $ st & csMode .~ Main
    ]

mainKeybindings :: [Keybinding]
mainKeybindings =
    [ KB "Show this help screen"
         (Vty.EvKey (Vty.KFun 1) []) $
         showHelpScreen >=> continue

    , KB "Toggle message preview"
         (Vty.EvKey (Vty.KChar 'p') [Vty.MMeta]) $
         toggleMessagePreview >=> continue

    , KB "Invoke $EDITOR to edit current message"
         (Vty.EvKey (Vty.KChar 'k') [Vty.MMeta]) $
         invokeExternalEditor

    , KB "Enter fast channel selection mode"
         (Vty.EvKey (Vty.KChar 'g') [Vty.MCtrl]) $
         beginChannelSelect >=> continue

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

    , KB "Switch to multi-line message compose mode"
         (Vty.EvKey (Vty.KChar 'e') [Vty.MMeta]) $
           startMultilineEditing >=> continue

    , KB "Leave multi-line message compose mode"
         (Vty.EvKey Vty.KEsc []) $
           stopMultilineEditing >=> continue
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
