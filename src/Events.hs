{-# LANGUAGE MultiWayIf #-}
module Events where

import           Prelude ()
import           Prelude.Compat

import           Brick
import           Control.Monad (forM_, when)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Monoid ((<>))
import           GHC.Exts (groupWith)
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform

import           Network.Mattermost.Types
import           Network.Mattermost.Lenses
import           Network.Mattermost.WebSocket

import           Connection
import           State
import           State.Common
import           Types
import           Types.KeyEvents

import           Events.Keybindings
import           Events.ShowHelp
import           Events.Main
import           Events.JoinChannel
import           Events.ChannelScroll
import           Events.ChannelSelect
import           Events.LeaveChannelConfirm
import           Events.DeleteChannelConfirm
import           Events.UrlSelect
import           Events.MessageSelect
import           Events.PostListOverlay
import           Events.UserListOverlay

onEvent :: ChatState -> BrickEvent Name MHEvent -> EventM Name (Next ChatState)
onEvent st ev = runMHEvent st (onEv >> fetchVisibleIfNeeded)
    where onEv = do case ev of
                      (AppEvent e) -> onAppEvent e
                      (VtyEvent (Vty.EvKey (Vty.KChar 'l') [Vty.MCtrl])) -> do
                           vty <- mh getVtyHandle
                           liftIO $ Vty.refresh vty
                      (VtyEvent e) -> onVtyEvent e
                      _ -> return ()

onAppEvent :: MHEvent -> MH ()
onAppEvent RefreshWebsocketEvent = connectWebsockets
onAppEvent WebsocketDisconnect = do
  csConnectionStatus .= Disconnected
  disconnectChannels
onAppEvent WebsocketConnect = do
  csConnectionStatus .= Connected
  refreshChannelsAndUsers
  refreshClientConfig
onAppEvent BGIdle     = csWorkerIsBusy .= Nothing
onAppEvent (BGBusy n) = csWorkerIsBusy .= Just n
onAppEvent (WSEvent we) =
  handleWSEvent we
onAppEvent (RespEvent f) = f
onAppEvent (AsyncErrEvent e) = do
  let msg = "An unexpected error has occurred! The exception encountered was:\n  " <>
            T.pack (show e) <>
            "\nPlease report this error at https://github.com/matterhorn-chat/matterhorn/issues"
  postErrorMessage msg
onAppEvent (WebsocketParseError e) = do
  let msg = "A websocket message could not be parsed:\n  " <>
            T.pack e <>
            "\nPlease report this error at https://github.com/matterhorn-chat/matterhorn/issues"
  postErrorMessage msg

onVtyEvent :: Vty.Event -> MH ()
onVtyEvent e = do
    -- Even if we aren't showing the help UI when a resize occurs, we
    -- need to invalidate its cache entry anyway in case the new size
    -- differs from the cached size.
    case e of
        (Vty.EvResize _ _) -> do
            mh $ invalidateCacheEntry HelpText
            mh $ invalidateCacheEntry ScriptHelpText
        _ -> return ()

    mode <- gets appMode
    case mode of
        Main                       -> onEventMain e
        ShowHelp _                 -> onEventShowHelp e
        ChannelSelect              -> onEventChannelSelect e
        UrlSelect                  -> onEventUrlSelect e
        LeaveChannelConfirm        -> onEventLeaveChannelConfirm e
        JoinChannel                -> onEventJoinChannel e
        ChannelScroll              -> onEventChannelScroll e
        MessageSelect              -> onEventMessageSelect e
        MessageSelectDeleteConfirm -> onEventMessageSelectDeleteConfirm e
        DeleteChannelConfirm       -> onEventDeleteChannelConfirm e
        PostListOverlay _          -> onEventPostListOverlay e
        UserListOverlay            -> onEventUserListOverlay e

handleWSEvent :: WebsocketEvent -> MH ()
handleWSEvent we = do
    myId <- gets myUserId
    myTId <- gets myTeamId
    case weEvent we of
        WMPosted
            | Just p <- wepPost (weData we) -> do
                -- If the message is a header change, also update the
                -- channel metadata.
                let wasMentioned = case wepMentions (weData we) of
                      Just lst -> myId `Set.member` lst
                      _ -> False
                addNewPostedMessage $ RecentPost p wasMentioned
            | otherwise -> return ()

        WMPostEdited
            | Just p <- wepPost (weData we) -> editMessage p
            | otherwise -> return ()

        WMPostDeleted
            | Just p <- wepPost (weData we) -> deleteMessage p
            | otherwise -> return ()

        WMStatusChange
            | Just status <- wepStatus (weData we)
            , Just uId <- wepUserId (weData we) ->
                setUserStatus uId status
            | otherwise -> return ()

        WMUserAdded
            | Just cId <- webChannelId (weBroadcast we) ->
                when (wepUserId (weData we) == Just myId &&
                      wepTeamId (weData we) == Just myTId) $
                    handleChannelInvite cId
            | otherwise -> return ()

        WMNewUser
            | Just uId <- wepUserId $ weData we -> handleNewUsers (Seq.singleton uId)
            | otherwise -> return ()

        WMUserRemoved
            | Just cId <- wepChannelId (weData we) ->
                when (webUserId (weBroadcast we) == Just myId) $
                    removeChannelFromState cId
            | otherwise -> return ()

        WMTyping
            | Just uId <- wepUserId $ weData we
            , Just cId <- webChannelId (weBroadcast we) -> handleTypingUser uId cId
            | otherwise -> return ()

        WMChannelDeleted
            | Just cId <- wepChannelId (weData we) ->
                when (webTeamId (weBroadcast we) == Just myTId) $
                    removeChannelFromState cId
            | otherwise -> return ()

        WMDirectAdded
            | Just cId <- webChannelId (weBroadcast we) -> handleChannelInvite cId
            | otherwise -> return ()

        -- An 'ephemeral message' is just Mattermost's version of our
        -- 'client message'. This can be a little bit wacky, e.g.
        -- if the user types '/shortcuts' in the browser, we'll get
        -- an ephemeral message even in MatterHorn with the browser
        -- shortcuts, but it's probably a good idea to handle these
        -- messages anyway.
        WMEphemeralMessage
            | Just p <- wepPost $ weData we -> postInfoMessage $ p^.postMessageL
            | otherwise -> return ()

        WMPreferenceChanged
            | Just prefs <- wepPreferences (weData we) ->
                mapM_ applyPreferenceChange prefs
            | otherwise -> return ()

        WMPreferenceDeleted
            | Just pref <- wepPreferences (weData we)
            , Just fps <- mapM preferenceToFlaggedPost pref ->
              forM_ fps $ \f ->
                  updateMessageFlag (flaggedPostId f) False
            | otherwise -> return ()

        WMReactionAdded
            | Just r <- wepReaction (weData we)
            , Just cId <- webChannelId (weBroadcast we) -> addReactions cId [r]
            | otherwise -> return ()

        WMReactionRemoved
            | Just r <- wepReaction (weData we)
            , Just cId <- webChannelId (weBroadcast we) -> removeReaction r cId
            | otherwise -> return ()

        WMChannelViewed
            | Just cId <- wepChannelId $ weData we -> refreshChannelById cId
            | otherwise -> return ()

        WMChannelUpdated
            | Just cId <- webChannelId $ weBroadcast we -> refreshChannelById cId
            | otherwise -> return ()

        WMGroupAdded
            | Just cId <- webChannelId (weBroadcast we) -> handleChannelInvite cId
            | otherwise -> return ()

        -- We are pretty sure we should do something about these:
        WMAddedToTeam -> return ()

        -- We aren't sure whether there is anything we should do about
        -- these yet:
        WMUpdateTeam -> return ()
        WMUserUpdated -> return ()
        WMLeaveTeam -> return ()

        -- We deliberately ignore these events:
        WMChannelCreated -> return ()
        WMEmojiAdded -> return ()
        WMWebRTC -> return ()
        WMHello -> return ()
        WMAuthenticationChallenge -> return ()
        WMUserRoleUpdated -> return ()


-- | Given a configuration, we want to check it for internal
-- consistency (i.e. that a given keybinding isn't associated with
-- multiple events which both need to get generated in the same UI
-- mode) and also for basic usability (i.e. we shouldn't be binding
-- events which can appear in the main UI to a key like @e@, which
-- would prevent us from being able to type messages containing an @e@
-- in them!
ensureKeybindingConsistency :: KeyConfig -> Either String ()
ensureKeybindingConsistency kc = mapM_ checkGroup allBindings
  where
    -- This is a list of lists, grouped by keybinding, of all the
    -- keybinding/event associations that are going to be used with
    -- the provided key configuration.
    allBindings = groupWith fst $ concat
      [ case M.lookup ev kc of
          Nothing -> zip (defaultBindings ev) (repeat (False, ev))
          Just (BindingList bs) -> zip bs (repeat (True, ev))
          Just Unbound -> []
      | ev <- allEvents
      ]

    -- the invariant here is that each call to checkGroup is made with
    -- a list where the first element of every list is the same
    -- binding. The Bool value in these is True if the event was
    -- associated with the binding by the user, and False if it's a
    -- Matterhorn default.
    checkGroup :: [(Binding, (Bool, KeyEvent))] -> Either String ()
    checkGroup [] = error "[ensureKeybindingConsistency: unreachable]"
    checkGroup evs@((b, _):_) = do

      -- We find out which modes an event can be used in and then
      -- invert the map, so this is a map from mode to the events
      -- contains which are bound by the binding included above.
      let modesFor :: M.Map String [(Bool, KeyEvent)]
          modesFor = M.unionsWith (++)
            [ M.fromList [ (m, [(i, ev)]) | m <- modeMap ev ]
            | (_, (i, ev)) <- evs
            ]

      -- If there is ever a situation where the same key is bound to
      -- two events which can appear in the same mode, then we want to
      -- throw an error, and also be informative about why. It is
      -- still okay to bind the same key to two events, so long as
      -- those events never appear in the same UI mode.
      forM_ (M.assocs modesFor) $ \ (_, vs) ->
         when (length vs > 1) $
           Left $ concat $
             "Multiple overlapping events bound to `" :
             T.unpack (ppBinding b) :
             "`:\n" :
             concat [ [ " - `"
                      , T.unpack (keyEventName ev)
                      , "` "
                      , if isFromUser
                          then "(via user override)"
                          else "(matterhorn default)"
                      , "\n"
                      ]
                    | (isFromUser, ev) <- vs
                    ]

      -- check for overlap a set of built-in keybindings when we're in
      -- a mode where the user is typing. (These are perfectly fine
      -- when we're in other modes.)
      when ("main" `M.member` modesFor && isBareBinding b) $ do
        Left $ concat $
          [ "The keybinding `"
          , T.unpack (ppBinding b)
          , "` is bound to the "
          , case map (ppEvent . snd . snd) evs of
              [] -> error "unreachable"
              [e] -> "event " ++ e
              es  -> "events " ++ intercalate " and " es
          , "\n"
          , "This is probably not what you want, as it will interfere\n"
          , "with the ability to write messages!\n"
          ]

    -- Events get some nice formatting!
    ppEvent ev = "`" ++ T.unpack (keyEventName ev) ++ "`"

    -- This check should get more nuanced, but as a first
    -- approximation, we shouldn't bind to any bare character key in
    -- the main mode.
    isBareBinding (Binding [] (Vty.KChar {})) = True
    isBareBinding _ = False

    -- We generate the which-events-are-valid-in-which-modes map from
    -- our actual keybinding set, so this should never get out of date.
    modeMap ev =
      let bindingHasEvent (KB _ _ _ (Just ev')) = ev == ev'
          bindingHasEvent _ = False
      in [ mode
         | (mode, bindings) <- modeMaps
         , any bindingHasEvent bindings
         ]

    modeMaps = [ ("main" :: String, mainKeybindings kc)
               , ("help screen", helpKeybindings kc)
               , ("channel select", channelSelectKeybindings kc)
               , ("url select", urlSelectKeybindings kc)
               , ("channel scroll", channelScrollKeybindings kc)
               , ("message select", messageSelectKeybindings kc)
               , ("post list overlay", postListOverlayKeybindings kc)
               ]
