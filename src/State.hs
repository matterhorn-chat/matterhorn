{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module State where

import           Brick (EventM, invalidateCacheEntry)
import           Brick.Widgets.Edit (getEditContents, editContentsL)
import           Brick.Widgets.List (list, listMoveTo, listSelectedElement)
import           Control.Applicative
import           Control.Exception (catch)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char (isAlphaNum)
import           Brick.Main (getVtyHandle, viewportScroll, vScrollToBeginning, vScrollBy)
import           Brick.Widgets.Edit (applyEdit)
import           Control.Monad (when, void)
import           Data.Text.Zipper (textZipper, clearZipper, insertMany, gotoEOL)
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import           Data.List (sort)
import           Data.Maybe (maybeToList, isJust, catMaybes, isNothing)
import           Data.Monoid ((<>))
import           Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Foldable as F
import           Graphics.Vty (outputIface)
import           Graphics.Vty.Output.Interface (ringTerminalBell)
import           Lens.Micro.Platform
import           System.Process (system)

import           Prelude

import           Network.Mattermost
import           Network.Mattermost.Exceptions
import           Network.Mattermost.Lenses

import           Config
import           Types
import           InputHistory
import           Themes
import           Zipper (Zipper)
import qualified Zipper as Z
import           Markdown (blockGetURLs)

import           State.Common

pageAmount :: Int
pageAmount = 15

-- * Refreshing Channel Data

-- | Get all the new messages for a given channel
refreshChannel :: ChannelId -> ChatState -> IO ()
refreshChannel chan st = doAsyncWith st $
  case F.find (\ p -> isJust (p^.mPostId)) (Seq.reverse (st^.csChannel(chan).ccContents.cdMessages)) of
    Just (Message { _mPostId = Just pId }) -> do
      posts <- mmGetPostsAfter (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) chan pId 0 100
      return $ \ st' -> do
        res <- F.foldrM addMessage st' [ (posts^.postsPostsL) HM.! p
                                       | p <- F.toList (posts^.postsOrderL)
                                       ]
        return (res & csChannel(chan).ccInfo.cdCurrentState .~ ChanLoaded)
    _ -> return return

-- | Find all the loaded channels and refresh their state, setting the state as dirty
-- until we get a response
refreshLoadedChannels :: ChatState -> EventM Name ChatState
refreshLoadedChannels st = do
  liftIO $ sequence_
    [ refreshChannel cId st
    | (cId, chan) <- HM.toList (st^.msgMap)
    , chan^.ccInfo.cdCurrentState == ChanLoaded
    ]
  let upd ChanLoaded = ChanRefreshing
      upd chanState  = chanState
  return (st & msgMap.each.ccInfo.cdCurrentState %~ upd)

-- * Joining, Leaving, and Inviting

startJoinChannel :: ChatState -> EventM Name ChatState
startJoinChannel st = do
    liftIO $ doAsyncWith st $ do
        MoreChannels chans <- mmGetMoreChannels (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL)
        return $ \ st' -> do
            return $ st' & csJoinChannelList .~ (Just $ list JoinChannelList (V.fromList $ F.toList chans) 1)

    return $ st & csMode .~ JoinChannel
                & csJoinChannelList .~ Nothing

joinChannel :: Channel -> ChatState -> EventM Name ChatState
joinChannel chan st = do
    let cId = getId chan

    liftIO $ doAsyncWith st $ do
        void $ mmJoinChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId
        return return

    return $ st & csMode .~ Main

-- | When another user adds us to a channel, we need to fetch the
-- channel info for that channel.
handleChannelInvite :: ChannelId -> ChatState -> EventM Name ChatState
handleChannelInvite cId st = do
    liftIO $ doAsyncWith st $ do
        tryMM (mmGetChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId)
              (\chan -> do
                return $ \st' -> do
                  st'' <- handleNewChannel (chan^.channelNameL) False chan st'
                  liftIO $ asyncFetchScrollback st'' cId
                  return st'')
    return st

startLeaveCurrentChannel :: ChatState -> EventM Name ChatState
startLeaveCurrentChannel st = do
    let cName = st^.csCurrentChannel.ccInfo.cdName
    case cName `elem` st^.csNames.cnDMs of
        True -> postErrorMessage "The /leave command cannot be used with direct message channels." st
        False -> return $ st & csMode .~ LeaveChannelConfirm

leaveCurrentChannel :: ChatState -> EventM Name ChatState
leaveCurrentChannel st = do
    let cId = st^.csCurrentChannelId
        cName = st^.csCurrentChannel.ccInfo.cdName
    -- Leave a normal channel.  If this is a DM channel, do nothing.
    case cName `elem` st^.csNames.cnDMs of
        True -> return st
        False -> do
            -- Issue API call to leave. Once that's done, clean up our state:
            liftIO $ doAsyncWith st $ do
                mmLeaveChannel (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId
                return $ \ st' -> do
                    let st'' = st' & csEditState.cedInputHistoryPosition       .at cId .~ Nothing
                                   & csEditState.cedLastChannelInput           .at cId .~ Nothing
                                   & csEditState.cedInputHistory               %~ removeChannelHistory cId
                                     -- Update input history
                                   & csNames.cnToChanId                        .at cName .~ Nothing
                                     -- Flush cnToChanId
                                   & csNames.cnChans                           %~ filter (/= cName)
                                     -- Flush cnChans
                                   & msgMap                                    .at cId .~ Nothing
                                     -- Update msgMap
                                   & csFocus                                   %~ Z.filterZipper (/= cId)
                                     -- Remove from focus zipper
                    return st''
            return st

-- *  Channel Updates and Notifications

hasUnread :: ChatState -> ChannelId -> Bool
hasUnread st cId = maybe False id $ do
  chan <- st^.msgMap.at(cId)
  let u = chan^.ccInfo.cdViewed
      v = chan^.ccInfo.cdUpdated
  return (v > u)

setLastViewedFor :: ChatState -> ChannelId -> EventM Name ChatState
setLastViewedFor st cId = do
  now <- liftIO getCurrentTime
  return (st & csChannel(cId).ccInfo.cdViewed .~ now)

updateViewed :: ChatState -> EventM Name ChatState
updateViewed st = liftIO (updateViewedIO st)

resetHistoryPosition :: ChatState -> EventM a ChatState
resetHistoryPosition st =
    let cId = st^.csCurrentChannelId
    in return $ st & csInputHistoryPosition.at cId .~ Just Nothing

updateStatus :: UserId -> T.Text -> ChatState -> EventM a ChatState
updateStatus uId t st =
  return (st & usrMap.ix(uId).uiStatus .~ statusFromText t)

clearEditor :: ChatState -> ChatState
clearEditor = cmdLine %~ applyEdit clearZipper

loadLastEdit :: ChatState -> ChatState
loadLastEdit st =
    let cId = st^.csCurrentChannelId
    in case st^.csLastChannelInput.at cId of
        Nothing -> st
        Just lastEdit -> st & cmdLine %~ (applyEdit $ insertMany (lastEdit) . clearZipper)

saveCurrentEdit :: ChatState -> ChatState
saveCurrentEdit st =
    let cId = st^.csCurrentChannelId
    in st & csLastChannelInput.at cId .~
      Just (T.intercalate "\n" $ getEditContents $ st^.cmdLine)

changeChannelCommon :: ChatState -> EventM Name ChatState
changeChannelCommon st =
    loadLastEdit <$>
    ((return . clearEditor) =<<
     fetchCurrentScrollback =<<
     resetHistoryPosition st)

preChangeChannelCommon :: ChatState -> EventM Name ChatState
preChangeChannelCommon st = do
    let cId = st^.csCurrentChannelId
    clearNewMessageCutoff cId $
        saveCurrentEdit $
        st & csRecentChannel .~ Just cId

nextChannel :: ChatState -> EventM Name ChatState
nextChannel st =
    setFocusWith st (getNextChannel st Z.right)

prevChannel :: ChatState -> EventM Name ChatState
prevChannel st =
    setFocusWith st (getNextChannel st Z.left)

recentChannel :: ChatState -> EventM Name ChatState
recentChannel st = case st ^. csRecentChannel of
  Nothing  -> return st
  Just cId -> setFocus cId st

nextUnreadChannel :: ChatState -> EventM Name ChatState
nextUnreadChannel st =
    setFocusWith st (getNextUnreadChannel st)

getNextChannel :: ChatState
               -> (Zipper ChannelId -> Zipper ChannelId)
               -> (Zipper ChannelId -> Zipper ChannelId)
getNextChannel st shift z = go (shift z)
  where go z'
          | (st^?msgMap.ix(Z.focus z').ccInfo.cdType) /= Just Direct = z'
          | otherwise = go (shift z')

getNextUnreadChannel :: ChatState
                     -> (Zipper ChannelId -> Zipper ChannelId)
getNextUnreadChannel st = Z.findRight (hasUnread st)

listThemes :: ChatState -> EventM Name ChatState
listThemes cs = do
    let mkThemeList _ = T.intercalate "\n\n" $
                        "Available built-in themes:" :
                        (("  " <>) <$> fst <$> themes)
    msg <- newClientMessage Informative (mkThemeList themes)
    addClientMessage msg cs

setTheme :: ChatState -> T.Text -> EventM Name ChatState
setTheme cs name =
    case lookup name themes of
        Nothing -> listThemes cs
        Just t -> return $ cs & csTheme .~ t

channelPageUp :: ChatState -> EventM Name ChatState
channelPageUp st = do
  let cId = st^.csCurrentChannelId
  vScrollBy (viewportScroll (ChannelMessages cId)) (-1 * pageAmount)
  return st

channelPageDown :: ChatState -> EventM Name ChatState
channelPageDown st = do
  let cId = st^.csCurrentChannelId
  vScrollBy (viewportScroll (ChannelMessages cId)) pageAmount
  return st

asyncFetchMoreMessages :: ChatState -> ChannelId -> IO ()
asyncFetchMoreMessages st cId =
    doAsyncWith st $ do
        let offset = length $ st^.csChannel(cId).ccContents.cdMessages
            numToFetch = 10
        posts <- mmGetPosts (st^.csConn) (st^.csTok) (st^.csMyTeam.teamIdL) cId (offset - 1) numToFetch
        return $ \st' -> do
            let cc = fromPosts st' posts
            invalidateCacheEntry (ChannelMessages $ st^.csCurrentChannelId)
            return $ st' & csChannel(cId).ccContents.cdMessages %~ (cc^.cdMessages Seq.><)

loadMoreMessages :: ChatState -> EventM Name ChatState
loadMoreMessages st = do
    case st^.csMode of
        ChannelScroll -> do
            liftIO $ asyncFetchMoreMessages st (st^.csCurrentChannelId)
        _ -> return ()
    return st

channelByName :: ChatState -> T.Text -> Maybe ChannelId
channelByName st n
    | "#" `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | "@" `T.isPrefixOf` n = st ^. csNames . cnToChanId . at (T.tail n)
    | otherwise            = st ^. csNames . cnToChanId . at n

-- | This switches to the named channel or creates it if it is a missing
-- but valid user channel.
changeChannel :: T.Text -> ChatState -> EventM Name ChatState
changeChannel name st =
    case channelByName st name of
      Just cId -> setFocus cId st
      Nothing -> attemptCreateDMChannel name st

setFocus :: ChannelId -> ChatState -> EventM Name ChatState
setFocus cId st = setFocusWith st (Z.findRight (== cId))

setFocusWith :: ChatState -> (Zipper ChannelId -> Zipper ChannelId) -> EventM Name ChatState
setFocusWith st f = do
    let newZipper = f oldZipper
        oldZipper = st^.csFocus
        newFocus = Z.focus newZipper
        oldFocus = Z.focus oldZipper

    -- If we aren't changing anything, skip all the book-keeping because
    -- we'll end up clobbering things like csRecentChannel.
    if (newFocus == oldFocus) then
        return st else do
          preChangeChannelCommon st >>=
              (\st' -> updateViewed (st' & csFocus .~ newZipper)) >>=
              changeChannelCommon

attemptCreateDMChannel :: T.Text -> ChatState -> EventM Name ChatState
attemptCreateDMChannel name st
  | name `elem` (st^.csNames.cnUsers) &&
    not (name `HM.member` (st^.csNames.cnToChanId)) = do
      -- We have a user of that name but no channel. Time to make one!
      let tId = st^.csMyTeam.teamIdL
          Just uId = st^.csNames.cnToUserId.at(name)
      liftIO $ doAsyncWith st $ do
        -- create a new channel
        nc <- mmCreateDirect (st^.csConn) (st^.csTok) tId uId
        return $ handleNewChannel name True nc
      return st
  | otherwise = do
    postErrorMessage ("No channel or user named " <> name) st

createOrdinaryChannel :: T.Text -> ChatState -> EventM Name ChatState
createOrdinaryChannel name st = do
  let tId = st^.csMyTeam.teamIdL
  liftIO $ doAsyncWith st $ do
    -- create a new chat channel
    let slug = T.map (\ c -> if isAlphaNum c then c else '-') (T.toLower name)
        minChannel = MinChannel
          { minChannelName        = slug
          , minChannelDisplayName = name
          , minChannelPurpose     = Nothing
          , minChannelHeader      = Nothing
          , minChannelType        = Ordinary
          }
    tryMM (mmCreateChannel (st^.csConn) (st^.csTok) tId minChannel)
          (return . handleNewChannel name True)
  return st

handleNewChannel :: T.Text -> Bool -> Channel -> ChatState -> EventM Name ChatState
handleNewChannel name switch nc st = do
  -- time to do a lot of state updating:
  -- create a new ClientChannel structure
  now <- liftIO getCurrentTime
  let cChannel = ClientChannel
        { _ccContents = emptyChannelContents
        , _ccInfo     = ChannelInfo
                          { _cdViewed           = now
                          , _cdUpdated          = now
                          , _cdName             = nc^.channelNameL
                          , _cdHeader           = nc^.channelHeaderL
                          , _cdType             = nc^.channelTypeL
                          , _cdCurrentState     = ChanLoaded
                          , _cdNewMessageCutoff = Nothing
                          }
        }
      -- add it to the message map, and to the map so we can look
      -- it up by user name
      st' = st & csNames.cnToChanId.at(name) .~ Just (getId nc)
               & (if nc^.channelTypeL == Direct
                  then id -- For direct channels the username is already
                          -- in the user list so do nothing
                  else csNames.cnChans %~ (sort . (name:)))
               & msgMap.at(getId nc) .~ Just cChannel
      -- we should figure out how to do this better: this adds it to
      -- the channel zipper in such a way that we don't ever change
      -- our focus to something else, which is kind of silly
      newZip = Z.updateList (mkChannelZipperList (st'^.csNames))
      st'' = st' & csFocus %~ newZip
          -- and we finally set our focus to the newly created channel
  if switch then setFocus (getId nc) st'' else return st''

editMessage :: Post -> ChatState -> EventM Name ChatState
editMessage new st = do
  now <- liftIO getCurrentTime
  let chan = csChannel (postChannelId new)
      isEditedMessage m = m^.mPostId == Just (new^.postIdL)
      msg = clientPostToMessage st (toClientPost new Nothing)
      rs = st & chan . ccContents . cdMessages . each . filtered isEditedMessage .~ msg
              & chan . ccInfo . cdUpdated .~ now
  if postChannelId new == rs^.csCurrentChannelId
    then updateViewed rs
    else return rs

deleteMessage :: Post -> ChatState -> EventM Name ChatState
deleteMessage new st = do
  now <- liftIO getCurrentTime
  let isDeletedMessage m = m^.mPostId == Just (new^.postIdL)
      chan = csChannel (postChannelId new)
      rs = st & chan . ccContents . cdMessages . each . filtered isDeletedMessage %~ (& mDeleted .~ True)
              & chan . ccInfo . cdUpdated .~ now
  if postChannelId new == rs^.csCurrentChannelId
    then updateViewed rs
    else return rs

maybeRingBell :: ChatState -> EventM Name ()
maybeRingBell st = do
    when (configActivityBell $ st^.csResources.crConfiguration) $ do
        -- This is safe because we only get Nothing in appStartEvent.
        Just vty <- getVtyHandle
        liftIO $ ringTerminalBell $ outputIface vty

addMessage :: Post -> ChatState -> EventM Name ChatState
addMessage new st = do
  case st^.msgMap.at (postChannelId new) of
      Nothing ->
          -- When we join channels, sometimes we get the "user has
          -- been added to channel" message here BEFORE we get the
          -- websocket event that says we got added to a channel. This
          -- means the message arriving here in addMessage can't be
          -- added yet because we haven't fetched the channel metadata
          -- in the websocket handler. So to be safe we just drop the
          -- message here, but this is the only case of messages that we
          -- /expect/ to drop for this reason. Hence the check for the
          -- msgMap channel ID key presence above.
          return st
      Just _ -> do
          now <- liftIO getCurrentTime
          let cp = toClientPost new (new^.postParentIdL)
              fromMe = (cp^.cpUser == (Just $ getId (st^.csMe))) &&
                       (isNothing $ cp^.cpUserOverride)
              updateTime = if fromMe then id else const now
              msg = clientPostToMessage st cp
              cId = postChannelId new

              doAddMessage s = do
                let chan = msgMap . ix cId
                    s' = s & csPostMap.ix(postId new) .~ msg
                    msg' = clientPostToMessage s (toClientPost new (new^.postParentIdL))
                    rs = s' & chan . ccContents . cdMessages %~ (Seq.|> msg')
                            & chan . ccInfo . cdUpdated %~ updateTime
                when (not fromMe) $ maybeRingBell s'
                if postChannelId new == rs^.csCurrentChannelId
                  then updateViewed rs
                  else setNewMessageCutoff rs cId msg

          -- If the message is in reply to another message, try to find it in
          -- the scrollback for the post's channel. If the message isn't there,
          -- fetch it. If we have to fetch it, don't post this message to the
          -- channel until we have fetched the parent.
          case msg^.mInReplyToMsg of
              ParentNotLoaded parentId -> do
                  liftIO $ doAsyncWith st $ do
                      let theTeamId = st^.csMyTeam.teamIdL
                      p <- mmGetPost (st^.csConn) (st^.csTok) theTeamId cId parentId
                      let postMap = HM.fromList [ ( pId
                                                  , clientPostToMessage st (toClientPost x (x^.postParentIdL))
                                                  )
                                                | (pId, x) <- HM.toList (p^.postsPostsL)
                                                ]
                      return $ \st'' -> doAddMessage $ st'' & csPostMap %~ (HM.union postMap)
                  return st
              _ -> doAddMessage st

setNewMessageCutoff :: ChatState -> ChannelId -> Message -> EventM Name ChatState
setNewMessageCutoff st cId msg =
    return $ st & csChannel(cId).ccInfo.cdNewMessageCutoff %~ (<|> Just (msg^.mDate))

clearNewMessageCutoff :: ChannelId -> ChatState -> EventM Name ChatState
clearNewMessageCutoff cId st = do
    return $ st & csChannel(cId).ccInfo.cdNewMessageCutoff .~ Nothing

getNewMessageCutoff :: ChannelId -> ChatState -> Maybe UTCTime
getNewMessageCutoff cId st = do
    cc <- st^.msgMap.at cId
    cc^.ccInfo.cdNewMessageCutoff


mmServerCommandWhitelist :: [T.Text]
mmServerCommandWhitelist =
    [ "me"
    ]

execMMCommand :: T.Text -> ChatState -> EventM Name ChatState
execMMCommand cmd st =
    case T.words cmd of
        (n:_) -> case n `elem` mmServerCommandWhitelist of
            False -> postErrorMessage ("Unknown command: " <> n) st
            True -> liftIO (runCmd `catch` handler)
        _ -> postErrorMessage ("Invalid command: " <> cmd) st
  where
  mc = MinCommand
        { minComChannelId = st^.csCurrentChannelId
        , minComCommand   = "/" <> cmd
        , minComSuggest   = False
        }
  runCmd = do
    _ <- mmExecute
      (st^.csConn)
      (st^.csTok)
      (st^.csMyTeam.teamIdL)
      mc
    return st
  handler (HTTPResponseException err) = do
    postErrorMessage ("Error running command: " <> (T.pack err)) st

fetchCurrentScrollback :: ChatState -> EventM a ChatState
fetchCurrentScrollback st = do
  let cId = st^.csCurrentChannelId
  when (maybe False (/= ChanLoaded) (st^?msgMap.ix(cId).ccInfo.cdCurrentState)) $
      liftIO $ asyncFetchScrollback st cId
  return st

mkChannelZipperList :: MMNames -> [ChannelId]
mkChannelZipperList chanNames =
  [ (chanNames ^. cnToChanId) HM.! i
  | i <- chanNames ^. cnChans ] ++
  [ c
  | i <- chanNames ^. cnUsers
  , c <- maybeToList (HM.lookup i (chanNames ^. cnToChanId)) ]

setChannelTopic :: ChatState -> T.Text -> IO ()
setChannelTopic st msg = do
    let chanId = st^.csCurrentChannelId
        theTeamId = st^.csMyTeam.teamIdL
    doAsyncWith st $ do
        void $ mmSetChannelHeader (st^.csConn) (st^.csTok) theTeamId chanId msg
        return $ \st' -> do
            return $ st' & msgMap.at chanId.each.ccInfo.cdHeader .~ msg

channelHistoryForward :: ChatState -> ChatState
channelHistoryForward st =
  let cId = st^.csCurrentChannelId
  in case st^.csInputHistoryPosition.at cId of
      Just (Just i)
        | i == 0 ->
          -- Transition out of history navigation
          loadLastEdit $ st & csInputHistoryPosition.at cId .~ Just Nothing
        | otherwise ->
          let Just entry = getHistoryEntry cId newI (st^.csInputHistory)
              newI = i - 1
              eLines = T.lines entry
              mv = if length eLines == 1 then gotoEOL else id
          in st & cmdLine.editContentsL .~ (mv $ textZipper eLines Nothing)
                & csInputHistoryPosition.at cId .~ (Just $ Just newI)
      _ -> st

channelHistoryBackward :: ChatState -> ChatState
channelHistoryBackward st =
  let cId = st^.csCurrentChannelId
  in case st^.csInputHistoryPosition.at cId of
      Just (Just i) ->
          let newI = i + 1
          in case getHistoryEntry cId newI (st^.csInputHistory) of
              Nothing -> st
              Just entry ->
                  let eLines = T.lines entry
                      mv = if length eLines == 1 then gotoEOL else id
                  in st & cmdLine.editContentsL .~ (mv $ textZipper eLines Nothing)
                        & csInputHistoryPosition.at cId .~ (Just $ Just newI)
      _ ->
          let newI = 0
          in case getHistoryEntry cId newI (st^.csInputHistory) of
              Nothing -> st
              Just entry ->
                  let eLines = T.lines entry
                      mv = if length eLines == 1 then gotoEOL else id
                  in (saveCurrentEdit st)
                         & cmdLine.editContentsL .~ (mv $ textZipper eLines Nothing)
                         & csInputHistoryPosition.at cId .~ (Just $ Just newI)

showHelpScreen :: ChatState -> EventM Name ChatState
showHelpScreen st = do
    vScrollToBeginning (viewportScroll HelpViewport)
    return $ st & csMode .~ ShowHelp

beginChannelSelect :: ChatState -> ChatState
beginChannelSelect st =
    st & csMode                        .~ ChannelSelect
       & csChannelSelectString         .~ ""
       & csChannelSelectChannelMatches .~ mempty
       & csChannelSelectUserMatches    .~ mempty

updateChannelSelectMatches :: ChatState -> ChatState
updateChannelSelectMatches st =
    -- Given the current channel select string, find all the channel and
    -- user matches and then update the match lists.
    let chanNameMatches = channelNameMatch (st^.csChannelSelectString)
        chanMatches = catMaybes $ chanNameMatches <$> st^.csNames.cnChans
        userMatches = catMaybes $ chanNameMatches <$> st^.csNames.cnUsers
        mkMap ms = HM.fromList [(channelNameFromMatch m, m) | m <- ms]
    in st & csChannelSelectChannelMatches .~ mkMap chanMatches
          & csChannelSelectUserMatches    .~ mkMap userMatches

channelNameMatch :: T.Text -> T.Text -> Maybe ChannelSelectMatch
channelNameMatch patStr chanName =
    if T.null patStr
    then Nothing
    else do
        pat <- parseChannelSelectPattern patStr
        applySelectPattern pat chanName

applySelectPattern :: ChannelSelectPattern -> T.Text -> Maybe ChannelSelectMatch
applySelectPattern (CSP ty pat) chanName = do
    let applyType Infix  | pat `T.isInfixOf`  chanName =
            case T.breakOn pat chanName of
                (pre, post) -> return (pre, pat, T.drop (T.length pat) post)

        applyType Prefix | pat `T.isPrefixOf` chanName = do
            let (b, a) = T.splitAt (T.length pat) chanName
            return ("", b, a)

        applyType Suffix | pat `T.isSuffixOf` chanName = do
            let (b, a) = T.splitAt (T.length chanName - T.length pat) chanName
            return (b, a, "")

        applyType Equal  | pat == chanName =
            return ("", chanName, "")

        applyType _ = Nothing

    (pre, m, post) <- applyType ty
    return $ ChannelSelectMatch pre m post

parseChannelSelectPattern :: T.Text -> Maybe ChannelSelectPattern
parseChannelSelectPattern pat = do
    (pat1, pfx) <- case "^" `T.isPrefixOf` pat of
        True  -> return (T.tail pat, Just Prefix)
        False -> return (pat, Nothing)

    (pat2, sfx) <- case "$" `T.isSuffixOf` pat1 of
        True  -> return (T.init pat1, Just Suffix)
        False -> return (pat1, Nothing)

    case (pfx, sfx) of
        (Nothing, Nothing)         -> return $ CSP Infix  pat2
        (Just Prefix, Nothing)     -> return $ CSP Prefix pat2
        (Nothing, Just Suffix)     -> return $ CSP Suffix pat2
        (Just Prefix, Just Suffix) -> return $ CSP Equal  pat2
        tys                        -> error $ "BUG: invalid channel select case: " <> show tys

startUrlSelect :: ChatState -> ChatState
startUrlSelect st =
    let urls = V.fromList $ findUrls (st^.csCurrentChannel)
    in st & csMode .~ UrlSelect
          & csUrlList .~ (listMoveTo (length urls - 1) $ list UrlList urls 2)

stopUrlSelect :: ChatState -> ChatState
stopUrlSelect = csMode .~ Main

findUrls :: ClientChannel -> [LinkChoice]
findUrls chan =
    let msgs = chan^.ccContents.cdMessages
    in removeDuplicates $ concat $ F.toList $ F.toList <$> Seq.reverse <$> msgURLs <$> msgs

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = snd . go Set.empty
  where go before [] = (before, [])
        go before (x:xs) =
          let (before', xs') = go before xs in
          if (x^.linkURL) `Set.member` before'
            then (before', xs')
            else (Set.insert (x^.linkURL) before', x : xs')

msgURLs :: Message -> Seq.Seq LinkChoice
msgURLs msg | Just uname <- msg^.mUserName =
  let msgUrls = (\ url -> LinkChoice (msg^.mDate) uname url url) <$>
                  (mconcat $ blockGetURLs <$> (F.toList $ msg^.mText))
      attachmentURLs = (\ a ->
                          LinkChoice
                            (msg^.mDate)
                            uname
                            ("attachment `" <> (a^.attachmentName) <> "`")
                            (a^.attachmentURL))
                       <$> (msg^.mAttachments)
  in msgUrls <> attachmentURLs
msgURLs _ = mempty

openSelectedURL :: ChatState -> EventM Name ChatState
openSelectedURL st | st^.csMode == UrlSelect =
    case configURLOpenCommand $ st^.csResources.crConfiguration of
        Nothing -> do
            msg <- newClientMessage Informative "Config option 'urlOpenCommand' missing; cannot open URL."
            addClientMessage msg $ st & csMode .~ Main
        Just urlOpenCommand -> do
            case listSelectedElement $ st^.csUrlList of
                Nothing -> return ()
                Just (_, link) ->
                    liftIO $ void $ system $ (T.unpack urlOpenCommand) <> " " <> show (link^.linkURL)
            return $ st & csMode .~ Main
openSelectedURL st = return st
