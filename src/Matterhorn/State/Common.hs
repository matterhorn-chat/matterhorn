module Matterhorn.State.Common
  (
  -- * System interface
    openFilePath
  , openWithOpener
  , runLoggedCommand
  , fetchFile
  , fetchFileAtPath

  -- * Posts
  , installMessagesFromPosts
  , updatePostMap

  -- * Utilities
  , postInfoMessage
  , postErrorMessageIO
  , postErrorMessage'
  , addEmoteFormatting
  , removeEmoteFormatting
  , toggleMouseMode

  , fetchMentionedUsers
  , doPendingUserFetches
  , doPendingUserStatusFetches

  , setThreadOrientationByName

  -- Cache management
  , invalidateChannelRenderingCache
  , invalidateMessageRenderingCacheByPostId

  , module Matterhorn.State.Async
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( invalidateCacheEntry, invalidateCache, getVtyHandle )
import           Control.Concurrent ( MVar, putMVar, forkIO )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( SomeException, try )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (.=), (%=), (%~), (.~) )
import           System.Directory ( createDirectoryIfMissing )
import           System.Environment.XDG.BaseDir ( getUserCacheDir )
import           System.Exit ( ExitCode(..) )
import           System.FilePath
import           System.IO ( hGetContents, hFlush )
import           System.Process ( proc, std_in, std_out, std_err, StdStream(..)
                                , createProcess, waitForProcess )

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           Matterhorn.FilePaths ( xdgName )
import           Matterhorn.State.Async
import           Matterhorn.Types
import           Matterhorn.Types.Common


-- * Client Messages

-- | Given a collection of posts from the server, save the posts in the
-- global post map. Also convert the posts to Matterhorn's Message type
-- and return them along with the set of all usernames mentioned in the
-- text of the resulting messages.
--
-- This also sets the mFlagged field of each message based on whether
-- its post ID is a flagged post according to crFlaggedPosts at the time
-- of this call.
installMessagesFromPosts :: Maybe TeamId -> Posts -> MH Messages
installMessagesFromPosts mTId postCollection = do
  flags <- use (csResources.crFlaggedPosts)

  -- Add all posts in this collection to the global post cache
  updatePostMap mTId postCollection

  mBaseUrl <- case mTId of
      Nothing -> return Nothing
      Just tId -> Just <$> getServerBaseUrl tId

  hostname <- use (csResources.crConn.cdHostnameL)

  -- Build the ordered list of posts. Note that postsOrder lists the
  -- posts most recent first, but we want most recent last.
  let postsInOrder = findPost <$> (Seq.reverse $ postsOrder postCollection)
      mkClientPost p = toClientPost hostname mBaseUrl mTId p (postId <$> parent p)
      clientPosts = mkClientPost <$> postsInOrder

      addNext cp (msgs, us) =
          let (msg, mUsernames) = clientPostToMessage cp
          in (addMessage (maybeFlag flags msg) msgs, Set.union us mUsernames)
      (ms, mentions) = foldr addNext (noMessages, mempty) clientPosts

  fetchMentionedUsers mentions
  return ms
    where
        maybeFlag flagSet msg
          | Just (MessagePostId pId) <- msg^.mMessageId, pId `Set.member` flagSet
            = msg & mFlagged .~ True
          | otherwise = msg
        parent x = do
            parentId <- x^.postRootIdL
            HM.lookup parentId (postCollection^.postsPostsL)
        findPost pId = case HM.lookup pId (postsPosts postCollection) of
            Nothing -> error $ "BUG: could not find post for post ID " <> show pId
            Just post -> post

-- Add all posts in this collection to the global post cache
updatePostMap :: Maybe TeamId -> Posts -> MH ()
updatePostMap mTId postCollection = do
  -- Build a map from post ID to Matterhorn message, then add the new
  -- messages to the global post map. We use the "postsPosts" field for
  -- this because that might contain more messages than the "postsOrder"
  -- list, since the former can contain other messages in threads that
  -- the server sent us, even if those messages are not part of the
  -- ordered post listing of "postsOrder."
  mBaseUrl <- case mTId of
      Nothing -> return Nothing
      Just tId -> Just <$> getServerBaseUrl tId

  hostname <- use (csResources.crConn.cdHostnameL)

  let postMap = HM.fromList
          [ ( pId
            , fst $ clientPostToMessage (toClientPost hostname mBaseUrl mTId x Nothing)
            )
          | (pId, x) <- HM.toList (postCollection^.postsPostsL)
          ]
  csPostMap %= HM.union postMap

-- | Add a 'ClientMessage' to the current channel's message list
addClientMessage :: ClientMessage -> MH ()
addClientMessage msg = do
    withCurrentTeam $ \tId -> do
        withCurrentChannel tId $ \cid _ -> do
            uuid <- generateUUID
            let addCMsg = ccMessageInterface.miMessages %~
                    (addMessage $ clientMessageToMessage msg & mMessageId .~ Just (MessageUUID uuid))
            csChannels %= modifyChannelById cid addCMsg

            invalidateChannelRenderingCache cid
            mh $ invalidateCacheEntry $ ChannelSidebar tId

            let msgTy = case msg^.cmType of
                  Error -> LogError
                  _     -> LogGeneral

            mhLog msgTy $ T.pack $ show msg

-- | Add a new 'ClientMessage' representing an error message to
--   the current channel's message list
postInfoMessage :: Text -> MH ()
postInfoMessage info =
    addClientMessage =<< newClientMessage Informative (sanitizeUserText' info)

-- | Add a new 'ClientMessage' representing an error message to
--   the current channel's message list
postErrorMessage' :: Text -> MH ()
postErrorMessage' err =
    addClientMessage =<< newClientMessage Error (sanitizeUserText' err)

postErrorMessageIO :: Text -> ChatState -> IO ChatState
postErrorMessageIO err st = do
  case st^.csCurrentTeamId of
      Nothing -> return st
      Just tId -> do
          case st^.csCurrentChannelId(tId) of
              Nothing -> return st
              Just cId -> do
                  msg <- newClientMessage Error err
                  uuid <- generateUUID_IO
                  let addEMsg = ccMessageInterface.miMessages %~
                          (addMessage $ clientMessageToMessage msg & mMessageId .~ Just (MessageUUID uuid))
                  return $ st & csChannels %~ modifyChannelById cId addEMsg

openFilePath :: FilePath -> MH ()
openFilePath path = openWithOpener (return $ Right path)

openWithOpener :: IO (Either MHError String) -> MH ()
openWithOpener getTarget = do
    cfg <- use (csResources.crConfiguration)
    case configURLOpenCommand cfg of
        Nothing ->
            mhError $ ConfigOptionMissing "urlOpenCommand"
        Just urlOpenCommand -> do
            doAsyncWith Preempt $ do
                targetResult <- getTarget
                let cmdWords = T.words urlOpenCommand
                    (cmds, args) = splitAt 1 (T.unpack <$> cmdWords)
                    cmd = if null cmds then "$BROWSER" else head cmds
                case targetResult of
                    Left e -> return $ Just $ Work "openWithOpener[1]" $ do
                        mhError e
                    Right target -> return $ Just $ Work "openWithOpener[2]" $ do
                        -- Is the URL-opening command interactive? If
                        -- so, pause Matterhorn and run the opener
                        -- interactively. Otherwise run the opener
                        -- asynchronously and continue running Matterhorn
                        -- interactively.
                        case configURLOpenCommandInteractive cfg of
                            False -> do
                                outputChan <- use (csResources.crSubprocessLog)
                                doAsyncWith Preempt $ do
                                    runLoggedCommand outputChan cmd
                                                     (args <> [target])
                                                     Nothing Nothing
                                    return Nothing
                            True -> do
                                -- If there isn't a new message cutoff
                                -- showing in the current channel, set one.
                                -- This way, while the user is gone using
                                -- their interactive URL opener, when they
                                -- return, any messages that arrive in the
                                -- current channel will be displayed as new.
                                withCurrentTeam $ \tId -> do
                                    withCurrentChannel tId $ \cId curChan -> do
                                        let msgs = curChan^.ccMessageInterface.miMessages
                                        case findLatestUserMessage isEditable msgs of
                                            Nothing -> return ()
                                            Just m ->
                                                case m^.mOriginalPost of
                                                    Nothing -> return ()
                                                    Just p ->
                                                        case curChan^.ccInfo.cdNewMessageIndicator of
                                                            Hide -> csChannel(cId).ccInfo.cdNewMessageIndicator .= (NewPostsAfterServerTime (p^.postCreateAtL))
                                                            _ -> return ()

                                -- No need to add a gap here: the websocket
                                -- disconnect/reconnect events will automatically
                                -- handle management of messages delivered while
                                -- suspended.
                                mhSuspendAndResume $ \st -> do
                                    result <- runInteractiveCommand cmd (args <> [target])

                                    let waitForKeypress = do
                                            putStrLn "Press any key to return to Matterhorn."
                                            void getChar

                                    case result of
                                        Right ExitSuccess -> return ()
                                        Left err -> do
                                            putStrLn $ "URL opener subprocess " <> (show urlOpenCommand) <>
                                                       " could not be run: " <> err
                                            waitForKeypress
                                        Right (ExitFailure code) -> do
                                            putStrLn $ "URL opener subprocess " <> (show urlOpenCommand) <>
                                                       " exited with non-zero status " <> show code
                                            waitForKeypress

                                    return $ case st^.csCurrentTeamId of
                                        Nothing -> st
                                        Just tId -> pushMode' tId Main st

runInteractiveCommand :: String
                      -> [String]
                      -> IO (Either String ExitCode)
runInteractiveCommand cmd args = do
    let opener = (proc cmd args) { std_in = Inherit
                                 , std_out = Inherit
                                 , std_err = Inherit
                                 }
    result <- try $ createProcess opener
    case result of
        Left (e::SomeException) -> return $ Left $ show e
        Right (_, _, _, ph) -> do
            ec <- waitForProcess ph
            return $ Right ec

runLoggedCommand :: STM.TChan ProgramOutput
                 -- ^ The output channel to send the output to
                 -> String
                 -- ^ The program name
                 -> [String]
                 -- ^ Arguments
                 -> Maybe BSL.ByteString
                 -- ^ The stdin to send, if any
                 -> Maybe (MVar ProgramOutput)
                 -- ^ Where to put the program output when it is ready
                 -> IO ()
runLoggedCommand outputChan cmd args mInput mOutputVar = void $ forkIO $ do
    let stdIn = maybe NoStream (const CreatePipe) mInput
        opener = (proc cmd args) { std_in = stdIn
                                 , std_out = CreatePipe
                                 , std_err = CreatePipe
                                 }
    result <- try $ createProcess opener
    case result of
        Left (e::SomeException) -> do
            let po = ProgramOutput cmd args "" (show e) (ExitFailure 1)
            STM.atomically $ STM.writeTChan outputChan po
            maybe (return ()) (flip putMVar po) mOutputVar
        Right (stdinResult, Just outh, Just errh, ph) -> do
            case stdinResult of
                Just inh -> do
                    case mInput of
                        Just input -> do
                            BSL.hPut inh input
                            hFlush inh
                        Nothing -> return ()
                Nothing -> return ()

            ec <- waitForProcess ph
            outResult <- hGetContents outh
            errResult <- hGetContents errh
            let po = ProgramOutput cmd args outResult errResult ec
            STM.atomically $ STM.writeTChan outputChan po
            maybe (return ()) (flip putMVar po) mOutputVar
        Right _ ->
            error $ "BUG: createProcess returned unexpected result, report this at " <>
                    "https://github.com/matterhorn-chat/matterhorn"

-- | Given a file ID and server session, fetch the file into a temporary
-- location and return its path. The caller is responsible for deleting
-- the file.
fetchFile :: FileId -> Session -> IO String
fetchFile fId sess = do
    -- The link is for an attachment, so fetch it and then
    -- open the local copy.
    info <- mmGetMetadataForFile fId sess
    cacheDir <- getUserCacheDir xdgName
    let dir = cacheDir </> "files" </> T.unpack (idString fId)
        filename = T.unpack (fileInfoName info)
        fullPath = dir </> filename

    fetchFileAtPath fId sess fullPath
    return fullPath

-- | Given a file ID and server session, fetch the file and save it to
-- the specified destination path. The destination path must refer to
-- the path to the file itself, not its parent directory. This function
-- will create only the parent directory in the specified path; it will
-- not create all path entries recursively. If the file already exists,
-- this function will overwrite the file.
--
-- The caller is responsible for catching all exceptions.
fetchFileAtPath :: FileId -> Session -> FilePath -> IO ()
fetchFileAtPath fId sess fullPath = do
    contents <- mmGetFile fId sess
    let dir = takeDirectory fullPath
    createDirectoryIfMissing True dir
    BS.writeFile fullPath contents

removeEmoteFormatting :: T.Text -> T.Text
removeEmoteFormatting t
    | "*" `T.isPrefixOf` t &&
      "*" `T.isSuffixOf` t = T.init $ T.drop 1 t
    | otherwise = t

addEmoteFormatting :: T.Text -> T.Text
addEmoteFormatting t = "*" <> t <> "*"

fetchMentionedUsers :: Set.Set MentionedUser -> MH ()
fetchMentionedUsers ms
    | Set.null ms = return ()
    | otherwise = do
        let convertMention (UsernameMention u) = UserFetchByUsername u
            convertMention (UserIdMention i) = UserFetchById i
        scheduleUserFetches $ convertMention <$> Set.toList ms

doPendingUserStatusFetches :: MH ()
doPendingUserStatusFetches = do
    mz <- getScheduledUserStatusFetches
    case mz of
        Nothing -> return ()
        Just z -> do
            statusChan <- use (csResources.crStatusUpdateChan)
            liftIO $ STM.atomically $ STM.writeTChan statusChan z

doPendingUserFetches :: MH ()
doPendingUserFetches = do
    fs <- getScheduledUserFetches

    let getUsername (UserFetchByUsername u) = Just u
        getUsername _ = Nothing

        getUserId (UserFetchById i) = Just i
        getUserId _ = Nothing

    fetchUsers (catMaybes $ getUsername <$> fs) (catMaybes $ getUserId <$> fs)

-- | Given a list of usernames, ensure that we have a user record for
-- each one in the state, either by confirming that a local record
-- exists or by issuing a request for user records.
fetchUsers :: [Text] -> [UserId] -> MH ()
fetchUsers rawUsernames uids = do
    st <- use id
    session <- getSession
    let usernames = trimUserSigil <$> rawUsernames
        missingUsernames = filter isMissing usernames
        isMissing n = and [ not $ T.null n
                          , not $ isSpecialMention n
                          , isNothing $ userByUsername n st
                          ]
        missingIds = filter (\i -> isNothing $ knownUserById i st) uids

    when (not $ null missingUsernames) $ do
        mhLog LogGeneral $ T.pack $ "fetchUsers: getting " <> show missingUsernames

    when (not $ null missingIds) $ do
        mhLog LogGeneral $ T.pack $ "fetchUsers: getting " <> show missingIds

    when ((not $ null missingUsernames) || (not $ null missingIds)) $ do
        doAsyncWith Normal $ do
            act1 <- case null missingUsernames of
                True -> return $ return ()
                False -> do
                    results <- mmGetUsersByUsernames (Seq.fromList missingUsernames) session
                    return $ do
                        forM_ results (\u -> addNewUser $ userInfoFromUser u True)

            act2 <- case null missingIds of
                True -> return $ return ()
                False -> do
                    results <- mmGetUsersByIds (Seq.fromList missingIds) session
                    return $ do
                        forM_ results (\u -> addNewUser $ userInfoFromUser u True)

            return $ Just $ Work "fetchUsers" $ act1 >> act2

invalidateChannelRenderingCache :: ChannelId -> MH ()
invalidateChannelRenderingCache cId = do
    mh $ invalidateCacheEntry $ MessageInterfaceMessages $ MessageInput cId
    mh $ invalidateCacheEntry $ MessageInterfaceMessages $ ThreadMessageInput cId

invalidateMessageRenderingCacheByPostId :: PostId -> MH ()
invalidateMessageRenderingCacheByPostId pId = do
    mh $ invalidateCacheEntry $ RenderedMessage $ MessagePostId pId

setThreadOrientationByName :: T.Text -> MH ()
setThreadOrientationByName o = do
    let o' = T.strip $ T.toLower o
    new <- case o' of
        "above" -> return $ Just ThreadAbove
        "below" -> return $ Just ThreadBelow
        "left"  -> return $ Just ThreadLeft
        "right" -> return $ Just ThreadRight
        _ -> do
            postErrorMessage' $ T.pack $ "Invalid orientation: " <> show o
            return Nothing

    case new of
        Nothing -> return ()
        Just n -> do
            csResources.crConfiguration.configThreadOrientationL .= n
            postInfoMessage $ "Thread orientation set to " <> o'
            mh invalidateCache

toggleMouseMode :: MH ()
toggleMouseMode = do
    vty <- mh getVtyHandle
    csResources.crConfiguration.configMouseModeL %= not
    newMode <- use (csResources.crConfiguration.configMouseModeL)
    liftIO $ Vty.setMode (Vty.outputIface vty) Vty.Mouse newMode
    postInfoMessage $ if newMode
                      then "Mouse input is now enabled."
                      else "Mouse input is now disabled."
