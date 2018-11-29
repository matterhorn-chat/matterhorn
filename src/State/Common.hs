module State.Common
  (
  -- * System interface
    openURL
  , runLoggedCommand

  -- * Posts
  , installMessagesFromPosts
  , updatePostMap

  -- * Utilities
  , postInfoMessage
  , postErrorMessageIO
  , postErrorMessage'
  , addEmoteFormatting
  , removeEmoteFormatting
  , fetchUsersByUsername

  , module State.Async
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( invalidateCacheEntry )
import           Control.Concurrent ( MVar, putMVar, forkIO )
import           Control.Concurrent.Async ( concurrently )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( SomeException, try )
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import           Lens.Micro.Platform ( (.=), (%=), (%~), (.~) )
import           System.Directory ( createDirectoryIfMissing )
import           System.Environment.XDG.BaseDir ( getUserCacheDir )
import           System.Exit ( ExitCode(..) )
import           System.FilePath
import           System.IO ( hGetContents, hFlush, hPutStrLn )
import           System.Process ( proc, std_in, std_out, std_err, StdStream(..)
                                , createProcess, waitForProcess )

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           FilePaths ( xdgName )
import           State.Async
import           Types
import           Types.Common


-- * Client Messages

-- | Given a collection of posts from the server, save the posts in the
-- global post map. Also convert the posts to Matterhorn's Message type
-- and return them along with the set of all usernames mentioned in the
-- text of the resulting messages.
--
-- This also sets the mFlagged field of each message based on whether
-- its post ID is a flagged post according to crFlaggedPosts at the time
-- of this call.
installMessagesFromPosts :: Posts -> MH (Messages, Set.Set Text)
installMessagesFromPosts postCollection = do
  flags <- use (csResources.crFlaggedPosts)

  -- Add all posts in this collection to the global post cache
  updatePostMap postCollection

  -- Build the ordered list of posts. Note that postsOrder lists the
  -- posts most recent first, but we want most recent last.
  let postsInOrder = findPost <$> (Seq.reverse $ postsOrder postCollection)
      mkClientPost p = toClientPost p (postId <$> parent p)
      clientPosts = mkClientPost <$> postsInOrder

      addNext cp (msgs, us) =
          let (msg, mUsernames) = clientPostToMessage cp
          in (addMessage (maybeFlag flags msg) msgs, Set.union us mUsernames)
      postsToMessages = foldr addNext (noMessages, mempty)

  return $ postsToMessages clientPosts
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
updatePostMap :: Posts -> MH ()
updatePostMap postCollection = do
  -- Build a map from post ID to Matterhorn message, then add the new
  -- messages to the global post map. We use the "postsPosts" field for
  -- this because that might contain more messages than the "postsOrder"
  -- list, since the former can contain other messages in threads that
  -- the server sent us, even if those messages are not part of the
  -- ordered post listing of "postsOrder."
  let postMap = HM.fromList
          [ ( pId
            , fst $ clientPostToMessage (toClientPost x Nothing)
            )
          | (pId, x) <- HM.toList (postCollection^.postsPostsL)
          ]
  csPostMap %= HM.union postMap

-- | Add a 'ClientMessage' to the current channel's message list
addClientMessage :: ClientMessage -> MH ()
addClientMessage msg = do
  cid <- use csCurrentChannelId
  uuid <- generateUUID
  let addCMsg = ccContents.cdMessages %~
          (addMessage $ clientMessageToMessage msg & mMessageId .~ Just (MessageUUID uuid))
  csChannels %= modifyChannelById cid addCMsg
  mh $ invalidateCacheEntry $ ChannelMessages cid

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
  msg <- newClientMessage Error err
  uuid <- generateUUID_IO
  let cId = st ^. csCurrentChannelId
      addEMsg = ccContents.cdMessages %~
          (addMessage $ clientMessageToMessage msg & mMessageId .~ Just (MessageUUID uuid))
  return $ st & csChannels %~ modifyChannelById cId addEMsg

openURL :: LinkChoice -> MH Bool
openURL link = do
    cfg <- use (csResources.crConfiguration)
    case configURLOpenCommand cfg of
        Nothing ->
            return False
        Just urlOpenCommand -> do
            session <- getSession

            -- Is the URL referring to an attachment?
            let act = case link^.linkFileId of
                    Nothing -> prepareLink link
                    Just fId -> prepareAttachment fId session

            -- Is the URL-opening command interactive? If so, pause
            -- Matterhorn and run the opener interactively. Otherwise
            -- run the opener asynchronously and continue running
            -- Matterhorn interactively.
            case configURLOpenCommandInteractive cfg of
                False -> do
                    outputChan <- use (csResources.crSubprocessLog)
                    doAsyncWith Preempt $ do
                        args <- act
                        runLoggedCommand False outputChan (T.unpack urlOpenCommand)
                                         args Nothing Nothing
                        return Nothing
                True -> do
                    -- If there isn't a new message cutoff showing in
                    -- the current channel, set one. This way, while the
                    -- user is gone using their interactive URL opener,
                    -- when they return, any messages that arrive in the
                    -- current channel will be displayed as new.
                    curChan <- use csCurrentChannel
                    let msgs = curChan^.ccContents.cdMessages
                    case findLatestUserMessage isEditable msgs of
                        Nothing -> return ()
                        Just m ->
                            case m^.mOriginalPost of
                                Nothing -> return ()
                                Just p ->
                                    case curChan^.ccInfo.cdNewMessageIndicator of
                                        Hide ->
                                            csCurrentChannel.ccInfo.cdNewMessageIndicator .= (NewPostsAfterServerTime (p^.postCreateAtL))
                                        _ -> return ()
                    -- No need to add a gap here: the websocket
                    -- disconnect/reconnect events will automatically
                    -- handle management of messages delivered while
                    -- suspended.

                    mhSuspendAndResume $ \st -> do
                        args <- act
                        void $ runInteractiveCommand (T.unpack urlOpenCommand) args
                        return st

                    setMode Main

            return True

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

runLoggedCommand :: Bool
                 -- ^ Whether stdout output is expected for this program
                 -> STM.TChan ProgramOutput
                 -- ^ The output channel to send the output to
                 -> String
                 -- ^ The program name
                 -> [String]
                 -- ^ Arguments
                 -> Maybe String
                 -- ^ The stdin to send, if any
                 -> Maybe (MVar ProgramOutput)
                 -- ^ Where to put the program output when it is ready
                 -> IO ()
runLoggedCommand stdoutOkay outputChan cmd args mInput mOutputVar = void $ forkIO $ do
    let stdIn = maybe NoStream (const CreatePipe) mInput
        opener = (proc cmd args) { std_in = stdIn
                                 , std_out = CreatePipe
                                 , std_err = CreatePipe
                                 }
    result <- try $ createProcess opener
    case result of
        Left (e::SomeException) -> do
            let po = ProgramOutput cmd args "" stdoutOkay (show e) (ExitFailure 1)
            STM.atomically $ STM.writeTChan outputChan po
            maybe (return ()) (flip putMVar po) mOutputVar
        Right (stdinResult, Just outh, Just errh, ph) -> do
            case stdinResult of
                Just inh -> do
                    let Just input = mInput
                    hPutStrLn inh input
                    hFlush inh
                Nothing -> return ()

            ec <- waitForProcess ph
            outResult <- hGetContents outh
            errResult <- hGetContents errh
            let po = ProgramOutput cmd args outResult stdoutOkay errResult ec
            STM.atomically $ STM.writeTChan outputChan po
            maybe (return ()) (flip putMVar po) mOutputVar
        Right _ ->
            error $ "BUG: createProcess returned unexpected result, report this at " <>
                    "https://github.com/matterhorn-chat/matterhorn"

prepareLink :: LinkChoice -> IO [String]
prepareLink link = return [T.unpack $ link^.linkURL]

prepareAttachment :: FileId -> Session -> IO [String]
prepareAttachment fId sess = do
    -- The link is for an attachment, so fetch it and then
    -- open the local copy.

    (info, contents) <- concurrently (mmGetMetadataForFile fId sess) (mmGetFile fId sess)
    cacheDir <- getUserCacheDir xdgName

    let dir   = cacheDir </> "files" </> T.unpack (idString fId)
        fname = dir </> T.unpack (fileInfoName info)

    createDirectoryIfMissing True dir
    BS.writeFile fname contents
    return [fname]

removeEmoteFormatting :: T.Text -> T.Text
removeEmoteFormatting t
    | "*" `T.isPrefixOf` t &&
      "*" `T.isSuffixOf` t = T.init $ T.drop 1 t
    | otherwise = t

addEmoteFormatting :: T.Text -> T.Text
addEmoteFormatting t = "*" <> t <> "*"

-- | Given a list of usernames, ensure that we have a user record for
-- each one in the state, either by confirming that a local record
-- exists or by issuing a request for user records.
fetchUsersByUsername :: [Text] -> MH ()
fetchUsersByUsername [] = return ()
fetchUsersByUsername usernames = do
    st <- use id
    session <- getSession
    let missing = filter (\n -> (not $ T.null n) && (isNothing $ userByUsername n st)) usernames
    when (not $ null missing) $ do
        mhLog LogGeneral $ T.pack $ "fetchUsersByUsername: getting " <> show usernames
        doAsyncWith Normal $ do
            results <- mmGetUsersByUsernames (Seq.fromList missing) session
            return $ Just $ do
                forM_ results (\u -> addNewUser $ userInfoFromUser u True)
