module State.Common
  (
  -- * System interface
    copyToClipboard
  , openURL
  , runLoggedCommand

  -- * Attachments
  , asyncFetchAttachments

  -- * Reactions
  , asyncFetchReactionsForPost
  , addReactions
  , removeReaction

  -- * Posts
  , messagesFromPosts

  -- * Async
  , AsyncPriority(..)
  , doAsync
  , doAsyncIO
  , doAsyncWith
  , doAsyncChannelMM
  , doAsyncWithIO
  , doAsyncMM
  , tryMM
  , endAsyncNOP

  -- * Utilities
  , postInfoMessage
  , postErrorMessageIO
  , postErrorMessage'
  , mhError
  , addEmoteFormatting
  , removeEmoteFormatting
  , msgURLs

  -- * Prefs
  , channelHiddenPreference
  )
where

import           Prelude ()
import           Prelude.MH

import           Control.Concurrent ( MVar, putMVar, forkIO )
import           Control.Concurrent.Async ( concurrently )
import qualified Control.Concurrent.STM as STM
import           Control.Exception ( SomeException, try )
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import           Lens.Micro.Platform ( (.=), (%=), (%~), (.~), traversed )
import           System.Directory ( createDirectoryIfMissing )
import           System.Environment.XDG.BaseDir ( getUserCacheDir )
import           System.Exit ( ExitCode(..) )
import           System.FilePath
import           System.Hclip ( setClipboard, ClipboardException(..) )
import           System.IO ( hGetContents, hFlush, hPutStrLn )
import           System.Process ( proc, std_in, std_out, std_err, StdStream(..)
                                , createProcess, waitForProcess )

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           FilePaths ( xdgName )
import           Markdown ( blockGetURLs )
import           Types


-- * Mattermost API

-- | Try to run a computation, posting an informative error
--   message if it fails with a 'MattermostServerError'.
tryMM :: IO a
      -- ^ The action to try (usually a MM API call)
      -> (a -> IO (MH ()))
      -- ^ What to do on success
      -> IO (MH ())
tryMM act onSuccess = do
    result <- liftIO $ try act
    case result of
        Left e -> return $ mhError $ ServerError e
        Right value -> liftIO $ onSuccess value

-- * Background Computation

-- $background_computation
--
-- The main context for Matterhorn is the EventM context provided by
-- the 'Brick' library.  This context is normally waiting for user
-- input (or terminal resizing, etc.) which gets turned into an
-- MHEvent and the 'onEvent' event handler is called to process that
-- event, after which the display is redrawn as necessary and brick
-- awaits the next input.
--
-- However, it is often convenient to communicate with the Mattermost
-- server in the background, so that large numbers of
-- synchronously-blocking events (e.g. on startup) or refreshes can
-- occur whenever needed and without negatively impacting the UI
-- updates or responsiveness.  This is handled by a 'forkIO' context
-- that waits on an STM channel for work to do, performs the work, and
-- then sends brick an MHEvent containing the completion or failure
-- information for that work.
--
-- The /doAsyncWith/ family of functions here facilitates that
-- asynchronous functionality.  This is typically used in the
-- following fashion:
--
-- > doSomething :: MH ()
-- > doSomething = do
-- >    got <- something
-- >    doAsyncWith Normal $ do
-- >       r <- mmFetchR ....
-- >       return $ do
-- >          csSomething.here %= processed r
--
-- The second argument is an IO monad operation (because 'forkIO' runs
-- in the IO Monad context), but it returns an MH monad operation.
-- The IO monad has access to the closure of 'doSomething' (e.g. the
-- 'got' value), but it should be aware that the state of the MH monad
-- may have been changed by the time the IO monad runs in the
-- background, so the closure is a snapshot of information at the time
-- the 'doAsyncWith' was called.
--
-- Similarly, the returned MH monad operation is *not* run in the
-- context of the 'forkIO' background, but it is instead passed via an
-- MHEvent back to the main brick thread, where it is executed in an
-- EventM handler's MH monad context.  This operation therefore has
-- access to the combined closure of the pre- 'doAsyncWith' code and
-- the closure of the IO operation.  It is important that the final MH
-- monad operation should *re-obtain* state information from the MH
-- monad instead of using or setting the state obtained prior to the
-- 'doAsyncWith' call.

-- | Priority setting for asynchronous work items. Preempt means that
-- the queued item will be the next work item begun (i.e. it goes to the
-- front of the queue); normal means it will go last in the queue.
data AsyncPriority = Preempt | Normal

-- | Run a computation in the background, ignoring any results from it.
doAsync :: AsyncPriority -> IO () -> MH ()
doAsync prio act = doAsyncWith prio (act >> return (return ()))

-- | Run a computation in the background, returning a computation to be
-- called on the 'ChatState' value.
doAsyncWith :: AsyncPriority -> IO (MH ()) -> MH ()
doAsyncWith prio act = do
    let putChan = case prio of
          Preempt -> STM.unGetTChan
          Normal  -> STM.writeTChan
    queue <- use (csResources.crRequestQueue)
    liftIO $ STM.atomically $ putChan queue act

doAsyncIO :: AsyncPriority -> ChatState -> IO () -> IO ()
doAsyncIO prio st act =
  doAsyncWithIO prio st (act >> return (return ()))

-- | Run a computation in the background, returning a computation to be
-- called on the 'ChatState' value.
doAsyncWithIO :: AsyncPriority -> ChatState -> IO (MH ()) -> IO ()
doAsyncWithIO prio st act = do
    let putChan = case prio of
          Preempt -> STM.unGetTChan
          Normal  -> STM.writeTChan
    let queue = st^.csResources.crRequestQueue
    STM.atomically $ putChan queue act

-- | Performs an asynchronous IO operation. On completion, the final
-- argument a completion function is executed in an MH () context in the
-- main (brick) thread.
doAsyncMM :: AsyncPriority
          -- ^ the priority for this async operation
          -> (Session -> TeamId -> IO a)
          -- ^ the async MM channel-based IO operation
          -> (a -> MH ())
          -- ^ function to process the results in brick event handling
          -- context
          -> MH ()
doAsyncMM prio mmOp eventHandler = do
  session <- getSession
  tId <- gets myTeamId
  doAsyncWith prio $ do
    r <- mmOp session tId
    return $ eventHandler r

-- | Helper type for a function to perform an asynchronous MM operation
-- on a channel and then invoke an MH completion event.
type DoAsyncChannelMM a =
    AsyncPriority
    -- ^ the priority for this async operation
    -> ChannelId
    -- ^ The channel
    -> (Session -> TeamId -> ChannelId -> IO a)
    -- ^ the asynchronous Mattermost channel-based IO operation
    -> (ChannelId -> a -> MH ())
    -- ^ function to process the results in brick event handling context
    -> MH ()

-- | Performs an asynchronous IO operation on a specific channel. On
-- completion, the final argument a completion function is executed in
-- an MH () context in the main (brick) thread.
doAsyncChannelMM :: DoAsyncChannelMM a
doAsyncChannelMM prio cId mmOp eventHandler =
  doAsyncMM prio (\s t -> mmOp s t cId) (eventHandler cId)

-- | Use this convenience function if no operation needs to be
-- performed in the MH state after an async operation completes.
endAsyncNOP :: ChannelId -> a -> MH ()
endAsyncNOP _ _ = return ()

-- * Client Messages

messagesFromPosts :: Posts -> MH Messages
messagesFromPosts p = do
  flags <- use (csResources.crFlaggedPosts)
  csPostMap %= HM.union postMap
  let msgs = postsToMessages (maybeFlag flags . clientPostToMessage) (clientPost <$> ps)
      postsToMessages f = foldr (addMessage . f) noMessages
  return msgs
    where
        postMap :: HashMap PostId Message
        postMap = HM.fromList
          [ ( pId
            , clientPostToMessage (toClientPost x Nothing)
            )
          | (pId, x) <- HM.toList (p^.postsPostsL)
          ]
        maybeFlag flagSet msg
          | Just (MessagePostId pId) <- msg^.mMessageId, pId `Set.member` flagSet
            = msg & mFlagged .~ True
          | otherwise = msg
        -- n.b. postsOrder is most recent first
        ps   = findPost <$> (Seq.reverse $ postsOrder p)
        clientPost :: Post -> ClientPost
        clientPost x = toClientPost x (postId <$> parent x)
        parent x = do
            parentId <- x^.postParentIdL
            HM.lookup parentId (p^.postsPostsL)
        findPost pId = case HM.lookup pId (postsPosts p) of
            Nothing -> error $ "BUG: could not find post for post ID " <> show pId
            Just post -> post

asyncFetchAttachments :: Post -> MH ()
asyncFetchAttachments p = do
  let cId = (p^.postChannelIdL)
      pId = (p^.postIdL)
  session <- getSession
  host    <- use (csResources.crConn.cdHostnameL)
  F.forM_ (p^.postFileIdsL) $ \fId -> doAsyncWith Normal $ do
    info <- mmGetMetadataForFile fId session
    let scheme = "https://"
        attUrl = scheme <> host <> urlForFile fId
        attachment = mkAttachment (fileInfoName info) attUrl fId
        addIfMissing a as =
            if isNothing $ Seq.elemIndexL a as
            then a Seq.<| as
            else as
        addAttachment m
          | m^.mMessageId == Just (MessagePostId pId) =
            m & mAttachments %~ (addIfMissing attachment)
          | otherwise              = m
    return $
      csChannel(cId).ccContents.cdMessages.traversed %= addAttachment

-- | Add a 'ClientMessage' to the current channel's message list
addClientMessage :: ClientMessage -> MH ()
addClientMessage msg = do
  cid <- use csCurrentChannelId
  uuid <- generateUUID
  let addCMsg = ccContents.cdMessages %~
          (addMessage $ clientMessageToMessage msg & mMessageId .~ Just (MessageUUID uuid))
  csChannels %= modifyChannelById cid addCMsg

-- | Add a new 'ClientMessage' representing an error message to
--   the current channel's message list
postInfoMessage :: Text -> MH ()
postInfoMessage err = addClientMessage =<< newClientMessage Informative err

-- | Add a new 'ClientMessage' representing an error message to
--   the current channel's message list
postErrorMessage' :: Text -> MH ()
postErrorMessage' err = addClientMessage =<< newClientMessage Error err

-- | Raise a rich error
mhError :: MHError -> MH ()
mhError err = raiseInternalEvent (DisplayError err)

postErrorMessageIO :: Text -> ChatState -> IO ChatState
postErrorMessageIO err st = do
  msg <- newClientMessage Error err
  uuid <- generateUUID_IO
  let cId = st ^. csCurrentChannelId
      addEMsg = ccContents.cdMessages %~
          (addMessage $ clientMessageToMessage msg & mMessageId .~ Just (MessageUUID uuid))
  return $ st & csChannels %~ modifyChannelById cId addEMsg

asyncFetchReactionsForPost :: ChannelId -> Post -> MH ()
asyncFetchReactionsForPost cId p
  | not (p^.postHasReactionsL) = return ()
  | otherwise = doAsyncChannelMM Normal cId
        (\s _ _ -> fmap toList (mmGetReactionsForPost (p^.postIdL) s))
        addReactions

addReactions :: ChannelId -> [Reaction] -> MH ()
addReactions cId rs = csChannel(cId).ccContents.cdMessages %= fmap upd
  where upd msg = msg & mReactions %~ insertAll (msg^.mMessageId)
        insert mId r
          | mId == Just (MessagePostId (r^.reactionPostIdL)) = Map.insertWith (+) (r^.reactionEmojiNameL) 1
          | otherwise = id
        insertAll mId msg = foldr (insert mId) msg rs

removeReaction :: Reaction -> ChannelId -> MH ()
removeReaction r cId = csChannel(cId).ccContents.cdMessages %= fmap upd
  where upd m | m^.mMessageId == Just (MessagePostId $ r^.reactionPostIdL) =
                  m & mReactions %~ (Map.insertWith (+) (r^.reactionEmojiNameL) (-1))
              | otherwise = m

copyToClipboard :: Text -> MH ()
copyToClipboard txt = do
  result <- liftIO (try (setClipboard (T.unpack txt)))
  case result of
    Left e -> do
      let errMsg = case e of
            UnsupportedOS _ ->
              "Matterhorn does not support yanking on this operating system."
            NoTextualData ->
              "Textual data is required to set the clipboard."
            MissingCommands cmds ->
              "Could not set clipboard due to missing one of the " <>
              "required program(s): " <> (T.pack $ show cmds)
      mhError $ ClipboardError errMsg
    Right () ->
      return ()

msgURLs :: Message -> Seq LinkChoice
msgURLs msg
  | NoUser <- msg^.mUser = mempty
  | otherwise =
  let uid = msg^.mUser
      msgUrls = (\ (url, text) -> LinkChoice (msg^.mDate) uid text url Nothing) <$>
                  (mconcat $ blockGetURLs <$> (toList $ msg^.mText))
      attachmentURLs = (\ a ->
                          LinkChoice
                            (msg^.mDate)
                            uid
                            ("attachment `" <> (a^.attachmentName) <> "`")
                            (a^.attachmentURL)
                            (Just (a^.attachmentFileId)))
                       <$> (msg^.mAttachments)
  in msgUrls <> attachmentURLs

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
                        return $ return ()
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
                        return $ setMode' Main st

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

channelHiddenPreference :: ChannelId -> MH Bool
channelHiddenPreference cId = do
    prefs <- use (csResources.crUserPreferences.userPrefGroupChannelPrefs)
    let matching = filter (\p -> fst p == cId) (HM.toList prefs)
    return $ any (not . snd) matching
