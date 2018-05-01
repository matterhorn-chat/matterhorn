module State.Common
  (
  -- * System interface
    openURL
  , runLoggedCommand

  -- * Attachments
  , asyncFetchAttachments

  -- * Posts
  , messagesFromPosts

  -- * Utilities
  , postInfoMessage
  , postErrorMessageIO
  , postErrorMessage'
  , addEmoteFormatting
  , removeEmoteFormatting
  , msgURLs

  , module State.Async
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
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import           Lens.Micro.Platform ( (.=), (%=), (%~), (.~), traversed )
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
import           Markdown ( blockGetURLs )
import           State.Async
import           Types


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

postErrorMessageIO :: Text -> ChatState -> IO ChatState
postErrorMessageIO err st = do
  msg <- newClientMessage Error err
  uuid <- generateUUID_IO
  let cId = st ^. csCurrentChannelId
      addEMsg = ccContents.cdMessages %~
          (addMessage $ clientMessageToMessage msg & mMessageId .~ Just (MessageUUID uuid))
  return $ st & csChannels %~ modifyChannelById cId addEMsg

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
