{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.MessageSelect
  (
  -- * Message selection mode
    openSelectedMessageInEditor
  , fillSelectedGap
  , beginConfirmDeleteSelectedMessage
  , deleteSelectedMessage
  , beginReplyCompose
  , beginEditMessage
  , getSelectedMessage
  , openThreadWindow
  , withSelectedMessage
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.Edit ( applyEdit )
import qualified Data.Text as T
import           Data.Text.Zipper ( clearZipper, insertMany )
import           Data.Maybe ( fromJust )
import           Lens.Micro.Platform
import qualified System.Environment as Sys
import           System.IO (hClose, hPutStr)
import qualified System.IO.Temp as Sys
import qualified System.Process as Sys

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Types

import           Matterhorn.State.Common
import           Matterhorn.State.MessageListing
import           Matterhorn.State.Messages ( asyncFetchMessagesForGap )
import           Matterhorn.Types
import           Matterhorn.Types.Common
import qualified Matterhorn.State.ThreadWindow as TW


getSelectedMessage :: Lens' ChatState (MessageInterface n i)
                   -> ChatState
                   -> Maybe Message
getSelectedMessage which st = do
    selMsgId <- selectMessageId $ st^.which.miListing.mlMessageSelect
    let chanMsgs = st^.which.miListing.mlMessages
    findMessage selMsgId chanMsgs

withSelectedMessage :: Lens' ChatState (MessageInterface n i)
                    -> (Message -> MH ())
                    -> MH ()
withSelectedMessage which act = do
    selectedMessage <- use (to (getSelectedMessage which))
    case selectedMessage of
        Nothing -> return ()
        Just m -> act m

openSelectedMessageInEditor :: Lens' ChatState (MessageInterface n i)
                            -> MH ()
openSelectedMessageInEditor which =
    withSelectedMessage which $ \msg ->
        when (not (isGap msg)) $ openMessageInEditor msg which

-- This will only work for channel message selection, not thread message
-- selection, since there will never be gap entries in the thread view.
-- But this is generalized enough that it looks like it should work for
-- thread views, but it won't because asyncFetchMessagesForGap only
-- works for channel message selection (and should).
fillSelectedGap :: Lens' ChatState (MessageInterface n i)
                -> MH ()
fillSelectedGap which = do
    cId <- use (which.miChannelId)
    withSelectedMessage which $ \msg ->
        when (isGap msg) $ asyncFetchMessagesForGap cId msg

openMessageInEditor :: Message -> Lens' ChatState (MessageInterface n i) -> MH ()
openMessageInEditor m which = do
    -- If EDITOR is in the environment, write the current message to a
    -- temp file, invoke EDITOR on it, read the result, remove the temp
    -- file, and update the program state.
    --
    -- If EDITOR is not present, fall back to 'vi'.
    mEnv <- liftIO $ Sys.lookupEnv "EDITOR"
    let editorProgram = maybe "vi" id mEnv

    case m^.mOriginalPost of
        Nothing -> return ()
        Just p -> do
            mhSuspendAndResume $ \s -> do
                Sys.withSystemTempFile "matterhorn_editor.md" $ \tmpFileName tmpFileHandle -> do
                    hPutStr tmpFileHandle $ T.unpack $ unsafeUserText $ postMessage p
                    hClose tmpFileHandle
                    void $ Sys.system (editorProgram <> " " <> tmpFileName)
                    return s

    exitMessageSelect (which.miListing)

openThreadWindow :: TeamId
                 -> Lens' ChatState (MessageInterface n i)
                 -> MH ()
openThreadWindow tId which =
    withSelectedMessage which $ \msg -> do
        when (isPostMessage msg) $ do
            rootMsg <- getReplyRootMessage msg
            let p = fromJust $ rootMsg^.mOriginalPost
            case msg^.mChannelId of
                Nothing -> return ()
                Just cId -> TW.openThreadWindow tId cId (postId p)

beginConfirmDeleteSelectedMessage :: TeamId
                                  -> Lens' ChatState (MessageInterface n i)
                                  -> MH ()
beginConfirmDeleteSelectedMessage tId which = do
    st <- use id
    target <- use (which.miTarget)
    withSelectedMessage which $ \msg ->
        when (isDeletable msg && isMine st msg) $
            pushMode tId $ MessageSelectDeleteConfirm target

deleteSelectedMessage :: Lens' ChatState (MessageInterface n i)
                      -> MH ()
deleteSelectedMessage which = do
    st <- use id
    withSelectedMessage which $ \msg ->
        when (isMine st msg && isDeletable msg) $ do
            exitMessageSelect (which.miListing)
            case msg^.mOriginalPost of
                Just p ->
                    doAsyncMM Preempt
                        (\s -> MM.mmDeletePost (postId p) s)
                        (const Nothing)
                Nothing -> return ()

beginReplyCompose :: Lens' ChatState (MessageInterface n i)
                  -> MH ()
beginReplyCompose which = do
    withSelectedMessage which $ \msg ->
        when (isReplyable msg) $ do
            rootMsg <- getReplyRootMessage msg
            let p = fromJust $ rootMsg^.mOriginalPost
            exitMessageSelect (which.miListing)
            which.miEditor.esEditMode .= Replying rootMsg p

beginEditMessage :: Lens' ChatState (MessageInterface n i)
                 -> MH ()
beginEditMessage which = do
    st <- use id
    withSelectedMessage which $ \msg ->
        when (isMine st msg && isEditable msg) $ do
            let p = fromJust $ msg^.mOriginalPost
            exitMessageSelect (which.miListing)
            which.miEditor.esEditMode .= Editing p (msg^.mType)
            -- If the post that we're editing is an emote, we need
            -- to strip the formatting because that's only there to
            -- indicate that the post is an emote. This is annoying and
            -- can go away one day when there is an actual post type
            -- value of "emote" that we can look at. Note that the
            -- removed formatting needs to be reinstated just prior to
            -- issuing the API call to update the post.
            let sanitized = sanitizeUserText $ postMessage p
            let toEdit = if isEmote msg
                         then removeEmoteFormatting sanitized
                         else sanitized
            which.miEditor.esEditor %= applyEdit (insertMany toEdit . clearZipper)
