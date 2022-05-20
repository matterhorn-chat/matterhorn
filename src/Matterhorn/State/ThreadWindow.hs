module Matterhorn.State.ThreadWindow
  ( openThreadWindow
  , closeThreadWindow
  )
where

import Prelude ()
import Matterhorn.Prelude

import qualified Data.HashMap.Strict as HM
import Lens.Micro.Platform ( (.=), _Just )

import Network.Mattermost.Types (TeamId, PostId, ChannelId)
import qualified Network.Mattermost.Types as MM
import qualified Network.Mattermost.Endpoints as MM
import Network.Mattermost.Lenses

import Matterhorn.Types
import Matterhorn.State.Common
import Matterhorn.State.Teams ( newThreadInterface )
import {-# SOURCE #-} Matterhorn.State.Messages

openThreadWindow :: TeamId -> ChannelId -> PostId -> MH ()
openThreadWindow tId cId pId = do
    -- If the thread we're switching to is the one we're already
    -- viewing, do nothing.
    m <- use (csTeam(tId).tsMode)
    mPid <- preuse (maybeThreadInterface(tId)._Just.threadRootPostId)
    when (not (m == ThreadWindow cId && mPid == Just pId)) $ do
        -- Fetch the entire thread associated with the post.
        doAsyncMM Preempt getThread (processThread m)
        where getThread session = MM.mmGetThread pId session
              processThread m posts = Just $ do
                  let numPosts = HM.size (MM.postsPosts posts)

                  when (numPosts > 0) $ do
                      st <- use id
                      eventQueue <- use (csResources.crEventQueue)
                      msgs <- installMessagesFromPosts (Just tId) posts

                      mapM_ (addMessageToState False False . OldPost)
                               [ (posts^.postsPostsL) HM.! p
                               | p <- toList (posts^.postsOrderL)
                               ]

                      let mMsg = getMessageForPostId st pId
                      case mMsg of
                          Just rootMsg | Just rootPost <- rootMsg^.mOriginalPost -> do
                              checker <- use (csTeam(tId).tsGlobalEditState.gedSpellChecker)
                              ti <- liftIO $ newThreadInterface checker eventQueue tId cId rootMsg rootPost msgs
                              csTeam(tId).tsThreadInterface .= Just ti
                              when (m /= ThreadWindow cId) $
                                  pushMode tId $ ThreadWindow cId
                          _ -> error "BUG: openThreadWindow failed to find the root message"

closeThreadWindow :: TeamId -> MH ()
closeThreadWindow tId = do
    let close = do
            csTeam(tId).tsThreadInterface .= Nothing
            popMode tId

    mCid <- use $ csCurrentChannelId tId

    case mCid of
        Nothing -> return ()
        Just cId -> whenMode tId (ThreadWindow cId) close
