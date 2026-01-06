module Matterhorn.State.ThreadWindow
  ( openThreadWindow
  , closeThreadWindow
  )
where

import Prelude ()
import Matterhorn.Prelude

import Brick.Main ( invalidateCache )
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
    mPid <- preuse (maybeThreadInterface(tId)._Just.miRootPostId)
    when (not (mPid == Just pId)) $ do
        -- Fetch the entire thread associated with the post.
        doAsyncMM Preempt getThread processThread
        where getThread session = MM.mmGetThread pId session
              processThread posts = Just $ Work "openThreadWindow" $ do
                  let numPosts = HM.size (MM.postsPosts posts)

                  when (numPosts > 0) $ do
                      eventQueue <- use (csResources.crEventQueue)
                      msgs <- installMessagesFromPosts (Just tId) posts

                      mapM_ (addMessageToState False False . OldPost)
                               [ (posts^.postsPostsL) HM.! p
                               | p <- toList (posts^.postsOrderL)
                               ]

                      st <- use id
                      case getMessageForPostId st pId of
                          Just rootMsg | Just rootPost <- rootMsg^.mOriginalPost -> do
                              checker <- use (csResources.crSpellChecker)
                              ti <- liftIO $ newThreadInterface checker eventQueue tId cId rootMsg rootPost msgs
                              csTeam(tId).tsThreadInterface .= Just ti
                              csTeam(tId).tsMessageInterfaceFocus .= FocusThread
                              mcId <- use (csCurrentChannelId(tId))
                              case mcId of
                                  Just curId -> invalidateChannelRenderingCache curId
                                  Nothing -> return ()
                          _ -> error "BUG: openThreadWindow failed to find the root message"

closeThreadWindow :: TeamId -> MH ()
closeThreadWindow tId = do
    csTeam(tId).tsThreadInterface .= Nothing
    csTeam(tId).tsMessageInterfaceFocus .= FocusCurrentChannel
    mh invalidateCache
