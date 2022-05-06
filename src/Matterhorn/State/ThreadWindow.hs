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

import Matterhorn.Types
import Matterhorn.State.Common

openThreadWindow :: TeamId -> ChannelId -> PostId -> MH ()
openThreadWindow tId cId pId = do
    -- If the thread we're switching to is the one we're already
    -- viewing, do nothing.
    m <- use (csTeam(tId).tsMode)
    mPid <- preuse (csTeam(tId).tsThreadInterface._Just.threadRootPostId)
    when (not (m == ThreadWindow && mPid == Just pId)) $ do
        -- Fetch the entire thread associated with the post.
        doAsyncMM Preempt getThread (processThread m)
        where getThread session = MM.mmGetThread pId session
              processThread m posts = Just $ do
                  let numPosts = HM.size (MM.postsPosts posts)

                  when (numPosts > 0) $ do
                      msgs <- installMessagesFromPosts (Just tId) posts
                      let ti = newThreadInterface tId pId cId msgs
                      csTeam(tId).tsThreadInterface .= Just ti
                      when (m /= ThreadWindow) $
                          pushMode tId ThreadWindow

closeThreadWindow :: TeamId -> MH ()
closeThreadWindow tId = do
    let close = do
            csTeam(tId).tsThreadInterface .= Nothing
            popMode tId

    whenMode tId ThreadWindowMessageSelect $ popMode tId
    whenMode tId ThreadWindow close
