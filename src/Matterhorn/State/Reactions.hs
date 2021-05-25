module Matterhorn.State.Reactions
  ( asyncFetchReactionsForPost
  , addReactions
  , removeReaction
  , updateReaction
  , toggleReaction
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Main ( invalidateCacheEntry )
import qualified Data.Map.Strict as Map
import           Lens.Micro.Platform
import qualified Data.Set as S

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           Matterhorn.State.Async
import           Matterhorn.State.Common ( fetchMentionedUsers )
import           Matterhorn.Types


-- | Queue up a fetch for the reactions of the specified post in the
-- specified channel.
asyncFetchReactionsForPost :: ChannelId -> Post -> MH ()
asyncFetchReactionsForPost cId p
  | not (p^.postHasReactionsL) = return ()
  | otherwise = doAsyncChannelMM Normal cId
        (\s _ -> fmap toList (mmGetReactionsForPost (p^.postIdL) s))
        (\_ rs -> Just $ addReactions cId rs)

-- | Add the specified reactions returned by the server to the relevant
-- posts in the specified channel. This should only be called in
-- response to a server API request or event. If you want to add
-- reactions to a post, start by calling @mmPostReaction@. We also
-- invalidate the cache for any rendered message corresponding to the
-- incoming reactions.
addReactions :: ChannelId -> [Reaction] -> MH ()
addReactions cId rs = do
    mh $ invalidateCacheEntry $ ChannelMessages cId
    csChannel(cId).ccContents.cdMessages %= fmap upd
    let mentions = S.fromList $ UserIdMention <$> reactionUserId <$> rs
    fetchMentionedUsers mentions
    invalidateRenderCache
  where upd msg = msg & mReactions %~ insertAll (msg^.mMessageId)
        insert mId r
          | mId == Just (MessagePostId (r^.reactionPostIdL)) =
              Map.insertWith S.union (r^.reactionEmojiNameL) (S.singleton $ r^.reactionUserIdL)
          | otherwise = id
        insertAll mId msg = foldr (insert mId) msg rs
        invalidateRenderCache = do
          let cacheIds = map cacheIdOf rs
          mh $ mapM_ invalidateCacheEntry cacheIds
        cacheIdOf r = RenderedMessage $ MessagePostId (r^.reactionPostIdL)

-- | Remove the specified reaction from its message in the specified
-- channel. This should only be called in response to a server event
-- instructing us to remove the reaction. If you want to trigger such an
-- event, use @updateReaction@. We also invalidate the cache for any
-- rendered message corresponding to the removed reaction.
removeReaction :: Reaction -> ChannelId -> MH ()
removeReaction r cId = do
    mh $ invalidateCacheEntry $ ChannelMessages cId
    csChannel(cId).ccContents.cdMessages %= fmap upd
    invalidateRenderCache
  where upd m | m^.mMessageId == Just (MessagePostId $ r^.reactionPostIdL) =
                  m & mReactions %~ (Map.alter delReaction (r^.reactionEmojiNameL))
              | otherwise = m
        delReaction mUs = S.delete (r^.reactionUserIdL) <$> mUs
        invalidateRenderCache =
          mh $ invalidateCacheEntry $ RenderedMessage $ MessagePostId (r^.reactionPostIdL)

-- | Set or unset a reaction on a post.
updateReaction :: PostId -> Text -> Bool -> MH ()
updateReaction pId text value = do
    session <- getSession
    myId <- gets myUserId
    if value
      then doAsyncWith Preempt $ do
                mmPostReaction pId myId text session
                return Nothing
      else doAsyncWith Preempt $ do
                mmDeleteReaction pId myId text session
                return Nothing

-- | Toggle a reaction on a post.
toggleReaction :: PostId -> Text -> Set UserId -> MH ()
toggleReaction pId text uIds = do
    myId <- gets myUserId
    let current = myId `S.member` uIds
    updateReaction pId text (not current)