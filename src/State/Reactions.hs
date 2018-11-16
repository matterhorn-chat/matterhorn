module State.Reactions
  ( asyncFetchReactionsForPost
  , addReactions
  , removeReaction
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Data.Map.Strict as Map
import           Lens.Micro.Platform

import           Network.Mattermost.Endpoints
import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           State.Async
import           Types


asyncFetchReactionsForPost :: ChannelId -> Post -> MH ()
asyncFetchReactionsForPost cId p
  | not (p^.postHasReactionsL) = return ()
  | otherwise = doAsyncChannelMM Normal cId
        (\s _ _ -> fmap toList (mmGetReactionsForPost (p^.postIdL) s))
        (\_ rs -> Just $ addReactions cId rs)

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
