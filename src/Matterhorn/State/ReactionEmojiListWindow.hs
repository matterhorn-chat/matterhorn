{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.ReactionEmojiListWindow
  ( enterReactionEmojiListWindowMode

  , reactionEmojiListSelectDown
  , reactionEmojiListSelectUp
  , reactionEmojiListPageDown
  , reactionEmojiListPageUp
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as Set
import           Data.Function ( on )
import           Data.List ( nubBy )

import           Network.Mattermost.Types

import           Matterhorn.Emoji
import           Matterhorn.State.ListWindow
import           Matterhorn.Types
import           Matterhorn.State.Reactions ( updateReaction )


enterReactionEmojiListWindowMode :: TeamId -> Message -> MH ()
enterReactionEmojiListWindowMode tId msg = do
    em <- use (csResources.crEmoji)
    myId <- gets myUserId
    enterListWindowMode tId (csTeam(tId).tsReactionEmojiListWindow) ReactionEmojiListWindow
        () (enterHandler msg) (fetchResults myId msg em)

enterHandler :: Message -> (Bool, T.Text) -> MH Bool
enterHandler msg (mine, e) = do
    case msg^.mOriginalPost of
        Nothing -> return False
        Just p -> do
            updateReaction (postId p) e (not mine)
            return True

fetchResults :: UserId
             -- ^ My user ID, so we can see which reactions I haven't
             -- posted
             -> Message
             -- ^ The selected message, so we can include its current
             -- reactions in the list
             -> EmojiCollection
             -- ^ The emoji collection
             -> ()
             -- ^ The scope to search
             -> Session
             -- ^ The connection session
             -> Text
             -- ^ The search string
             -> IO (Vec.Vector (Bool, T.Text))
fetchResults myId msg em () session searchString = do
    let currentReactions = [ (myId `Set.member` uIds, k)
                           | (k, uIds) <- M.toList (msg^.mReactions)
                           ]
        matchingCurrentOtherReactions = [ (mine, r) | (mine, r) <- currentReactions
                                        , matchesEmoji searchString r
                                        , not mine
                                        ]
        matchingCurrentMyReactions = [ (mine, r) | (mine, r) <- currentReactions
                                     , matchesEmoji searchString r
                                     , mine
                                     ]
    serverMatches <- getMatchingEmoji session em searchString
    return $ Vec.fromList $ nubBy ((==) `on` snd) $
        matchingCurrentOtherReactions <> matchingCurrentMyReactions <> ((False,) <$> serverMatches)

-- | Move the selection up in the emoji list window by one emoji.
reactionEmojiListSelectUp :: TeamId -> MH ()
reactionEmojiListSelectUp tId = reactionEmojiListMove tId L.listMoveUp

-- | Move the selection down in the emoji list window by one emoji.
reactionEmojiListSelectDown :: TeamId -> MH ()
reactionEmojiListSelectDown tId = reactionEmojiListMove tId L.listMoveDown

-- | Move the selection up in the emoji list window by a page of emoji
-- (ReactionEmojiListPageSize).
reactionEmojiListPageUp :: TeamId -> MH ()
reactionEmojiListPageUp tId = reactionEmojiListMove tId (L.listMoveBy (-1 * reactionEmojiListPageSize))

-- | Move the selection down in the emoji list window by a page of emoji
-- (ReactionEmojiListPageSize).
reactionEmojiListPageDown :: TeamId -> MH ()
reactionEmojiListPageDown tId = reactionEmojiListMove tId (L.listMoveBy reactionEmojiListPageSize)

-- | Transform the emoji list results in some way, e.g. by moving the
-- cursor, and then check to see whether the modification warrants a
-- prefetch of more search results.
reactionEmojiListMove :: TeamId -> (L.List Name (Bool, T.Text) -> L.List Name (Bool, T.Text)) -> MH ()
reactionEmojiListMove tId = listWindowMove (csTeam(tId).tsReactionEmojiListWindow)

-- | The number of emoji in a "page" for cursor movement purposes.
reactionEmojiListPageSize :: Int
reactionEmojiListPageSize = 10
