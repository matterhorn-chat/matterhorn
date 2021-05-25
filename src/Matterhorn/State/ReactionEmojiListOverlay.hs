{-# LANGUAGE TupleSections #-}
module Matterhorn.State.ReactionEmojiListOverlay
  ( enterReactionEmojiListOverlayMode

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
import           Lens.Micro.Platform ( to )

import           Network.Mattermost.Types

import           Matterhorn.Emoji
import           Matterhorn.State.ListOverlay
import           Matterhorn.State.MessageSelect
import           Matterhorn.Types
import           Matterhorn.State.Reactions ( updateReaction )


enterReactionEmojiListOverlayMode :: MH ()
enterReactionEmojiListOverlayMode = do
    selectedMessage <- use (to getSelectedMessage)
    case selectedMessage of
        Nothing -> return ()
        Just msg -> do
            tId <- use csCurrentTeamId
            em <- use (csResources.crEmoji)
            myId <- gets myUserId
            enterListOverlayMode (csTeam(tId).tsReactionEmojiListOverlay) ReactionEmojiListOverlay
                () enterHandler (fetchResults myId msg em)

enterHandler :: (Bool, T.Text) -> MH Bool
enterHandler (mine, e) = do
    selectedMessage <- use (to getSelectedMessage)
    case selectedMessage of
        Nothing -> return False
        Just m -> do
            case m^.mOriginalPost of
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

-- | Move the selection up in the emoji list overlay by one emoji.
reactionEmojiListSelectUp :: MH ()
reactionEmojiListSelectUp = reactionEmojiListMove L.listMoveUp

-- | Move the selection down in the emoji list overlay by one emoji.
reactionEmojiListSelectDown :: MH ()
reactionEmojiListSelectDown = reactionEmojiListMove L.listMoveDown

-- | Move the selection up in the emoji list overlay by a page of emoji
-- (ReactionEmojiListPageSize).
reactionEmojiListPageUp :: MH ()
reactionEmojiListPageUp = reactionEmojiListMove (L.listMoveBy (-1 * reactionEmojiListPageSize))

-- | Move the selection down in the emoji list overlay by a page of emoji
-- (ReactionEmojiListPageSize).
reactionEmojiListPageDown :: MH ()
reactionEmojiListPageDown = reactionEmojiListMove (L.listMoveBy reactionEmojiListPageSize)

-- | Transform the emoji list results in some way, e.g. by moving the
-- cursor, and then check to see whether the modification warrants a
-- prefetch of more search results.
reactionEmojiListMove :: (L.List Name (Bool, T.Text) -> L.List Name (Bool, T.Text)) -> MH ()
reactionEmojiListMove = listOverlayMove (csCurrentTeam.tsReactionEmojiListOverlay)

-- | The number of emoji in a "page" for cursor movement purposes.
reactionEmojiListPageSize :: Int
reactionEmojiListPageSize = 10
