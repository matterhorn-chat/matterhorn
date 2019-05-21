module State.ReactionEmojiListOverlay
  ( enterReactionEmojiListOverlayMode

  , reactionEmojiListSelectDown
  , reactionEmojiListSelectUp
  , reactionEmojiListPageDown
  , reactionEmojiListPageUp
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as Set
import           Data.List ( nub )
import           Lens.Micro.Platform ( to )

import           Network.Mattermost.Types
import           Network.Mattermost.Endpoints ( mmPostReaction )

import           Emoji
import           State.ListOverlay
import           State.MessageSelect
import           State.Async
import           Types


enterReactionEmojiListOverlayMode :: MH ()
enterReactionEmojiListOverlayMode = do
    selectedMessage <- use (to getSelectedMessage)
    case selectedMessage of
        Nothing -> return ()
        Just msg -> do
            em <- use (csResources.crEmoji)
            myId <- gets myUserId
            enterListOverlayMode csReactionEmojiListOverlay ReactionEmojiListOverlay
                () enterHandler (fetchResults myId msg em)

enterHandler :: T.Text -> MH Bool
enterHandler e = do
    session <- getSession
    myId <- gets myUserId

    selectedMessage <- use (to getSelectedMessage)
    case selectedMessage of
        Nothing -> return False
        Just m -> do
            case m^.mOriginalPost of
                Nothing -> return False
                Just p -> do
                    doAsyncWith Preempt $ do
                        mmPostReaction (postId p) myId e session
                        return Nothing
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
             -> IO (Vec.Vector T.Text)
fetchResults myId msg em () session searchString = do
    let currentReactions = [ k
                           | (k, uIds) <- M.toList (msg^.mReactions)
                           , not $ myId `Set.member` uIds
                           ]
        matchingCurrentReactions = [ r | r <- currentReactions
                                   , T.toLower searchString `T.isInfixOf` r
                                   ]
    serverMatches <- getMatchingEmoji session em searchString
    return $ Vec.fromList $ nub $ matchingCurrentReactions <> serverMatches

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
reactionEmojiListMove :: (L.List Name T.Text -> L.List Name T.Text) -> MH ()
reactionEmojiListMove = listOverlayMove csReactionEmojiListOverlay

-- | The number of emoji in a "page" for cursor movement purposes.
reactionEmojiListPageSize :: Int
reactionEmojiListPageSize = 10
