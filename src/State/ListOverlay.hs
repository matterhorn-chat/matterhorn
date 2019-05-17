{-# LANGUAGE RankNTypes #-}
module State.ListOverlay
  ( listOverlayActivateCurrent
  , listOverlaySearchString
  , listOverlayMove
  , exitListOverlay
  , enterListOverlayMode
  , resetListOverlaySearch
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as Vec
import           Lens.Micro.Platform ( Lens', (%=), (.=) )
import           Network.Mattermost.Types ( Session )

import           Types
import           State.Common


listOverlayActivateCurrent :: Lens' ChatState (ListOverlayState a b) -> MH ()
listOverlayActivateCurrent which = do
  mItem <- L.listSelectedElement <$> use (which.listOverlaySearchResults)
  case mItem of
      Nothing -> return ()
      Just (_, val) -> do
          handler <- use (which.listOverlayEnterHandler)
          activated <- handler val
          if activated
             then setMode Main
             else return ()

listOverlaySearchString :: Lens' ChatState (ListOverlayState a b) -> MH Text
listOverlaySearchString which =
    (head . E.getEditContents) <$> use (which.listOverlaySearchInput)

listOverlayMove :: Lens' ChatState (ListOverlayState a b)
                -> (L.List Name a -> L.List Name a)
                -> MH ()
listOverlayMove which how =
    which.listOverlaySearchResults %= how

-- | Clear out the state of the list overlay and return to the Main
-- mode.
exitListOverlay :: Lens' ChatState (ListOverlayState a b) -> MH ()
exitListOverlay which = do
    newList <- use (which.listOverlayNewList)
    which.listOverlaySearchResults .= newList mempty
    which.listOverlayEnterHandler .= (const $ return False)
    setMode Main

enterListOverlayMode :: (Lens' ChatState (ListOverlayState a b))
                     -> Mode
                     -> b
                     -> (a -> MH Bool)
                     -> (b -> Session -> Text -> IO (Vec.Vector a))
                     -> MH ()
enterListOverlayMode which mode scope enterHandler fetcher = do
    which.listOverlaySearchScope .= scope
    which.listOverlaySearchInput.E.editContentsL %= Z.clearZipper
    which.listOverlayEnterHandler .= enterHandler
    which.listOverlayFetchResults .= fetcher
    which.listOverlaySearching .= False
    which.listOverlayHasAllResults .= False
    setMode mode
    resetListOverlaySearch which

resetListOverlaySearch :: Lens' ChatState (ListOverlayState a b) -> MH ()
resetListOverlaySearch which = do
    searchPending <- use (which.listOverlaySearching)

    when (not searchPending) $ do
        searchString <- listOverlaySearchString which
        which.listOverlaySearching .= True
        which.listOverlayHasAllResults .= False
        newList <- use (which.listOverlayNewList)
        which.listOverlaySearchResults .= newList mempty
        session <- getSession
        scope <- use (which.listOverlaySearchScope)
        fetcher <- use (which.listOverlayFetchResults)
        doAsyncWith Preempt $ do
            results <- fetcher scope session searchString
            return $ Just $ do
                which.listOverlaySearchResults .= newList results
                which.listOverlaySearching .= False

                -- Now that the results are available, check to see if the
                -- search string changed since this request was submitted.
                -- If so, issue another search.
                afterSearchString <- listOverlaySearchString which
                when (searchString /= afterSearchString) $ resetListOverlaySearch which
