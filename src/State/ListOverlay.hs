{-# LANGUAGE RankNTypes #-}
module State.ListOverlay
  ( listOverlayActivateCurrent
  , listOverlaySearchString
  , listOverlayMove
  , exitListOverlay
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import           Lens.Micro.Platform ( Lens', (%=), (.=) )

import           Types


listOverlayActivateCurrent :: Lens' ChatState (ListOverlayState a b) -> MH ()
listOverlayActivateCurrent which = do
  mItem <- L.listSelectedElement <$> use (which.listOverlaySearchResults)
  case mItem of
      Nothing -> return ()
      Just (_, user) -> do
          handler <- use (which.listOverlayEnterHandler)
          activated <- handler user
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

-- | Clear out the state of the user list overlay and return to the Main
-- mode.
exitListOverlay :: Lens' ChatState (ListOverlayState a b) -> MH ()
exitListOverlay which = do
    newList <- use (which.listOverlayNewList)
    which.listOverlaySearchResults .= newList mempty
    which.listOverlayEnterHandler .= (const $ return False)
    setMode Main
