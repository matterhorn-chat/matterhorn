{-# LANGUAGE RankNTypes #-}
module State.ListOverlay
  ( listOverlayActivateCurrent
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Brick.Widgets.List as L
import           Lens.Micro.Platform ( Lens' )

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
