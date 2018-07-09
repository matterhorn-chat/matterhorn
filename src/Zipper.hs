module Zipper
  ( Zipper
  , fromList
  , focus
  , focusL
  , left
  , leftL
  , right
  , rightL
  , findLeft
  , findRight
  , maybeFindRight
  , updateList
  , filterZipper
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Data.Foldable as F
import           Lens.Micro.Platform ( Lens, lens, ix, (.~) )


data Zipper a = Zipper
  { zFocus :: Int
  , zElems :: [a]
  }

instance F.Foldable Zipper where
  foldMap f = foldMap f . zElems

-- Move the focus one element to the left
left :: Zipper a -> Zipper a
left z = z { zFocus = (zFocus z - 1) `mod` length (zElems z) }

-- A lens on the zipper moved to the left
leftL :: Lens (Zipper a) (Zipper a) (Zipper a) (Zipper a)
leftL = lens left (\ _ b -> right b)

-- Move the focus one element to the right
right :: Zipper a -> Zipper a
right z = z { zFocus = (zFocus z + 1) `mod` length (zElems z) }

-- A lens on the zipper moved to the right
rightL :: Lens (Zipper a) (Zipper a) (Zipper a) (Zipper a)
rightL = lens right (\ _ b -> left b)

-- Return the focus element
focus :: Zipper a -> a
focus z = zElems z !! zFocus z

-- A lens to return the focus element
focusL :: Lens (Zipper a) (Zipper a) a a
focusL = lens focus upd
  where upd (Zipper n elems) x = Zipper n (elems & ix(n) .~ x)

-- Turn a list into a wraparound zipper, focusing on the head
fromList :: [a] -> Zipper a
fromList xs = Zipper { zFocus = 0, zElems = xs }

-- Shift the focus until a given element is found, or return the
-- same zipper if none applies
findRight :: (a -> Bool) -> Zipper a -> Zipper a
findRight f z = fromMaybe z $ maybeFindRight f z

-- Shift the focus until a given element is found, or return
-- Nothing if none applies
maybeFindRight :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
maybeFindRight f z
  | f (focus z) = Just z
  | otherwise   = go (right z) (zFocus z)
  where go zC n
          | n == zFocus zC = Nothing
          | f (focus zC)   = Just zC
          | otherwise      = go (right zC) n

-- Shift the focus until a given element is found, or return the
-- same zipper if none applies
findLeft :: (a -> Bool) -> Zipper a -> Zipper a
findLeft f z
  | f (focus z) = z
  | otherwise   = go (left z) (zFocus z)
  where go zC n
          | n == zFocus zC = zC
          | f (focus zC)   = zC
          | otherwise      = go (left zC) n

updateList :: (Eq a) => [a] -> Zipper a -> Zipper a
updateList newList oldZip = findLeft (== oldFocus) newZip
  where oldFocus = focus oldZip
        newZip   = oldZip { zElems = newList }

filterZipper :: (Eq a) => (a -> Bool) -> Zipper a -> Zipper a
filterZipper f oldZip = findLeft (== oldFocus) newZip
  where oldFocus = focus oldZip
        newZip   = oldZip { zElems = filter f $ zElems oldZip }
