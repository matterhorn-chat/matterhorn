module Zipper where

data Zipper a = Zipper
  { zFocus :: Int
  , zElems :: [a]
  }

-- Move the focus one element to the left
left :: Zipper a -> Zipper a
left z = z { zFocus = (zFocus z - 1) `mod` length (zElems z) }

-- Move the focus one element to the right
right :: Zipper a -> Zipper a
right z = z { zFocus = (zFocus z + 1) `mod` length (zElems z) }

-- Return the focus element
focus :: Zipper a -> a
focus z = zElems z !! zFocus z

-- Turn a list into a wraparound zipper, focusing on the head
fromList :: [a] -> Zipper a
fromList xs = Zipper { zFocus = 0, zElems = xs }

-- Shift the focus until a given element is found, or return the
-- same zipper if none applies
findRight :: (a -> Bool) -> Zipper a -> Zipper a
findRight f z
  | f (focus z) = z
  | otherwise   = go (right z) (zFocus z)
  where go zC n
          | n == zFocus zC = zC
          | f (focus zC)   = zC
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
