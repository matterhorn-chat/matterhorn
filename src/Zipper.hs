module Zipper
  ( Zipper
  , fromList
  , focus
  , left
  , leftL
  , right
  , rightL
  , findRight
  , maybeFindRight
  , updateList
  , filterZipper
  , maybeMapZipper
  )
where

import           Prelude ()
import           Prelude.MH

import           Data.Maybe ( fromJust )
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.CircularList as C
import           Lens.Micro.Platform

data Zipper a b =
    Zipper { zRing :: C.CList b
           , zTrees :: Seq.Seq (a, Seq.Seq b)
           }

instance F.Foldable (Zipper a) where
    foldMap f = foldMap f .
                F.toList .
                mconcat .
                F.toList .
                fmap snd .
                zTrees

instance Functor (Zipper a) where
    fmap f z =
        Zipper { zRing = f <$> zRing z
               , zTrees = zTrees z & mapped._2.mapped %~ f
               }

-- Move the focus one element to the left
left :: Zipper a b -> Zipper a b
left z = z { zRing = C.rotL (zRing z) }

-- A lens on the zipper moved to the left
leftL :: Lens (Zipper a b) (Zipper a b) (Zipper a b) (Zipper a b)
leftL = lens left (\ _ b -> right b)

-- Move the focus one element to the right
right :: Zipper a b -> Zipper a b
right z = z { zRing = C.rotR (zRing z) }

-- A lens on the zipper moved to the right
rightL :: Lens (Zipper a b) (Zipper a b) (Zipper a b) (Zipper a b)
rightL = lens right (\ _ b -> left b)

-- Return the focus element
focus :: Zipper a b -> b
focus = fromJust . C.focus . zRing

-- Turn a list into a wraparound zipper, focusing on the head
fromList :: (Eq b) => [(a, [b])] -> Zipper a b
fromList xs =
    let ts = Seq.fromList $ xs & mapped._2 %~ Seq.fromList
        tsList = F.toList $ mconcat $ F.toList $ snd <$> ts
        maybeFocus = if null tsList
                     then id
                     else fromJust . C.rotateTo (tsList !! 0)
    in Zipper { zRing = maybeFocus $ C.fromList tsList
              , zTrees = ts
              }

-- Shift the focus until a given element is found, or return the
-- same zipper if none applies
findRight :: (b -> Bool) -> Zipper a b -> Zipper a b
findRight f z = fromMaybe z $ maybeFindRight f z

-- Shift the focus until a given element is found, or return
-- Nothing if none applies
maybeFindRight :: (b -> Bool) -> Zipper a b -> Maybe (Zipper a b)
maybeFindRight f z = do
    newRing <- C.findRotateTo f (zRing z)
    return z { zRing = newRing }

updateList :: (Eq b) => [(a, [b])] -> Zipper a b -> Zipper a b
updateList newList oldZip = findRight (== focus oldZip) $ fromList newList

maybeMapZipper :: (Eq c) => (b -> Maybe c) -> Zipper a b -> Zipper a c
maybeMapZipper f z =
    let oldTrees = zTrees z
        newTrees = F.toList $ oldTrees & mapped._2 %~ (catMaybes . F.toList . fmap f)
    in fromList newTrees

filterZipper :: (Eq b) => (b -> Bool) -> Zipper a b -> Zipper a b
filterZipper f oldZip = maintainFocus newZip
  where maintainFocus = findRight (== focus oldZip)
        newZip = Zipper { zTrees = zTrees oldZip & mapped._2 %~ Seq.filter f
                        , zRing = C.filterR f (zRing oldZip)
                        }
