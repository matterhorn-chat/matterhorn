module Matterhorn.Zipper
  ( Zipper
  , fromList
  , toList
  , focus
  , unsafeFocus
  , left
  , leftL
  , right
  , rightL
  , findRight
  , maybeFindRight
  , updateListBy
  , filterZipper
  , maybeMapZipper
  , isEmpty
  , position
  )
where

import           Prelude ()
import           Matterhorn.Prelude hiding (toList)

import           Data.List ( elemIndex )
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

isEmpty :: Zipper a b -> Bool
isEmpty = C.isEmpty . zRing

position :: (Eq b) => Zipper a b -> Maybe Int
position z = do
    f <- focus z
    elemIndex f $ concat $ fmap snd $ toList z

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
focus :: Zipper a b -> Maybe b
focus = C.focus . zRing

unsafeFocus :: Zipper a b -> b
unsafeFocus = fromJust . focus

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

toList :: Zipper a b -> [(a, [b])]
toList z = F.toList $ zTrees z & mapped._2 %~ F.toList

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

-- | Update the zipper's entry list, using the specified function
-- determine which entry should be selected in the new zipper state.
updateListBy :: (Eq b)
             => (Maybe b -> b -> Bool)
             -- ^ The comparison function. This is given the previous
             -- zipper's focus value (which is optional) and is given
             -- every element in the new zipper state for comparison.
             -- This should return True for the item in the new zipper
             -- that matches the focused item in the old zipper.
             -> [(a, [b])]
             -- ^ The new zipper list contents.
             -> Zipper a b
             -- ^ The old zipper.
             -> Zipper a b
updateListBy f newList oldZip = findRight (f (focus oldZip)) $ fromList newList

maybeMapZipper :: (Eq c) => (b -> Maybe c) -> Zipper a b -> Zipper a c
maybeMapZipper f z =
    let oldTrees = zTrees z
        newTrees = F.toList $ oldTrees & mapped._2 %~ (catMaybes . F.toList . fmap f)
    in fromList newTrees

filterZipper :: (Eq b) => (b -> Bool) -> Zipper a b -> Zipper a b
filterZipper f oldZip = maintainFocus newZip
  where maintainFocus = findRight ((== focus oldZip) . Just)
        newZip = Zipper { zTrees = zTrees oldZip & mapped._2 %~ Seq.filter f
                        , zRing = C.filterR f (zRing oldZip)
                        }
