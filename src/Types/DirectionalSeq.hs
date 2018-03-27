{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{- | These declarations allow the use of a DirectionalSeq, which is a
   Seq that uses a phantom type to identify the ordering of the
   elements in the sequence (Forward or Reverse).  The constructors
   are not exported from this module so that a DirectionalSeq can only
   be constructed by the functions in this module.
-}

module Types.DirectionalSeq where


import           Data.Monoid.Compat
import qualified Data.Sequence as Seq


data Chronological
data Retrograde
class SeqDirection a
instance SeqDirection Chronological
instance SeqDirection Retrograde

data SeqDirection dir => DirectionalSeq dir a =
    DSeq { dseq :: Seq.Seq a }
         deriving (Show, Functor, Foldable, Traversable)

emptyDirSeq :: DirectionalSeq dir a
emptyDirSeq = DSeq mempty

appendDirSeq :: DirectionalSeq dir a -> DirectionalSeq dir a -> DirectionalSeq dir a
appendDirSeq a b = DSeq $ mappend (dseq a) (dseq b)

onDirectedSeq :: SeqDirection dir => (Seq.Seq a -> Seq.Seq b)
              -> DirectionalSeq dir a -> DirectionalSeq dir b
onDirectedSeq f = DSeq . f . dseq

-- | Uses a start-predicate and and end-predicate to
-- identify (the first matching) subset that is delineated by
-- start-predicate and end-predicate (inclusive).  It will then call
-- the passed operation function on the subset messages to get back a
-- (possibly modified) set of messages, along with an extracted value.
-- The 'onDirSeqSubset' function will replace the original subset of
-- messages with the set returned by the operation function and return
-- the resulting message list along with the extracted value.

onDirSeqSubset :: SeqDirection dir =>
                 (e -> Bool) -> (e -> Bool)
               -> (DirectionalSeq dir e -> (DirectionalSeq dir e, a))
               -> DirectionalSeq dir e
               -> (DirectionalSeq dir e, a)
onDirSeqSubset startPred endPred op entries =
    let ml = dseq entries
        (bl, ml1) = Seq.breakl startPred ml
        (ml2, el) = Seq.breakl endPred ml1
        -- move match from start of el to end of ml2
        (ml2', el') = if not (Seq.null el)
                      then (ml2 <> Seq.take 1 el, Seq.drop 1 el)
                      else (ml2, el)
        (ml3, rval) = op $ DSeq ml2'
    in (DSeq bl `appendDirSeq` ml3 `appendDirSeq` DSeq el', rval)

-- | dirSeqBreakl splits the DirectionalSeq into a tuple where the
-- first element is the (possibly empty) DirectionalSeq of all
-- elements from the start for which the predicate returns false; the
-- second tuple element is the remainder of the list, starting with
-- the first element for which the predicate matched.
dirSeqBreakl :: SeqDirection dir =>
               (e -> Bool) -> DirectionalSeq dir e
             -> (DirectionalSeq dir e, DirectionalSeq dir e)
dirSeqBreakl isMatch entries =
    let (removed, remaining) = Seq.breakl isMatch $ dseq entries
    in (DSeq removed, DSeq remaining)

-- | dirSeqPartition splits the DirectionalSeq into a tuple of two
-- DirectionalSeq elements: the first contains all elements for which
-- the predicate is true and the second contains all elements for
-- which the predicate is false.
dirSeqPartition :: SeqDirection dir =>
                  (e -> Bool) -> DirectionalSeq dir e
                -> (DirectionalSeq dir e, DirectionalSeq dir e)
dirSeqPartition isMatch entries =
    let (match, nomatch) = Seq.partition isMatch $ dseq entries
    in (DSeq match, DSeq nomatch)


withDirSeqHead :: SeqDirection dir => (e -> r) -> DirectionalSeq dir e -> Maybe r
withDirSeqHead op entries =
    case Seq.viewl (dseq entries) of
      Seq.EmptyL -> Nothing
      e Seq.:< _ -> Just $ op e
