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


import           Data.Monoid ()
import qualified Data.Sequence as Seq


data Chronological
data Retrograde
class SeqDirection a
instance SeqDirection Chronological
instance SeqDirection Retrograde

data SeqDirection dir => DirectionalSeq dir a =
    DSeq { dseq :: Seq.Seq a }
         deriving (Show, Functor, Foldable, Traversable)

instance SeqDirection a => Monoid (DirectionalSeq a e) where
    mempty = DSeq mempty
    mappend a b = DSeq $ mappend (dseq a) (dseq b)

onDirectedSeq :: SeqDirection dir => (Seq.Seq a -> Seq.Seq b)
              -> DirectionalSeq dir a -> DirectionalSeq dir b
onDirectedSeq f = DSeq . f . dseq

