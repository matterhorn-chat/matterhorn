{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

{-| This module is for internal re-exports of commonly-used
  functions. This also lets us avoid churn between versions of GHC by
  putting changed functions behind CPP in a single place.
-}

{-
  GHC version <--> base version (https://wiki.haskell.org/Base_package) includes:
     8.0.1      4.9.0.0
     8.2.1      4.10.0.0
     8.4.1      4.11.0.0

  GHC distributions include a set of core packages; overriding these
  with new versions is unwise.  Core packages include (see
  https://downloads.haskell.org/~ghc/X.Y.Z/docs/html/users_guide/X.Y.Z-notes.html#included-libraries
  ):

    base, Cabal, array, bytestring, containers, deepseq, directory,
    filepath, mtl, parsec, process, text, time, unix
-}

module Prelude.MH
  ( module P
#if !MIN_VERSION_base(4,9,0)
  , (<>)
#endif
  , (<|>)
  -- commonly-used functions from Maybe
  , Maybe.isJust
  , Maybe.isNothing
  , Maybe.listToMaybe
  , Maybe.maybeToList
  , Maybe.fromMaybe
  , Maybe.catMaybes

  -- a non-partial Read function
  , Read.readMaybe

  -- commonly-used functions from Monad
  , Monad.forM
  , Monad.forM_
  , Monad.filterM
  , Monad.when
  , Monad.unless
  , Monad.void
  , Monad.join
  , Monad.forever
  , Monad.foldM
  , Monad.MonadIO(..)

  -- commonly-used functions from List
  , Foldable.toList
  , List.find
  , List.sort
  , List.intercalate
  , Exts.sortWith
  , Exts.groupWith

  -- common read-only lens operators
  , (Lens.&)
  , (Lens.^.)
  , Lens.use

  -- not available in all versions of GHC currently in use
#if MIN_VERSION_base(4,10,0)
  , Clock.nominalDay
#else
  , nominalDay
#endif

  -- various type aliases
  , Text
  , HashMap
  , Seq
  , Set
  , Time.UTCTime
  , Time.TimeZoneSeries
  , Time.NominalDiffTime
  )
where


#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ( (<>) )
#endif

import           Control.Applicative ( (<|>) )
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as Monad
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Time as Time
#if MIN_VERSION_base(4,10,0)
import qualified Data.Time.Clock as Clock
#endif
import qualified Data.Time.LocalTime.TimeZone.Series as Time
import qualified GHC.Exts as Exts
import qualified Lens.Micro.Platform as Lens
import           Prelude.Compat
import qualified Prelude.Compat as P
import qualified Text.Read as Read

-- these below we import only for type aliases
import           Data.HashMap.Strict ( HashMap )
import           Data.Sequence ( Seq )
import           Data.Set ( Set )
import           Data.Text ( Text )

#if !MIN_VERSION_base(4,10,0)
nominalDay :: Time.NominalDiffTime
nominalDay = 86400
#endif
