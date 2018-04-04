{-# LANGUAGE NoImplicitPrelude #-}

module Prelude.MH
( module Prelude.Compat
, (<>)
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
, List.find
, List.sort
, List.intercalate
, Exts.sortWith
, Exts.groupWith
) where

import           Data.Semigroup ((<>))
import           Prelude.Compat
import           Control.Applicative ((<|>))
import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified GHC.Exts as Exts
import qualified Text.Read as Read
