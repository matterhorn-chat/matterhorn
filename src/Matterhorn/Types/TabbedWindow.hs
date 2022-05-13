{-# LANGUAGE MultiWayIf #-}
module Matterhorn.Types.TabbedWindow
  ( TabbedWindow(..)
  , TabbedWindowEntry(..)
  , TabbedWindowTemplate(..)

  , tabbedWindow
  , getCurrentTabbedWindowEntry
  , tabbedWindowNextTab
  , tabbedWindowPreviousTab
  , runTabShowHandlerFor
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( Widget )
import           Data.List ( nub, elemIndex )
import qualified Data.Text as T
import qualified Graphics.Vty as Vty


-- | An entry in a tabbed window corresponding to a tab and its content.
-- Parameterized over an abstract handle type ('a') for the tabs so we
-- can give each a unique handle.
data TabbedWindowEntry s m n a =
    TabbedWindowEntry { tweValue :: a
                      -- ^ The handle for this tab.
                      , tweRender :: a -> s -> Widget n
                      -- ^ The rendering function to use when this tab
                      -- is selected.
                      , tweHandleEvent :: a -> Vty.Event -> m ()
                      -- ^ The event-handling function to use when this
                      -- tab is selected.
                      , tweTitle :: a -> Bool -> T.Text
                      -- ^ Title function for this tab, with a boolean
                      -- indicating whether this is the current tab.
                      , tweShowHandler :: a -> m ()
                      -- ^ A handler to be invoked when this tab is
                      -- shown.
                      }

-- | The definition of a tabbed window. Note that this does not track
-- the *state* of the window; it merely provides a collection of tab
-- window entries (see above). To track the state of a tabbed window,
-- use a TabbedWindow.
--
-- Parameterized over an abstract handle type ('a') for the tabs so we
-- can give each a unique handle.
data TabbedWindowTemplate s m n a =
    TabbedWindowTemplate { twtEntries :: [TabbedWindowEntry s m n a]
                         -- ^ The entries in tabbed windows with this
                         -- structure.
                         , twtTitle :: a -> Widget n
                         -- ^ The title-rendering function for this kind
                         -- of tabbed window.
                         }

-- | An instantiated tab window. This is based on a template and tracks
-- the state of the tabbed window (current tab).
--
-- Parameterized over an abstract handle type ('a') for the tabs so we
-- can give each a unique handle.
data TabbedWindow s m n a =
    TabbedWindow { twValue :: a
                 -- ^ The handle of the currently-selected tab.
                 , twTemplate :: TabbedWindowTemplate s m n a
                 -- ^ The template to use as a basis for rendering the
                 -- window and handling user input.
                 , twWindowWidth :: Int
                 , twWindowHeight :: Int
                 -- ^ Window dimensions
                 }

-- | Construct a new tabbed window from a template. This will raise an
-- exception if the initially-selected tab does not exist in the window
-- template, or if the window template has any duplicated tab handles.
--
-- Note that the caller is responsible for determining whether to call
-- the initially-selected tab's on-show handler.
tabbedWindow :: (Show a, Eq a)
             => a
             -- ^ The handle corresponding to the tab that should be
             -- selected initially.
             -> TabbedWindowTemplate s m n a
             -- ^ The template for the window to construct.
             -> (Int, Int)
             -- ^ The window dimensions (width, height).
             -> TabbedWindow s m n a
tabbedWindow initialVal t (width, height) =
    let handles = tweValue <$> twtEntries t
    in if | null handles ->
              error "BUG: tabbed window template must provide at least one entry"
          | length handles /= length (nub handles) ->
              error "BUG: tabbed window should have one entry per handle"
          | not (initialVal `elem` handles) ->
              error $ "BUG: tabbed window handle " <>
                      show initialVal <> " not present in template"
          | otherwise ->
              TabbedWindow { twTemplate = t
                           , twValue = initialVal
                           , twWindowWidth = width
                           , twWindowHeight = height
                           }

-- | Get the currently-selected tab entry for a tabbed window. Raise
-- an exception if the window's selected tab handle is not found in its
-- template (which is a bug in the tabbed window infrastructure).
getCurrentTabbedWindowEntry :: (Show a, Eq a)
                            => TabbedWindow s m n a
                            -> TabbedWindowEntry s m n a
getCurrentTabbedWindowEntry w =
    lookupTabbedWindowEntry (twValue w) w

-- | Run the on-show handler for the window tab entry with the specified
-- handle.
runTabShowHandlerFor :: (Eq a, Show a) => a -> TabbedWindow s m n a -> m ()
runTabShowHandlerFor handle w = do
    let entry = lookupTabbedWindowEntry handle w
    tweShowHandler entry handle

-- | Look up a tabbed window entry by handle. Raises an exception if no
-- such entry exists.
lookupTabbedWindowEntry :: (Eq a, Show a)
                        => a
                        -> TabbedWindow s m n a
                        -> TabbedWindowEntry s m n a
lookupTabbedWindowEntry handle w =
    let matchesVal e = tweValue e == handle
    in case filter matchesVal (twtEntries $ twTemplate w) of
        [e] -> e
        _ -> error $ "BUG: tabbed window entry for " <> show (twValue w) <>
                     " should have matched a single entry"

-- | Switch a tabbed window's selected tab to its next tab, cycling back
-- to the first tab if the last tab is the selected tab. This also
-- invokes the on-show handler for the newly-selected tab.
--
-- Note that this does nothing if the window has only one tab.
tabbedWindowNextTab :: (Monad m, Show a, Eq a)
                    => TabbedWindow s m n a
                    -> m (TabbedWindow s m n a)
tabbedWindowNextTab w | length (twtEntries $ twTemplate w) == 1 = return w
tabbedWindowNextTab w = do
    let curIdx = case elemIndex (tweValue curEntry) allHandles of
            Nothing ->
                error $ "BUG: tabbedWindowNextTab: could not find " <>
                        "current handle in handle list"
            Just i -> i
        nextIdx = if curIdx == length allHandles - 1
                  then 0
                  else curIdx + 1
        newHandle = allHandles !! nextIdx
        allHandles = tweValue <$> twtEntries (twTemplate w)
        curEntry = getCurrentTabbedWindowEntry w
        newWin = w { twValue = newHandle }

    runTabShowHandlerFor newHandle newWin
    return newWin

-- | Switch a tabbed window's selected tab to its previous tab, cycling
-- to the last tab if the first tab is the selected tab. This also
-- invokes the on-show handler for the newly-selected tab.
--
-- Note that this does nothing if the window has only one tab.
tabbedWindowPreviousTab :: (Monad m, Show a, Eq a)
                        => TabbedWindow s m n a
                        -> m (TabbedWindow s m n a)
tabbedWindowPreviousTab w | length (twtEntries $ twTemplate w) == 1 = return w
tabbedWindowPreviousTab w = do
    let curIdx = case elemIndex (tweValue curEntry) allHandles of
            Nothing ->
                error $ "BUG: tabbedWindowPreviousTab: could not find " <>
                        "current handle in handle list"
            Just i -> i
        nextIdx = if curIdx == 0
                  then length allHandles - 1
                  else curIdx - 1
        newHandle = allHandles !! nextIdx
        allHandles = tweValue <$> twtEntries (twTemplate w)
        curEntry = getCurrentTabbedWindowEntry w
        newWin = w { twValue = newHandle }

    runTabShowHandlerFor newHandle newWin
    return newWin
