{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.ListWindow
  ( listWindowActivateCurrent
  , listWindowActivate
  , listWindowSearchString
  , listWindowMove
  , exitListWindow
  , enterListWindowMode
  , resetListWindowSearch
  , onEventListWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick ( BrickEvent(VtyEvent) )
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Data.Text.Zipper as Z
import qualified Data.Vector as Vec
import           Lens.Micro.Platform ( Lens', (%=), (.=) )
import           Network.Mattermost.Types ( Session, TeamId )
import qualified Graphics.Vty as Vty

import           Matterhorn.Types
import           Brick.Keybindings
import           Matterhorn.State.Common
import           Matterhorn.State.Editing ( editingKeybindings )


-- | Activate the specified list window's selected item by invoking the
-- window's configured enter keypress handler function.
listWindowActivateCurrent :: TeamId -> Lens' ChatState (ListWindowState a b) -> MH ()
listWindowActivateCurrent tId which = do
  mItem <- L.listSelectedElement <$> use (which.listWindowSearchResults)
  case mItem of
      Nothing -> return ()
      Just (_, val) -> listWindowActivate tId which val

-- | Activate the specified list window's selected item by invoking the
-- window's configured enter keypress handler function.
listWindowActivate :: TeamId -> Lens' ChatState (ListWindowState a b) -> a -> MH ()
listWindowActivate tId which val = do
    handler <- use (which.listWindowEnterHandler)
    activated <- handler val
    if activated
       then popMode tId
       else return ()

-- | Get the current search string for the specified window.
listWindowSearchString :: Lens' ChatState (ListWindowState a b) -> MH Text
listWindowSearchString which =
    (head . E.getEditContents) <$> use (which.listWindowSearchInput)

-- | Move the list cursor in the specified window.
listWindowMove :: Lens' ChatState (ListWindowState a b)
                -- ^ Which window
                -> (L.List Name a -> L.List Name a)
                -- ^ How to transform the list in the window
                -> MH ()
listWindowMove which how = which.listWindowSearchResults %= how

-- | Clear the state of the specified list window and return to the
-- Main mode.
exitListWindow :: TeamId
                -> Lens' ChatState (ListWindowState a b)
                -- ^ Which window to reset
                -> MH ()
exitListWindow tId which = do
    newList <- use (which.listWindowNewList)
    which.listWindowSearchResults .= newList mempty
    which.listWindowEnterHandler .= (const $ return False)
    popMode tId

-- | Initialize a list window with the specified arguments and switch
-- to the specified mode.
enterListWindowMode :: TeamId
                     -> (Lens' ChatState (ListWindowState a b))
                     -- ^ Which window to initialize
                     -> Mode
                     -- ^ The mode to change to
                     -> b
                     -- ^ The window's initial search scope
                     -> (a -> MH Bool)
                     -- ^ The window's enter keypress handler
                     -> (b -> Session -> Text -> IO (Vec.Vector a))
                     -- ^ The window's results fetcher function
                     -> MH ()
enterListWindowMode tId which mode scope enterHandler fetcher = do
    which.listWindowSearchScope .= scope
    which.listWindowSearchInput.E.editContentsL %= Z.clearZipper
    which.listWindowEnterHandler .= enterHandler
    which.listWindowFetchResults .= fetcher
    which.listWindowSearching .= False
    newList <- use (which.listWindowNewList)
    which.listWindowSearchResults .= newList mempty
    pushMode tId mode
    resetListWindowSearch which

-- | Reset the window's search by initiating a new search request for
-- the string that is currently in the window's editor. This does
-- nothing if a search for this window is already in progress.
resetListWindowSearch :: Lens' ChatState (ListWindowState a b) -> MH ()
resetListWindowSearch which = do
    searchPending <- use (which.listWindowSearching)

    when (not searchPending) $ do
        searchString <- listWindowSearchString which
        which.listWindowSearching .= True
        newList <- use (which.listWindowNewList)
        session <- getSession
        scope <- use (which.listWindowSearchScope)
        fetcher <- use (which.listWindowFetchResults)
        doAsyncWith Preempt $ do
            results <- fetcher scope session searchString
            return $ Just $ do
                which.listWindowSearchResults .= newList results
                which.listWindowSearching .= False

                -- Now that the results are available, check to see if the
                -- search string changed since this request was submitted.
                -- If so, issue another search.
                afterSearchString <- listWindowSearchString which
                when (searchString /= afterSearchString) $ resetListWindowSearch which

-- | Generically handle an event for the list window state targeted
-- by the specified lens. Automatically dispatches new searches in the
-- window's editor if the editor contents change.
onEventListWindow :: Lens' ChatState (ListWindowState a b)
                   -- ^ Which window to dispatch to?
                   -> (KeyConfig KeyEvent -> KeyDispatcher KeyEvent MH)
                   -- ^ The keybinding builder
                   -> Vty.Event
                   -- ^ The event
                   -> MH Bool
onEventListWindow which keybindings =
    handleEventWith [ mhHandleKeyboardEvent keybindings
                    , handleEditorEvent which
                    ]

handleEditorEvent :: Lens' ChatState (ListWindowState a b) -> Vty.Event -> MH Bool
handleEditorEvent which e = do
    -- Get the editor content before the event.
    before <- listWindowSearchString which

    -- First find a matching keybinding in the keybinding list.
    handled <- mhHandleKeyboardEvent (editingKeybindings (which.listWindowSearchInput)) e

    -- If we didn't find a matching binding, just handle the event as a
    -- normal editor input event.
    when (not handled) $
        mhZoom (which.listWindowSearchInput) E.handleEditorEvent (VtyEvent e)

    -- Get the editor content after the event. If the string changed,
    -- start a new search.
    after <- listWindowSearchString which
    when (before /= after) $ resetListWindowSearch which

    return True
