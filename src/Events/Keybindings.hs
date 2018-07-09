module Events.Keybindings
  ( defaultBindings
  , lookupKeybinding
  , getFirstDefaultBinding

  , mkKb
  , staticKb
  , mkKeybindings

  , handleKeyboardEvent

  -- Re-exports:
  , Keybinding (..)
  , KeyEvent (..)
  , KeyConfig
  , allEvents
  , parseBinding
  , keyEventName
  , keyEventFromName

  )
where

import           Prelude ()
import           Prelude.MH

import qualified Data.Map.Strict as M
import qualified Graphics.Vty as Vty

import           Types
import           Types.KeyEvents


-- * Keybindings

-- | A 'Keybinding' represents a keybinding along with its
--   implementation
data Keybinding =
    KB { kbDescription :: Text
       , kbEvent :: Vty.Event
       , kbAction :: MH ()
       , kbBindingInfo :: Maybe KeyEvent
       }

-- | Find a keybinding that matches a Vty Event
lookupKeybinding :: Vty.Event -> [Keybinding] -> Maybe Keybinding
lookupKeybinding e kbs = case filter ((== e) . kbEvent) kbs of
  []    -> Nothing
  (x:_) -> Just x

handleKeyboardEvent
  :: (KeyConfig -> [Keybinding])
  -> (Vty.Event -> MH ())
  -> Vty.Event
  -> MH ()
handleKeyboardEvent keyList fallthrough e = do
  conf <- use (csResources.crConfiguration)
  let keyMap = keyList (configUserKeys conf)
  case lookupKeybinding e keyMap of
    Just kb -> kbAction kb
    Nothing -> fallthrough e

mkKb :: KeyEvent -> Text -> MH () -> KeyConfig -> [Keybinding]
mkKb ev msg action conf =
  [ KB msg (bindingToEvent key) action (Just ev) | key <- allKeys ]
  where allKeys | Just (BindingList ks) <- M.lookup ev conf = ks
                | Just Unbound <- M.lookup ev conf = []
                | otherwise = defaultBindings ev

staticKb :: Text -> Vty.Event -> MH () -> KeyConfig -> [Keybinding]
staticKb msg event action _ = [KB msg event action Nothing]

mkKeybindings :: [KeyConfig -> [Keybinding]] -> KeyConfig -> [Keybinding]
mkKeybindings ks conf = concat [ k conf | k <- ks ]

bindingToEvent :: Binding -> Vty.Event
bindingToEvent binding =
  Vty.EvKey (kbKey binding) (kbMods binding)

getFirstDefaultBinding :: KeyEvent -> Binding
getFirstDefaultBinding ev =
    case defaultBindings ev of
        [] -> error $ "BUG: event " <> show ev <> " has no default bindings!"
        (b:_) -> b

defaultBindings :: KeyEvent -> [Binding]
defaultBindings ev =
  let meta binding = binding { kbMods = Vty.MMeta : kbMods binding }
      ctrl binding = binding { kbMods = Vty.MCtrl : kbMods binding }
      kb k = Binding { kbMods = [], kbKey = k }
      key c = Binding { kbMods = [], kbKey = Vty.KChar c }
      fn n = Binding { kbMods = [], kbKey = Vty.KFun n }
  in case ev of
        VtyRefreshEvent -> [ ctrl (key 'l') ]
        ShowHelpEvent -> [ fn 1 ]
        EnterSelectModeEvent -> [ ctrl (key 's') ]
        ReplyRecentEvent -> [ ctrl (key 'r') ]
        ToggleMessagePreviewEvent -> [ meta (key 'p') ]
        InvokeEditorEvent -> [ meta (key 'k') ]
        EnterFastSelectModeEvent -> [ ctrl (key 'g') ]
        QuitEvent -> [ ctrl (key 'q') ]
        NextChannelEvent -> [ ctrl (key 'n') ]
        PrevChannelEvent -> [ ctrl (key 'p') ]
        NextUnreadChannelEvent -> [ meta (key 'a') ]
        NextUnreadUserOrChannelEvent -> [ ]
        LastChannelEvent -> [ meta (key 's') ]
        EnterOpenURLModeEvent -> [ ctrl (key 'o') ]
        ClearUnreadEvent -> [ meta (key 'l') ]
        ToggleMultiLineEvent -> [ meta (key 'e') ]
        EnterFlaggedPostsEvent -> [ meta (key '8') ]

        CancelEvent -> [ kb Vty.KEsc
                       , ctrl (key 'c')
                       ]

        -- channel-scroll-specific
        LoadMoreEvent -> [ ctrl (key 'b') ]

        -- scrolling events
        ScrollUpEvent -> [ kb Vty.KUp ]
        ScrollDownEvent -> [ kb Vty.KDown ]
        PageUpEvent -> [ kb Vty.KPageUp ]
        PageDownEvent -> [ kb Vty.KPageDown ]
        ScrollTopEvent -> [ kb Vty.KHome ]
        ScrollBottomEvent -> [ kb Vty.KEnd ]

        SelectUpEvent -> [ key 'k', kb Vty.KUp ]
        SelectDownEvent -> [ key 'j', kb Vty.KDown ]

        ActivateListItemEvent -> [ kb Vty.KEnter ]

        -- search selection - like SelectUp/Down above but need to not
        -- conflict with editor inputs
        SearchSelectUpEvent -> [ ctrl (key 'p'), kb Vty.KUp ]
        SearchSelectDownEvent -> [  ctrl (key 'n'), kb Vty.KDown ]

        ViewMessageEvent    -> [ key 'v' ]
        FlagMessageEvent    -> [ key 'f' ]
        YankMessageEvent    -> [ key 'y' ]
        YankWholeMessageEvent -> [ key 'Y' ]
        DeleteMessageEvent  -> [ key 'd' ]
        EditMessageEvent    -> [ key 'e' ]
        ReplyMessageEvent   -> [ key 'r' ]
        OpenMessageURLEvent -> [ key 'o' ]
