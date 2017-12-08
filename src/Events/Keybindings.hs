module Events.Keybindings
  ( Keybinding (..)
  , KeyEvent (..)
  , lookupKeybinding
  , KeyConfig
  , allEvents
  , bindingFromString
  , keyEventToString
  , keyEventFromString
  , defaultBindings

  , mkKb
  , staticKb
  , mkKeybindings
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import           Types
import           Types.KeyEvents

-- * Keybindings

-- | A 'Keybinding' represents a keybinding along with its
--   implementation
data Keybinding =
    KB { kbDescription :: T.Text
       , kbEvent :: Vty.Event
       , kbAction :: MH ()
       }

-- | Find a keybinding that matches a Vty Event
lookupKeybinding :: Vty.Event -> [Keybinding] -> Maybe Keybinding
lookupKeybinding e kbs = case filter ((== e) . kbEvent) kbs of
  []    -> Nothing
  (x:_) -> Just x

mkKb :: KeyEvent -> T.Text -> MH () -> KeyConfig -> Keybinding
mkKb ev msg action conf = KB msg (bindingToEvent key) action
  where key | Just k <- M.lookup ev conf = k
            | Just k <- M.lookup ev defaultBindings = k
            | otherwise = undefined

staticKb :: T.Text -> Vty.Event -> MH () -> KeyConfig -> Keybinding
staticKb msg event action _ = KB msg event action

mkKeybindings :: [KeyConfig -> Keybinding] -> KeyConfig -> [Keybinding]
mkKeybindings ks conf = [ k conf | k <- ks ]

bindingToEvent :: Binding -> Vty.Event
bindingToEvent binding =
  Vty.EvKey (kbKey binding) (kbMods binding)

defaultBindings :: M.Map KeyEvent Binding
defaultBindings =
  let meta binding = binding { kbMods = Vty.MMeta : kbMods binding }
      ctrl binding = binding { kbMods = Vty.MCtrl : kbMods binding }
      kb k = Binding { kbMods = [], kbKey = k }
      key c = Binding { kbMods = [], kbKey = Vty.KChar c }
      fn n = Binding { kbMods = [], kbKey = Vty.KFun n }
  in M.fromList
      [ ( VtyRefreshEvent, ctrl (key 'l') )
      , ( ShowHelpEvent, fn 1 )
      , ( EnterSelectModeEvent, ctrl (key 's') )
      , ( ReplyRecentEvent, ctrl (key 'r') )
      , ( ToggleMessagePreviewEvent, meta (key 'p') )
      , ( InvokeEditorEvent, meta (key 'k') )
      , ( EnterFastSelectModeEvent, ctrl (key 'g') )
      , ( QuitEvent, ctrl (key 'q') )
      , ( NextChannelEvent, ctrl (key 'n') )
      , ( PrevChannelEvent, ctrl (key 'p') )
      , ( NextUnreadChannelEvent, meta (key 'a') )
      , ( LastChannelEvent, meta (key 's') )
      , ( EnterOpenURLModeEvent, ctrl (key 'o') )
      , ( ClearUnreadEvent, meta (key 'l') )
      , ( ToggleMultiLineEvent, meta (key 'e') )
      , ( CancelReplyEvent, kb Vty.KEsc )
      , ( EnterFlaggedPostsEvent, meta (key '8') )
      ]
