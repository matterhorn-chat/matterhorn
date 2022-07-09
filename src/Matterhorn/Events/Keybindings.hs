module Matterhorn.Events.Keybindings
  ( lookupKeybinding

  , mkKb
  , staticKb
  , mkKeybindings
  , keyHandlerMapPairs

  , handleKeyboardEvent

  , EventHandler(..)
  , KeyHandler(..)
  , KeyEventHandler(..)
  , KeyEventTrigger(..)
  , KeyHandlerMap(..)

  -- Re-exports:
  , module Matterhorn.Types.KeyEvents
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Map.Strict as M
import qualified Graphics.Vty as Vty

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents


-- * Keybindings

-- | An 'EventHandler' represents a event handler.
data EventHandler =
    EH { ehDescription :: Text
       -- ^ The description of this handler's behavior.
       , ehAction :: MH ()
       -- ^ The action to take when this handler is invoked.
       }

-- | A trigger for a key event.
data KeyEventTrigger =
    Static Vty.Event
    -- ^ The key event is always triggered by a specific key.
    | ByEvent KeyEvent
    -- ^ The key event is always triggered by an abstract key event (and
    -- thus configured to be bound to specific key(s) in the KeyConfig).
    deriving (Show, Eq, Ord)

-- | A handler for an abstract key event.
data KeyEventHandler =
    KEH { kehHandler :: EventHandler
        -- ^ The handler to invoke.
        , kehEventTrigger :: KeyEventTrigger
        -- ^ The trigger for the handler.
        }

-- | A handler for a specific key.
data KeyHandler =
    KH { khHandler :: KeyEventHandler
       -- ^ The handler to invoke.
       , khKey :: Vty.Event
       -- ^ The specific key that should trigger this handler.
       }

newtype KeyHandlerMap = KeyHandlerMap (M.Map Vty.Event KeyHandler)

-- | Find a keybinding that matches a Vty Event
lookupKeybinding :: Vty.Event -> KeyHandlerMap -> Maybe KeyHandler
lookupKeybinding e (KeyHandlerMap m) = M.lookup e m

-- | Handle a keyboard event by looking it up in a map of bindings and
-- invoking the matching binding's handler. Return True if the key event
-- was handled with a matching binding; False if not (the fallback
-- case).
handleKeyboardEvent :: (KeyConfig KeyEvent -> KeyHandlerMap)
                    -- ^ The function to build a key handler map from a
                    -- key configuration.
                    -> Vty.Event
                    -- ^ The event to handle.
                    -> MH Bool
handleKeyboardEvent mkKeyMap e = do
  conf <- use (csResources.crConfiguration)
  let keyMap = mkKeyMap (configUserKeys conf)
  case lookupKeybinding e keyMap of
    Just kh -> (ehAction $ kehHandler $ khHandler kh) >> return True
    Nothing -> return False

mkHandler :: Text -> MH () -> EventHandler
mkHandler msg action =
    EH { ehDescription = msg
       , ehAction = action
       }

mkKb :: KeyEvent -> Text -> MH () -> KeyEventHandler
mkKb ev msg action =
    KEH { kehHandler = mkHandler msg action
        , kehEventTrigger = ByEvent ev
        }

keyHandlerFromConfig :: KeyConfig KeyEvent -> KeyEventHandler -> [KeyHandler]
keyHandlerFromConfig kc eh =
    case kehEventTrigger eh of
        Static key ->
            [ KH eh key ]
        ByEvent ev ->
            [ KH eh (bindingToEvent b) | b <- allBindings ]
            where allBindings | Just (BindingList ks) <- lookupKeyConfigBindings kc ev = ks
                              | Just Unbound <- lookupKeyConfigBindings kc ev = []
                              | otherwise = allDefaultBindings kc ev

staticKb :: Text -> Vty.Event -> MH () -> KeyEventHandler
staticKb msg event action =
    KEH { kehHandler = mkHandler msg action
        , kehEventTrigger = Static event
        }

mkKeybindings :: [KeyEventHandler] -> KeyConfig KeyEvent -> KeyHandlerMap
mkKeybindings ks conf = KeyHandlerMap $ M.fromList $ keyHandlerMapPairs ks conf

keyHandlerMapPairs :: [KeyEventHandler] -> KeyConfig KeyEvent -> [(Vty.Event, KeyHandler)]
keyHandlerMapPairs ks conf = pairs
    where
        pairs = mkPair <$> handlers
        mkPair h = (khKey h, h)
        handlers = concat $ keyHandlerFromConfig conf <$> ks

bindingToEvent :: Binding -> Vty.Event
bindingToEvent binding =
  Vty.EvKey (kbKey binding) (kbMods binding)
