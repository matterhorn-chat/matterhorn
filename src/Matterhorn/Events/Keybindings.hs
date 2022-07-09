module Matterhorn.Events.Keybindings
  ( lookupKeybinding

  , mkKb
  , staticKb
  , mkKeybindings

  , handleKeyboardEvent

  , EventHandler(..)
  , KeyHandler(..)
  , KeyEventHandler(..)
  , KeyEventTrigger(..)
  , KeyHandlerMap(..)

  , ensureKeybindingConsistency

  -- Re-exports:
  , module Matterhorn.Types.KeyEvents
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust)
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

-- | Given a configuration, we want to check it for internal consistency
-- (i.e. that a given keybinding isn't associated with multiple events
-- which both need to get generated in the same UI mode) and also for
-- basic usability (i.e. we shouldn't be binding events which can appear
-- in the main UI to a key like @e@, which would prevent us from being
-- able to type messages containing an @e@ in them!
ensureKeybindingConsistency :: KeyConfig KeyEvent -> [(T.Text, [KeyEventHandler])] -> Either String ()
ensureKeybindingConsistency kc modeMaps = mapM_ checkGroup allBindings
  where
    -- This is a list of lists, grouped by keybinding, of all the
    -- keybinding/event associations that are going to be used with the
    -- provided key configuration.
    allBindings = groupWith fst $ concat
      [ case lookupKeyConfigBindings kc ev of
          Nothing -> zip (allDefaultBindings kc ev) (repeat (False, ev))
          Just (BindingList bs) -> zip bs (repeat (True, ev))
          Just Unbound -> []
      | (_, ev) <- keyEventsList (keyConfigEvents kc)
      ]

    -- The invariant here is that each call to checkGroup is made with a
    -- list where the first element of every list is the same binding.
    -- The Bool value in these is True if the event was associated with
    -- the binding by the user, and False if it's a Matterhorn default.
    checkGroup :: [(Binding, (Bool, KeyEvent))] -> Either String ()
    checkGroup [] = error "[ensureKeybindingConsistency: unreachable]"
    checkGroup evs@((b, _):_) = do

      -- We find out which modes an event can be used in and then invert
      -- the map, so this is a map from mode to the events contains
      -- which are bound by the binding included above.
      let modesFor :: M.Map T.Text [(Bool, KeyEvent)]
          modesFor = M.unionsWith (++)
            [ M.fromList [ (m, [(i, ev)]) | m <- modeMap ev ]
            | (_, (i, ev)) <- evs
            ]

      -- If there is ever a situation where the same key is bound to two
      -- events which can appear in the same mode, then we want to throw
      -- an error, and also be informative about why. It is still okay
      -- to bind the same key to two events, so long as those events
      -- never appear in the same UI mode.
      forM_ (M.assocs modesFor) $ \ (_, vs) ->
         when (length vs > 1) $
           Left $ concat $
             "Multiple overlapping key events bound to `" :
             T.unpack (ppBinding b) :
             "`:\n" :
             concat [ [ " - `"
                      , T.unpack (fromJust $ keyEventName (keyConfigEvents kc) ev)
                      , "` "
                      , if isFromUser
                          then "(via user configuration)"
                          else "(matterhorn default)"
                      , "\n"
                      ]
                    | (isFromUser, ev) <- vs
                    ]

      -- Check for overlap a set of built-in keybindings when we're in a
      -- mode where the user is typing. (These are perfectly fine when
      -- we're in other modes.)
      when ("main" `M.member` modesFor && isBareBinding b) $ do
        Left $ concat $
          [ "The keybinding `"
          , T.unpack (ppBinding b)
          , "` is bound to the "
          , case map (ppEvent . snd . snd) evs of
              [] -> error "unreachable"
              [e] -> "event " ++ e
              es  -> "events " ++ intercalate " and " es
          , "\n"
          , "This is probably not what you want, as it will interfere "
          , "with the ability to write messages!\n"
          ]

    -- Events get some nice formatting!
    ppEvent ev = "`" ++ T.unpack (fromJust $ keyEventName (keyConfigEvents kc) ev) ++ "`"

    -- This check should get more nuanced, but as a first approximation,
    -- we shouldn't bind to any bare character key in the main mode.
    isBareBinding (Binding [] (Vty.KChar {})) = True
    isBareBinding _ = False

    -- We generate the which-events-are-valid-in-which-modes map from
    -- our actual keybinding set, so this should never get out of date.
    modeMap :: KeyEvent -> [T.Text]
    modeMap ev =
      let matches kh = ByEvent ev == (kehEventTrigger $ khHandler kh)
      in [ mode
         | (mode, handlers) <- modeMaps
         , let pairs = keyHandlerMapPairs handlers kc
           in not $ null $ filter matches $ snd <$> pairs
         ]
