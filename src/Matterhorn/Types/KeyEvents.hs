module Matterhorn.Types.KeyEvents
  (
  -- * Key binding configurations
    KeyConfig(keyConfigEvents)
  , Binding(..)
  , BindingState(..)
  , newKeyConfig
  , lookupKeyConfigBindings

  -- * Querying key binding configurations
  , getFirstDefaultBinding
  , firstActiveBinding
  , allDefaultBindings

  -- * Parsing and pretty-printing of bindings
  , parseBinding
  , parseBindingList
  , ppBinding
  , ppMaybeBinding
  , nonCharKeys
  , eventToBinding

  -- * Key event collections
  , KeyEvents
  , keyEvents
  , keyEventsList
  , lookupKeyEvent
  , keyEventName

  -- * Key event handler maps
  , KeyHandlerMap(..)
  , mkKeybindings
  , lookupKeybinding
  , Handler(..)
  , KeyHandler(..)
  , KeyEventHandler(..)
  , KeyEventTrigger(..)

  -- * Building handlers
  , mkKb
  , staticKb
  , keyHandlerMapPairs

  -- * Handling events
  , handleKeyboardEvent
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Bimap as B
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as Vty


data KeyEvents e = KeyEvents (B.Bimap Text e)
                 deriving (Eq, Show)

keyEvents :: (Ord e) => [(Text, e)] -> KeyEvents e
keyEvents pairs = KeyEvents $ B.fromList pairs

keyEventsList :: KeyEvents e -> [(Text, e)]
keyEventsList (KeyEvents m) = B.toList m

eventToBinding :: Vty.Event -> Binding
eventToBinding (Vty.EvKey k mods) = Binding mods k
eventToBinding k = error $ "BUG: invalid keybinding " <> show k

data Binding =
    Binding { kbMods :: [Vty.Modifier]
            , kbKey  :: Vty.Key
            } deriving (Eq, Show, Ord)

data BindingState =
    BindingList [Binding]
    | Unbound
    deriving (Show, Eq, Ord)

-- | A configuration of custom key bindings.
data KeyConfig e =
    KeyConfig { keyConfigBindingMap :: M.Map e BindingState
              -- ^ The map of custom bindings for events with custom
              -- bindings
              , keyConfigEvents :: KeyEvents e
              -- ^ The base mapping of events and their names that is
              -- used in this configuration
              , keyConfigDefaultBindings :: M.Map e [Binding]
              -- ^ A mapping of events and their default key bindings,
              -- if any
              }
              deriving (Show, Eq)

newKeyConfig :: (Ord e)
             => KeyEvents e
             -- ^ The base mapping of key events to use
             -> [(e, BindingState)]
             -- ^ Custom bindings by key event
             -> [(e, [Binding])]
             -- ^ Default bindings by key event
             -> KeyConfig e
newKeyConfig evs bindings defaults =
    KeyConfig { keyConfigBindingMap = M.fromList bindings
              , keyConfigEvents = evs
              , keyConfigDefaultBindings = M.fromList defaults
              }

getFirstDefaultBinding :: (Show e, Ord e) => KeyConfig e -> e -> Maybe Binding
getFirstDefaultBinding kc ev = do
    bs <- M.lookup ev (keyConfigDefaultBindings kc)
    case bs of
        (b:_) -> Just b
        _ -> Nothing

allDefaultBindings :: (Ord e) => KeyConfig e -> e -> [Binding]
allDefaultBindings kc ev =
    fromMaybe [] $ M.lookup ev (keyConfigDefaultBindings kc)

firstActiveBinding :: (Show e, Ord e) => KeyConfig e -> e -> Maybe Binding
firstActiveBinding kc ev = foundBinding <|> defaultBinding
    where
        defaultBinding = getFirstDefaultBinding kc ev
        foundBinding = do
            bState <- lookupKeyConfigBindings kc ev
            case bState of
                BindingList (b:_) -> Just b
                _ -> Nothing

lookupKeyConfigBindings :: (Ord e) => KeyConfig e -> e -> Maybe BindingState
lookupKeyConfigBindings kc e = M.lookup e $ keyConfigBindingMap kc

parseBinding :: Text -> Either String Binding
parseBinding kb = go (T.splitOn "-" $ T.toLower kb) []
  where go [k] mods = do
          key <- pKey k
          return Binding { kbMods = mods, kbKey = key }
        go (k:ks) mods = do
          m <- case k of
            "s"       -> return Vty.MShift
            "shift"   -> return Vty.MShift
            "m"       -> return Vty.MMeta
            "meta"    -> return Vty.MMeta
            "a"       -> return Vty.MAlt
            "alt"     -> return Vty.MAlt
            "c"       -> return Vty.MCtrl
            "ctrl"    -> return Vty.MCtrl
            "control" -> return Vty.MCtrl
            _         -> Left ("Unknown modifier prefix: " ++ show k)
          go ks (m:mods)
        go [] _ = Left "Empty keybinding not allowed"
        pKey "esc"       = return Vty.KEsc
        pKey "backspace" = return Vty.KBS
        pKey "enter"     = return Vty.KEnter
        pKey "left"      = return Vty.KLeft
        pKey "right"     = return Vty.KRight
        pKey "up"        = return Vty.KUp
        pKey "down"      = return Vty.KDown
        pKey "upleft"    = return Vty.KUpLeft
        pKey "upright"   = return Vty.KUpRight
        pKey "downleft"  = return Vty.KDownLeft
        pKey "downright" = return Vty.KDownRight
        pKey "center"    = return Vty.KCenter
        pKey "backtab"   = return Vty.KBackTab
        pKey "printscreen" = return Vty.KPrtScr
        pKey "pause"     = return Vty.KPause
        pKey "insert"    = return Vty.KIns
        pKey "home"      = return Vty.KHome
        pKey "pgup"      = return Vty.KPageUp
        pKey "del"       = return Vty.KDel
        pKey "end"       = return Vty.KEnd
        pKey "pgdown"    = return Vty.KPageDown
        pKey "begin"     = return Vty.KBegin
        pKey "menu"      = return Vty.KMenu
        pKey "space"     = return (Vty.KChar ' ')
        pKey "tab"       = return (Vty.KChar '\t')
        pKey t
          | Just (c, "") <- T.uncons t =
              return (Vty.KChar c)
          | Just n <- T.stripPrefix "f" t =
              case readMaybe (T.unpack n) of
                  Nothing -> Left ("Unknown keybinding: " ++ show t)
                  Just i -> return (Vty.KFun i)
          | otherwise = Left ("Unknown keybinding: " ++ show t)

ppBinding :: Binding -> Text
ppBinding (Binding mods k) =
    T.intercalate "-" $ (ppMod <$> mods) <> [ppKey k]

ppMaybeBinding :: Maybe Binding -> Text
ppMaybeBinding Nothing =
    "(no binding)"
ppMaybeBinding (Just b) =
    ppBinding b

ppKey :: Vty.Key -> Text
ppKey (Vty.KChar c)   = ppChar c
ppKey (Vty.KFun n)    = "F" <> (T.pack $ show n)
ppKey Vty.KBackTab    = "BackTab"
ppKey Vty.KEsc        = "Esc"
ppKey Vty.KBS         = "Backspace"
ppKey Vty.KEnter      = "Enter"
ppKey Vty.KUp         = "Up"
ppKey Vty.KDown       = "Down"
ppKey Vty.KLeft       = "Left"
ppKey Vty.KRight      = "Right"
ppKey Vty.KHome       = "Home"
ppKey Vty.KEnd        = "End"
ppKey Vty.KPageUp     = "PgUp"
ppKey Vty.KPageDown   = "PgDown"
ppKey Vty.KDel        = "Del"
ppKey Vty.KUpLeft     = "UpLeft"
ppKey Vty.KUpRight    = "UpRight"
ppKey Vty.KDownLeft   = "DownLeft"
ppKey Vty.KDownRight  = "DownRight"
ppKey Vty.KCenter     = "Center"
ppKey Vty.KPrtScr     = "PrintScreen"
ppKey Vty.KPause      = "Pause"
ppKey Vty.KIns        = "Insert"
ppKey Vty.KBegin      = "Begin"
ppKey Vty.KMenu       = "Menu"

nonCharKeys :: [Text]
nonCharKeys = map ppKey
  [ Vty.KBackTab, Vty.KEsc, Vty.KBS, Vty.KEnter, Vty.KUp, Vty.KDown
  , Vty.KLeft, Vty.KRight, Vty.KHome, Vty.KEnd, Vty.KPageDown
  , Vty.KPageUp, Vty.KDel, Vty.KUpLeft, Vty.KUpRight, Vty.KDownLeft
  , Vty.KDownRight, Vty.KCenter, Vty.KPrtScr, Vty.KPause, Vty.KIns
  , Vty.KBegin, Vty.KMenu
  ]

ppChar :: Char -> Text
ppChar '\t' = "Tab"
ppChar ' '  = "Space"
ppChar c    = T.singleton c

ppMod :: Vty.Modifier -> Text
ppMod Vty.MMeta  = "M"
ppMod Vty.MAlt   = "A"
ppMod Vty.MCtrl  = "C"
ppMod Vty.MShift = "S"

parseBindingList :: Text -> Either String BindingState
parseBindingList t =
    if T.toLower t == "unbound"
    then return Unbound
    else BindingList <$> mapM (parseBinding . T.strip) (T.splitOn "," t)

lookupKeyEvent :: (Ord e) => KeyEvents e -> Text -> Maybe e
lookupKeyEvent (KeyEvents m) name = B.lookup name m

keyEventName :: (Ord e) => KeyEvents e -> e -> Maybe Text
keyEventName (KeyEvents m) e = B.lookupR e m

-- | An 'Handler' represents a handler implementation to be invoked in
-- response to some event.
data Handler m =
    EH { ehDescription :: Text
       -- ^ The description of this handler's behavior.
       , ehAction :: m ()
       -- ^ The action to take when this handler is invoked.
       }

-- | A trigger for a key event.
data KeyEventTrigger e =
    Static Vty.Event
    -- ^ The key event is always triggered by a specific key.
    | ByEvent e
    -- ^ The key event is always triggered by an abstract key event (and
    -- thus configured to be bound to specific key(s) in the KeyConfig).
    deriving (Show, Eq, Ord)

-- | A handler for an abstract key event.
data KeyEventHandler e m =
    KEH { kehHandler :: Handler m
        -- ^ The handler to invoke.
        , kehEventTrigger :: KeyEventTrigger e
        -- ^ The trigger for the handler.
        }

-- | A handler for a specific key.
data KeyHandler e m =
    KH { khHandler :: KeyEventHandler e m
       -- ^ The handler to invoke.
       , khKey :: Vty.Event
       -- ^ The specific key that should trigger this handler.
       }

newtype KeyHandlerMap e m = KeyHandlerMap (M.Map Vty.Event (KeyHandler e m))

-- | Find a keybinding that matches a Vty Event
lookupKeybinding :: Vty.Event -> KeyHandlerMap e m -> Maybe (KeyHandler e m)
lookupKeybinding e (KeyHandlerMap m) = M.lookup e m

-- | Handle a keyboard event by looking it up in a map of bindings and
-- invoking the matching binding's handler. Return True if the key event
-- was handled with a matching binding; False if not (the fallback
-- case).
handleKeyboardEvent :: (Monad m)
                    => KeyConfig e
                    -- ^ The key configuration to use
                    -> (KeyConfig e -> KeyHandlerMap e m)
                    -- ^ The function to build a key handler map from a
                    -- key configuration.
                    -> Vty.Event
                    -- ^ The event to handle.
                    -> m Bool
handleKeyboardEvent kc mkKeyMap e = do
  let handlerMap = mkKeyMap kc
  case lookupKeybinding e handlerMap of
    Just kh -> (ehAction $ kehHandler $ khHandler kh) >> return True
    Nothing -> return False

mkHandler :: Text -> m () -> Handler m
mkHandler msg action =
    EH { ehDescription = msg
       , ehAction = action
       }

mkKb :: e -> Text -> m () -> KeyEventHandler e m
mkKb ev msg action =
    KEH { kehHandler = mkHandler msg action
        , kehEventTrigger = ByEvent ev
        }

keyHandlerFromConfig :: (Ord e)
                     => KeyConfig e
                     -> KeyEventHandler e m
                     -> [KeyHandler e m]
keyHandlerFromConfig kc eh =
    case kehEventTrigger eh of
        Static key ->
            [ KH eh key ]
        ByEvent ev ->
            [ KH eh (bindingToEvent b) | b <- allBindings ]
            where allBindings | Just (BindingList ks) <- lookupKeyConfigBindings kc ev = ks
                              | Just Unbound <- lookupKeyConfigBindings kc ev = []
                              | otherwise = allDefaultBindings kc ev

staticKb :: Text -> Vty.Event -> m () -> KeyEventHandler e m
staticKb msg event action =
    KEH { kehHandler = mkHandler msg action
        , kehEventTrigger = Static event
        }

mkKeybindings :: (Ord e)
              => [KeyEventHandler e m]
              -> KeyConfig e
              -> KeyHandlerMap e m
mkKeybindings ks conf = KeyHandlerMap $ M.fromList $ keyHandlerMapPairs ks conf

keyHandlerMapPairs :: (Ord e)
                   => [KeyEventHandler e m]
                   -> KeyConfig e
                   -> [(Vty.Event, KeyHandler e m)]
keyHandlerMapPairs ks conf = pairs
    where
        pairs = mkPair <$> handlers
        mkPair h = (khKey h, h)
        handlers = concat $ keyHandlerFromConfig conf <$> ks

bindingToEvent :: Binding -> Vty.Event
bindingToEvent binding =
  Vty.EvKey (kbKey binding) (kbMods binding)
