module Matterhorn.KeybindingConsistency
  ( ensureKeybindingConsistency
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Map.Strict as M
import           Data.Maybe ( fromJust )
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents
import           Matterhorn.Events.Keybindings

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
