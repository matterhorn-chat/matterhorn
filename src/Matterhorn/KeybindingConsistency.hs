module Matterhorn.KeybindingConsistency
  ( ensureKeybindingConsistency
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Keybindings

import qualified Data.Text as T

import           Matterhorn.Types

-- | Given a configuration, we want to check it for internal consistency
-- (i.e. that a given keybinding isn't associated with multiple events
-- which both need to get generated in the same UI mode).
ensureKeybindingConsistency :: KeyConfig KeyEvent
                            -> [(T.Text, [MHKeyEventHandler])]
                            -> Either String ()
ensureKeybindingConsistency kc modeMaps = do
    forM_ modeMaps $ \(mode, handlers) -> do
        case keyDispatcher kc handlers of
            Left conflicts ->
                Left $ T.unpack $
                    "Key binding conflict in '" <> mode <> "' mode:\n" <>
                    bindingConflictMessage kc conflicts
            Right {} -> return ()
