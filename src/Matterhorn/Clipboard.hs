module Matterhorn.Clipboard
  ( copyToClipboard
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Control.Exception ( try )
import qualified Data.Text as T
import           System.Hclip ( setClipboard, ClipboardException(..) )

import           Matterhorn.Types


copyToClipboard :: Text -> MH ()
copyToClipboard txt = do
  result <- liftIO (try (setClipboard (T.unpack txt)))
  case result of
    Left e -> do
      let errMsg = case e of
            UnsupportedOS _ ->
              "Matterhorn does not support yanking on this operating system."
            NoTextualData ->
              "Textual data is required to set the clipboard."
            MissingCommands cmds ->
              "Could not set clipboard due to missing one of the " <>
              "required program(s): " <> (T.pack $ show cmds)
      mhError $ ClipboardError errMsg
    Right () ->
      return ()
