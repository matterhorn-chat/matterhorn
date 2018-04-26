module Types.Common
  ( sanitizeUserText
  )
where

import Prelude ()
import Prelude.MH

import qualified Data.Text as T

import Network.Mattermost.Types ( UserText, unsafeUserText )

sanitizeUserText :: UserText -> T.Text
sanitizeUserText ut =
    T.replace "\ESC" "<ESC>" $
    T.replace "\t" " " $
    unsafeUserText ut
