module Types.Common
  ( sanitizeUserText
  , sanitizeUserText'
  , sanitizeChar
  )
where

import Prelude ()
import Prelude.MH

import qualified Data.Text as T

import Network.Mattermost.Types ( UserText, unsafeUserText )

sanitizeUserText :: UserText -> T.Text
sanitizeUserText = sanitizeUserText' . unsafeUserText

sanitizeUserText' :: T.Text -> T.Text
sanitizeUserText' t =
    T.replace "\ESC" "<ESC>" $
    T.replace "\t" " " t

sanitizeChar :: Char -> T.Text
sanitizeChar '\ESC' = "<ESC>"
sanitizeChar '\t' = " "
sanitizeChar c = T.singleton c
