{-# LANGUAGE MultiWayIf #-}
module Types.Common
  ( sanitizeUserText
  , sanitizeUserText'
  , userIdForDMChannel
  )
where

import Prelude ()
import Prelude.MH

import qualified Data.Text as T

import Network.Mattermost.Types ( UserText, unsafeUserText, UserId(..), Id(..) )

sanitizeUserText :: UserText -> T.Text
sanitizeUserText = sanitizeUserText' . unsafeUserText

sanitizeUserText' :: T.Text -> T.Text
sanitizeUserText' =
    T.replace "\ESC" "<ESC>" .
    T.replace "\t" " " .
    T.filter (\c -> c >= ' ' || c == '\n')  -- remove non-printable

-- | Extract the corresponding other user from a direct channel name.
-- Returns Nothing if the string is not a direct channel name or if it
-- is but neither user ID in the name matches the current user's ID.
userIdForDMChannel :: UserId
                   -- ^ My user ID
                   -> Text
                   -- ^ The channel name
                   -> Maybe UserId
userIdForDMChannel me chanName =
    -- Direct channel names are of the form "UID__UID" where one of the
    -- UIDs is mine and the other is the other channel participant.
    let vals = T.splitOn "__" chanName
    in case vals of
        [u1, u2] -> if | (UI $ Id u1) == me  -> Just $ UI $ Id u2
                       | (UI $ Id u2) == me  -> Just $ UI $ Id u1
                       | otherwise        -> Nothing
        _ -> Nothing
