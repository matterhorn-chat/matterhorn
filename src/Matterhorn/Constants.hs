module Matterhorn.Constants
  ( pageAmount
  , userTypingExpiryInterval
  , numScrollbackPosts
  , previewMaxHeight
  , normalChannelSigil
  , normalChannelSigilChar
  , userSigil
  , userSigilChar
  , editMarking
  , channelListMinAutoWidth
  , channelListMaxAutoWidth
  )
where

import Prelude ()
import Matterhorn.Prelude

import qualified Data.Text as T


-- | The minimum channel list width when the width is set to 'auto'.
channelListMinAutoWidth :: Int
channelListMinAutoWidth = 24

-- | The maximum channel list width when the width is set to 'auto'.
channelListMaxAutoWidth :: Int
channelListMaxAutoWidth = 44

-- | The number of rows to consider a "page" when scrolling
pageAmount :: Int
pageAmount = 15

-- | The expiry interval in seconds for user typing notifications.
userTypingExpiryInterval :: NominalDiffTime
userTypingExpiryInterval = 5

numScrollbackPosts :: Int
numScrollbackPosts = 100

-- | The maximum height of the message preview, in lines.
previewMaxHeight :: Int
previewMaxHeight = 5

-- Sigils
normalChannelSigil :: Text
normalChannelSigil = T.singleton normalChannelSigilChar

normalChannelSigilChar :: Char
normalChannelSigilChar = '~'

userSigil :: Text
userSigil = T.singleton userSigilChar

userSigilChar :: Char
userSigilChar = '@'

editMarking :: Text
editMarking = "(edited)"
