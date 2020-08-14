module Matterhorn.Constants
  ( pageAmount
  , userTypingExpiryInterval
  , numScrollbackPosts
  , previewMaxHeight
  , normalChannelSigil
  , userSigil
  )
where

import Prelude ()
import Matterhorn.Prelude


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
normalChannelSigil = "~"

userSigil :: Text
userSigil = "@"
