module Constants
  ( pageAmount
  , userTypingExpiryInterval
  , numScrollbackPosts
  )
where

import Prelude ()
import Prelude.MH


-- | The number of rows to consider a "page" when scrolling
pageAmount :: Int
pageAmount = 15

-- | The expiry interval in seconds for user typing notifications.
userTypingExpiryInterval :: NominalDiffTime
userTypingExpiryInterval = 5

numScrollbackPosts :: Int
numScrollbackPosts = 100

