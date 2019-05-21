{-# LANGUAGE ScopedTypeVariables #-}
module Emoji
  ( EmojiCollection
  , loadEmoji
  , emptyEmojiCollection
  , lookupEmoji
  , getMatchingEmoji
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Control.Exception as E
import           Control.Monad.Except
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Sequence as Seq

import           Network.Mattermost.Types ( Session )
import qualified Network.Mattermost.Endpoints as MM


newtype EmojiData = EmojiData (Seq.Seq T.Text)

-- | The collection of all emoji names we loaded from a JSON disk file.
-- You might rightly ask: why don't we use a Trie here, for efficient
-- lookups? The answer is that we need infix lookups; prefix matches are
-- not enough. In practice it seems not to matter that much; despite the
-- O(n) search we get good enough performance that we aren't worried
-- about this. If at some point this becomes an issue, other data
-- structures with good infix lookup performance should be identified
-- (full-text search, perhaps?).
newtype EmojiCollection = EmojiCollection [T.Text]

instance A.FromJSON EmojiData where
    parseJSON = A.withArray "EmojiData" $ \v -> do
        aliasVecs <- forM v $ \val ->
            flip (A.withObject "EmojiData Entry") val $ \obj -> do
                as <- obj A..: "aliases"
                forM as $ A.withText "Alias list element" return

        return $ EmojiData $ mconcat $ F.toList aliasVecs

emptyEmojiCollection :: EmojiCollection
emptyEmojiCollection = EmojiCollection mempty

loadEmoji :: FilePath -> IO (Either String EmojiCollection)
loadEmoji path = runExceptT $ do
    result <- lift $ E.try $ BSL.readFile path
    case result of
        Left (e::E.SomeException) -> throwError $ show e
        Right bs -> do
            EmojiData es <- ExceptT $ return $ A.eitherDecode bs
            return $ EmojiCollection $ F.toList es

lookupEmoji :: EmojiCollection -> T.Text -> [T.Text]
lookupEmoji (EmojiCollection es) search =
    filter (\e -> T.toLower search `T.isInfixOf` e) es

getMatchingEmoji :: Session -> EmojiCollection -> T.Text -> IO [T.Text]
getMatchingEmoji session em searchString = do
    let localAlts = lookupEmoji em searchString
    custom <- case T.null searchString of
        True -> MM.mmGetListOfCustomEmoji Nothing Nothing session
        False -> MM.mmSearchCustomEmoji searchString session
    return $ sort $ (MM.emojiName <$> custom) <> localAlts
