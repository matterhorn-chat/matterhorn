{-# LANGUAGE ScopedTypeVariables #-}
module Matterhorn.Emoji
  ( EmojiCollection
  , loadEmoji
  , emptyEmojiCollection
  , getMatchingEmoji
  , matchesEmoji
  )
where

import           Prelude ()
import           Matterhorn.Prelude

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

-- | Load an EmojiCollection from a JSON disk file.
loadEmoji :: FilePath -> IO (Either String EmojiCollection)
loadEmoji path = runExceptT $ do
    result <- lift $ E.try $ BSL.readFile path
    case result of
        Left (e::E.SomeException) -> throwError $ show e
        Right bs -> do
            EmojiData es <- ExceptT $ return $ A.eitherDecode bs
            return $ EmojiCollection $ T.toLower <$> F.toList es

-- | Look up matching emoji in the collection using the provided search
-- string. This does a case-insensitive infix match. The search string
-- may be provided with or without leading and trailing colons.
lookupEmoji :: EmojiCollection -> T.Text -> [T.Text]
lookupEmoji (EmojiCollection es) search =
    filter (matchesEmoji search) es

-- | Match a search string against an emoji.
matchesEmoji :: T.Text
             -- ^ The search string (will be converted to lowercase and
             -- colons will be removed)
             -> T.Text
             -- ^ The emoji string (assumed to be lowercase and without
             -- leading/trailing colons)
             -> Bool
matchesEmoji searchString e =
    sanitizeEmojiSearch searchString `T.isInfixOf` e

sanitizeEmojiSearch :: T.Text -> T.Text
sanitizeEmojiSearch = stripColons . T.toLower . T.strip

-- | Perform an emoji search against both the local EmojiCollection as
-- well as the server's custom emoji. Return the results, sorted. If the
-- empty string is specified, all local and all custom emoji will be
-- included in the returned list.
getMatchingEmoji :: Session -> EmojiCollection -> T.Text -> IO [T.Text]
getMatchingEmoji session em rawSearchString = do
    let localAlts = lookupEmoji em rawSearchString
        sanitized = sanitizeEmojiSearch rawSearchString
    customResult <- E.try $ case T.null sanitized of
        True -> MM.mmGetListOfCustomEmoji Nothing Nothing session
        False -> MM.mmSearchCustomEmoji sanitized session

    let custom = case customResult of
            Left (_::E.SomeException) -> []
            Right result -> result

    return $ sort $ (MM.emojiName <$> custom) <> localAlts

stripColons :: T.Text -> T.Text
stripColons t =
    stripHeadColon $ stripTailColon t
    where
        stripHeadColon v = if ":" `T.isPrefixOf` v
                           then T.tail v
                           else v
        stripTailColon v = if ":" `T.isSuffixOf` v
                           then T.init v
                           else v
