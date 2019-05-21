{-# LANGUAGE ScopedTypeVariables #-}
module Emoji
  ( EmojiCollection
  , loadEmoji
  , emptyEmojiCollection
  , lookupEmoji
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
import qualified Data.Text.Encoding as T
import qualified Data.Trie as DT
import qualified Data.Sequence as Seq


newtype EmojiData = EmojiData (Seq.Seq T.Text)
newtype EmojiCollection = EmojiCollection (DT.Trie T.Text)

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
            let entries = [(T.encodeUtf8 t, t) | t <- F.toList es]
            return $ EmojiCollection $ DT.fromList entries

lookupEmoji :: EmojiCollection -> T.Text -> [T.Text]
lookupEmoji (EmojiCollection tr) search =
    DT.lookupBy f (T.encodeUtf8 search) tr
    where
        f Nothing sub = F.toList sub
        f (Just v) sub = v : F.toList sub
