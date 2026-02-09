{-# LANGUAGE RankNTypes #-}
module Matterhorn.State.UrlSelect
  (
  -- * URL selection modes
    startTopicUrlSelect
  , startMessageUrlSelect
  , stopUrlSelect
  , openSelectedURL
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.List ( listSelectedElement, listReplace )
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Vector as V
import           Lens.Micro.Platform ( (.=), (%=), to, Lens' )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.Links
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( TeamBaseURL, parseMarkdown, unBlocks, blockGetURLs )
import           Matterhorn.Util


startMessageUrlSelect :: Lens' ChatState (MessageInterface n i)
                      -> MH ()
startMessageUrlSelect which = do
    msgs <- use (which.miListing.mlMessages)
    let urls = V.fromList $ findMessageUrls msgs
    startUrlSelect which urls

startTopicUrlSelect :: TeamId
                    -> Lens' ChatState (MessageInterface n i)
                    -> MH ()
startTopicUrlSelect tId which = do
    baseUrl <- getServerBaseUrl tId
    cId <- use (which.miChannelId)
    withChannel cId $ \ch -> do
        let urls = V.fromList $ findTopicUrls baseUrl $ ch^.ccInfo.cdHeader
        startUrlSelect which urls

startUrlSelect :: Lens' ChatState (MessageInterface n i)
               -> V.Vector LinkChoice
               -> MH ()
startUrlSelect which urls = do
    src <- use (which.miListing.mlUrlListSource)
    let urlsWithIndexes = V.indexed urls
    which.miMode .= MessageListingMode
    which.miListing.mlMode .= ShowUrlList
    which.miListing.mlUrlList.ulList %= listReplace urlsWithIndexes (Just $ length urls - 1)
    which.miListing.mlUrlList.ulSource .= Just src

stopUrlSelect :: Lens' ChatState (MessageInterface n i)
              -> MH ()
stopUrlSelect which = do
    which.miMode .= Compose

openSelectedURL :: Lens' ChatState (MessageInterface n i) -> MH ()
openSelectedURL which = do
    selected <- use (which.miListing.mlUrlList.ulList.to listSelectedElement)
    case selected of
        Nothing -> return ()
        Just (_, (_, link)) -> openLinkTarget (link^.linkTarget)

findTopicUrls :: TeamBaseURL -> T.Text -> [LinkChoice]
findTopicUrls baseUrl topic =
    let parsed = parseMarkdown (Just baseUrl) topic
        mkTarget (Right url) = LinkURL url
        mkTarget (Left (tName, pId)) = LinkPermalink tName pId
        mkEntry (val, text) = LinkChoice Nothing NoAuthor text (mkTarget val)
        links = mkEntry <$> (mconcat $ blockGetURLs <$> (F.toList $ unBlocks parsed))
    in links

findMessageUrls :: Messages -> [LinkChoice]
findMessageUrls ms =
    let msgs = filterMessages (not . _mDeleted) ms
    in removeDuplicates $ concat $ toList $ toList <$> msgURLs <$> msgs

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = nubOn (\ l -> (l^.linkTarget, l^.linkUser, l^.linkLabel))
