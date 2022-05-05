module Matterhorn.State.UrlSelect
  (
  -- * URL selection mode
    startUrlSelect
  , stopUrlSelect
  , openSelectedURL
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick.Widgets.List ( list, listMoveTo, listSelectedElement )
import qualified Data.Vector as V
import           Lens.Micro.Platform ( (.=), to )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.Links
import           Matterhorn.Types
import           Matterhorn.Util


startUrlSelect :: TeamId -> MH ()
startUrlSelect tId = do
    withCurrentChannel tId $ \_ chan -> do
        let urls = V.fromList $ findUrls $ chan^.ccContents.cdMessages
            urlsWithIndexes = V.indexed urls
        pushMode tId UrlSelect
        csTeam(tId).tsUrlList .= (listMoveTo (length urls - 1) $ list (UrlList tId) urlsWithIndexes 2)

stopUrlSelect :: TeamId -> MH ()
stopUrlSelect = popMode

openSelectedURL :: TeamId -> MH ()
openSelectedURL tId = whenMode tId UrlSelect $ do
    selected <- use (csTeam(tId).tsUrlList.to listSelectedElement)
    case selected of
        Nothing -> return ()
        Just (_, (_, link)) -> openLinkTarget (link^.linkTarget)
    popMode tId

findUrls :: Messages -> [LinkChoice]
findUrls ms =
    let msgs = filterMessages (not . _mDeleted) ms
    in removeDuplicates $ concat $ toList $ toList <$> msgURLs <$> msgs

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = nubOn (\ l -> (l^.linkTarget, l^.linkUser, l^.linkLabel))
