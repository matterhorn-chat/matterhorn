{-# LANGUAGE RankNTypes #-}
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
import           Lens.Micro.Platform ( (.=), to, Traversal' )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.Links
import           Matterhorn.Types
import           Matterhorn.Util


startUrlSelect :: TeamId -> Traversal' ChatState Messages -> Maybe URLListSource -> MH ()
startUrlSelect tId which src = do
    msgs <- use which
    let urls = V.fromList $ findUrls msgs
        urlsWithIndexes = V.indexed urls
    pushMode tId UrlSelect
    csTeam(tId).tsUrlList .= URLList { _ulList = listMoveTo (length urls - 1) $ list (UrlList tId) urlsWithIndexes 2
                                     , _ulSource = src
                                     }

stopUrlSelect :: TeamId -> MH ()
stopUrlSelect = popMode

openSelectedURL :: TeamId -> MH ()
openSelectedURL tId = whenMode tId UrlSelect $ do
    selected <- use (csTeam(tId).tsUrlList.ulList.to listSelectedElement)
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
