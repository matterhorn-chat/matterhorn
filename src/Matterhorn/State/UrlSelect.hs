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

import           Brick.Widgets.List ( listSelectedElement, listReplace )
import qualified Data.Vector as V
import           Lens.Micro.Platform ( (.=), (%=), to, Lens' )

import           Matterhorn.State.Links
import           Matterhorn.Types
import           Matterhorn.Util


startUrlSelect :: Lens' ChatState (MessageInterface n i)
               -> MH ()
startUrlSelect which = do
    msgs <- use (which.miMessages)
    src <- use (which.miUrlListSource)
    let urls = V.fromList $ findUrls msgs
        urlsWithIndexes = V.indexed urls
    which.miMode .= ShowUrlList
    which.miUrlList.ulList %= listReplace urlsWithIndexes (Just $ length urls - 1)
    which.miUrlList.ulSource .= Just src

stopUrlSelect :: Lens' ChatState (MessageInterface n i)
              -> MH ()
stopUrlSelect which = do
    which.miMode .= Compose

openSelectedURL :: Lens' ChatState (MessageInterface n i) -> MH ()
openSelectedURL which = do
    selected <- use (which.miUrlList.ulList.to listSelectedElement)
    case selected of
        Nothing -> return ()
        Just (_, (_, link)) -> openLinkTarget (link^.linkTarget)
    stopUrlSelect which

findUrls :: Messages -> [LinkChoice]
findUrls ms =
    let msgs = filterMessages (not . _mDeleted) ms
    in removeDuplicates $ concat $ toList $ toList <$> msgURLs <$> msgs

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = nubOn (\ l -> (l^.linkTarget, l^.linkUser, l^.linkLabel))
