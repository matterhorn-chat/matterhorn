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
    urls <- use (csCurrentChannel(tId).to findUrls.to V.fromList)
    let urlsWithIndexes = V.indexed urls
    setMode tId UrlSelect
    csTeam(tId).tsUrlList .= (listMoveTo (length urls - 1) $ list (UrlList tId) urlsWithIndexes 2)

stopUrlSelect :: TeamId -> MH ()
stopUrlSelect tId = do
    setMode tId Main

openSelectedURL :: TeamId -> MH ()
openSelectedURL tId = whenMode tId UrlSelect $ do
    selected <- use (csTeam(tId).tsUrlList.to listSelectedElement)
    case selected of
        Nothing -> setMode tId Main
        Just (_, (_, link)) -> do
            opened <- openLinkTarget (link^.linkTarget)
            when (not opened) $ do
                mhError $ ConfigOptionMissing "urlOpenCommand"
                setMode tId Main

findUrls :: ClientChannel -> [LinkChoice]
findUrls chan =
    let msgs = chan^.ccContents.cdMessages
    in removeDuplicates $ concat $ toList $ toList <$> msgURLs <$> msgs

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = nubOn (\ l -> (l^.linkTarget, l^.linkUser, l^.linkLabel))
