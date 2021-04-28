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

import           Matterhorn.State.Links
import           Matterhorn.Types
import           Matterhorn.Util


startUrlSelect :: MH ()
startUrlSelect = do
    urls <- use (csCurrentChannel.to findUrls.to V.fromList)
    let urlsWithIndexes = V.indexed urls
    tId <- use csCurrentTeamId
    setMode UrlSelect
    csCurrentTeam.tsUrlList .= (listMoveTo (length urls - 1) $ list (UrlList tId) urlsWithIndexes 2)

stopUrlSelect :: MH ()
stopUrlSelect = setMode Main

openSelectedURL :: MH ()
openSelectedURL = whenMode UrlSelect $ do
    selected <- use (csCurrentTeam.tsUrlList.to listSelectedElement)
    case selected of
        Nothing -> setMode Main
        Just (_, (_, link)) -> do
            opened <- openLinkTarget (link^.linkTarget)
            when (not opened) $ do
                mhError $ ConfigOptionMissing "urlOpenCommand"
                setMode Main

findUrls :: ClientChannel -> [LinkChoice]
findUrls chan =
    let msgs = chan^.ccContents.cdMessages
    in removeDuplicates $ concat $ toList $ toList <$> msgURLs <$> msgs

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = nubOn (\ l -> (l^.linkTarget, l^.linkUser, l^.linkLabel))
