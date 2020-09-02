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

import           Matterhorn.State.Common
import           Matterhorn.Types
import           Matterhorn.Util


startUrlSelect :: MH ()
startUrlSelect = do
    urls <- use (csCurrentChannel.to findUrls.to V.fromList)
    setMode UrlSelect
    csUrlList .= (listMoveTo (length urls - 1) $ list UrlList urls 2)

stopUrlSelect :: MH ()
stopUrlSelect = setMode Main

openSelectedURL :: MH ()
openSelectedURL = whenMode UrlSelect $ do
    selected <- use (csUrlList.to listSelectedElement)
    case selected of
        Nothing -> return ()
        Just (_, link) -> do
            opened <- openLinkTarget (link^.linkTarget)
            when (not opened) $ do
                mhError $ ConfigOptionMissing "urlOpenCommand"
                setMode Main

findUrls :: ClientChannel -> [LinkChoice]
findUrls chan =
    let msgs = chan^.ccContents.cdMessages
    in removeDuplicates $ concat $ toList $ toList <$> msgURLs <$> msgs

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = nubOn (\ l -> (l^.linkTarget, l^.linkUser))
