module State.UrlSelect
  (
  -- * URL selection mode
    startUrlSelect
  , stopUrlSelect
  , openSelectedURL
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Widgets.List ( list, listMoveTo, listSelectedElement )
import qualified Data.Vector as V
import           Lens.Micro.Platform ( (.=), to )

import           State.Common
import           Types
import           Util


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
            opened <- openURL link
            when (not opened) $ do
                mhError $ ConfigOptionMissing "urlOpenCommand"
                setMode Main

findUrls :: ClientChannel -> [LinkChoice]
findUrls chan =
    let msgs = chan^.ccContents.cdMessages
    in removeDuplicates $ concat $ toList $ toList <$> msgURLs <$> msgs

removeDuplicates :: [LinkChoice] -> [LinkChoice]
removeDuplicates = nubOn (\ l -> (l^.linkURL, l^.linkUser))
