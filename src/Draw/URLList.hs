module Draw.URLList
  ( renderUrlList
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.List ( renderList )
import qualified Data.Foldable as F
import           Lens.Micro.Platform ( to )

import           Network.Mattermost.Types ( ServerTime(..) )

import           Draw.Messages
import           Draw.Util
import           Draw.RichText
import           Themes
import           Types


renderUrlList :: ChatState -> Widget Name
renderUrlList st =
    header <=> urlDisplay
    where
        header = withDefAttr channelHeaderAttr $ vLimit 1 $
                 (txt $ "URLs: " <> (st^.csCurrentChannel.ccInfo.cdName)) <+>
                 fill ' '

        urlDisplay = if F.length urls == 0
                     then str "No URLs found in this channel."
                     else renderList renderItem True urls

        urls = st^.csUrlList

        renderItem sel link =
          let time = link^.linkTime
          in attr sel $ vLimit 2 $
            (vLimit 1 $
             hBox [ let u = maybe "<server>" id (link^.linkUser.to (nameForUserRef st))
                    in colorUsername (myUsername st) u u
                  , if link^.linkName == link^.linkURL
                      then emptyWidget
                      else (txt ": " <+> (renderText $ link^.linkName))
                  , fill ' '
                  , renderDate st $ withServerTime time
                  , str " "
                  , renderTime st $ withServerTime time
                  ] ) <=>
            (vLimit 1 (renderText $ link^.linkURL))

        attr True = forceAttr urlListSelectedAttr
        attr False = id
