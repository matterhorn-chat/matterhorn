module Matterhorn.Draw.URLList
  ( renderUrlList
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.List ( renderList )
import qualified Data.Foldable as F
import           Lens.Micro.Platform ( to )

import           Network.Mattermost.Types ( ServerTime(..), idString )

import           Matterhorn.Draw.Messages
import           Matterhorn.Draw.Util
import           Matterhorn.Draw.RichText
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( unURL )


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

        me = myUsername st

        renderItem sel link =
          let time = link^.linkTime
          in attr sel $ vLimit 2 $
            (vLimit 1 $
             hBox [ let u = maybe "<server>" id (link^.linkUser.to (nameForUserRef st))
                    in colorUsername me u u
                  , case link^.linkLabel of
                      Nothing -> emptyWidget
                      Just label -> txt ": " <+> hBox (F.toList $ renderElementSeq me label)
                  , fill ' '
                  , renderDate st $ withServerTime time
                  , str " "
                  , renderTime st $ withServerTime time
                  ] ) <=>
            (vLimit 1 (renderLinkTarget (link^.linkTarget)))

        renderLinkTarget (LinkPermalink tName pId) =
            renderText $ "Team: " <> tName <> ", post " <> idString pId
        renderLinkTarget (LinkURL url) = renderText $ unURL url
        renderLinkTarget (LinkFileId _) = txt " "

        attr True = forceAttr urlListSelectedAttr
        attr False = id
