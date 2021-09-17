module Matterhorn.Draw.URLList
  ( renderUrlList
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border ( hBorder )
import           Brick.Widgets.List ( renderList )
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import           Lens.Micro.Platform ( to )

import           Network.Mattermost.Types ( ServerTime(..), idString )

import           Matterhorn.Draw.Messages
import           Matterhorn.Draw.Util
import           Matterhorn.Draw.RichText
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.RichText


renderUrlList :: ChatState -> Widget Name
renderUrlList st =
    header <=> urlDisplay
    where
        header = (withDefAttr channelHeaderAttr $ vLimit 1 $
                 (renderText' Nothing "" (getHighlightSet st) Nothing $
                  "URLs: " <> (mkChannelName st (st^.csCurrentChannel.ccInfo))) <+>
                 fill ' ') <=> hBorder

        urlDisplay = if F.length urls == 0
                     then str "No URLs found in this channel."
                     else renderList renderItem True urls

        urls = st^.csCurrentTeam.tsUrlList

        me = myUsername st

        hSet = getHighlightSet st

        renderItem sel (i, link) =
          let time = link^.linkTime
          in attr sel $ vLimit 2 $
            (vLimit 1 $
             hBox [ let u = maybe "<server>" id (link^.linkUser.to (printableNameForUserRef st))
                    in colorUsername me u u
                  , case link^.linkLabel of
                      Nothing -> emptyWidget
                      Just label ->
                          case Seq.null (unInlines label) of
                              True -> emptyWidget
                              False -> txt ": " <+> renderRichText me hSet Nothing False Nothing Nothing
                                                    (Blocks $ Seq.singleton $ Para label)
                  , fill ' '
                  , renderDate st $ withServerTime time
                  , str " "
                  , renderTime st $ withServerTime time
                  ] ) <=>
            (vLimit 1 (clickable (ClickableURLListEntry i (link^.linkTarget)) $ renderLinkTarget (link^.linkTarget)))

        renderLinkTarget (LinkPermalink (TeamURLName tName) pId) =
            renderText $ "Team: " <> tName <> ", post " <> idString pId
        renderLinkTarget (LinkURL url) = renderText $ unURL url
        renderLinkTarget (LinkFileId _) = txt " "

        attr True = forceAttr urlListSelectedAttr
        attr False = id
