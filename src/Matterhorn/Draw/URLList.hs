module Matterhorn.Draw.URLList
  ( drawUrlSelectWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.List ( renderList, listSelectedElement )
import           Brick.Widgets.Center
import           Data.List ( intersperse )
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import           Lens.Micro.Platform ( to )

import           Network.Mattermost.Types ( ServerTime(..), TeamId, idString )

import           Matterhorn.Draw.Messages
import           Matterhorn.Draw.Util
import           Matterhorn.Draw.RichText
import           Matterhorn.Events.UrlSelect
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.RichText
import           Matterhorn.Types.KeyEvents


drawUrlSelectWindow :: ChatState -> TeamId -> Widget Name
drawUrlSelectWindow st tId =
    centerLayer $
    hLimit 100 $
    vLimit 35 $
    joinBorders $
    border $
    vBox [ renderUrlList st tId
         , urlSelectBottomBar st tId
         , urlSelectInputArea st tId
         ]

renderUrlList :: ChatState -> TeamId -> Widget Name
renderUrlList st tId =
    header <=> urlDisplay
    where
        header = fromMaybe emptyWidget headerFromSrc

        headerFromSrc = do
            title <- headerTitleFromSrc =<< src
            return $ (withDefAttr channelHeaderAttr $
                     (vLimit 1 (renderText' Nothing "" hSet Nothing title <+> fill ' '))) <=> hBorder

        headerTitleFromSrc (FromThreadIn cId) = do
            cName <- channelNameFor cId
            return $ "Links from thread in " <> cName
        headerTitleFromSrc (FromChannel cId) = do
            cName <- channelNameFor cId
            return $ "Links from " <> cName

        channelNameFor cId = do
            chan <- st^?csChannel(cId)
            return $ mkChannelName st (chan^.ccInfo)

        urlDisplay = if F.length urls == 0
                     then str "No links found."
                     else renderList renderItem True urls

        urls = st^.csTeam(tId).tsUrlList.ulList
        src = st^.csTeam(tId).tsUrlList.ulSource

        me = myUsername st

        hSet = getHighlightSet st tId

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

urlSelectBottomBar :: ChatState -> TeamId -> Widget Name
urlSelectBottomBar st tId =
    case listSelectedElement $ st^.csTeam(tId).tsUrlList.ulList of
        Nothing -> hBorder
        Just (_, (_, link)) ->
            let options = [ ( isFile
                            , ev SaveAttachmentEvent
                            , "save attachment"
                            )
                          ]
                ev = keyEventBindings st (urlSelectKeybindings tId)
                isFile entry = case entry^.linkTarget of
                    LinkFileId {} -> True
                    _ -> False
                optionList = if null usableOptions
                             then txt "(no actions available for this link)"
                             else hBox $ intersperse (txt " ") usableOptions
                usableOptions = catMaybes $ mkOption <$> options
                mkOption (f, k, desc) = if f link
                                        then Just $ withDefAttr urlSelectStatusAttr (txt k) <+>
                                                    txt (":" <> desc)
                                        else Nothing
            in hBox [ borderElem bsHorizontal
                    , txt "["
                    , txt "Options: "
                    , optionList
                    , txt "]"
                    , hBorder
                    ]

urlSelectInputArea :: ChatState -> TeamId -> Widget Name
urlSelectInputArea st tId =
    let getBinding = keyEventBindings st (urlSelectKeybindings tId)
    in hCenter $ hBox [ withDefAttr clientEmphAttr $ txt "Enter"
                      , txt ":open  "
                      , withDefAttr clientEmphAttr $ txt $ getBinding CancelEvent
                      , txt ":close"
                      ]
