{-# LANGUAGE OverloadedStrings #-}
module Matterhorn.Draw.DeleteChannelConfirm
    ( drawDeleteChannelConfirm
    )
where

import Prelude ()
import Matterhorn.Prelude

import Network.Mattermost.Types ( TeamId )

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Matterhorn.Themes
import Matterhorn.Types


drawDeleteChannelConfirm :: ChatState -> TeamId -> Widget Name
drawDeleteChannelConfirm st tId =
    case st^.csCurrentChannelId(tId) of
        Nothing -> emptyWidget
        Just cId ->
            case st^?csChannel(cId) of
                Nothing -> emptyWidget
                Just chan ->
                    let cName = chan^.ccInfo.cdName
                    in centerLayer $ hLimit 50 $ vLimit 15 $
                       withDefAttr dialogAttr $
                       borderWithLabel (txt "Confirm Delete Channel") $
                       vBox [ padBottom (Pad 1) $ hCenter $ txt "Are you sure you want to delete this channel?"
                            , padBottom (Pad 1) $ hCenter $ withDefAttr dialogEmphAttr $ txt cName
                            , hCenter $ txt "Press " <+> (withDefAttr dialogEmphAttr $ txt "Y") <+> txt " to delete the channel"
                            , hCenter $ txt "or any other key to cancel."
                            ]
