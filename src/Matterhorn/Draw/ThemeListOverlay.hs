{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Matterhorn.Draw.ThemeListOverlay
  ( drawThemeListOverlay
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import qualified Brick.Widgets.List as L
import           Brick.Widgets.Border ( hBorder )
import           Brick.Widgets.Center ( hCenter )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Draw.ListOverlay ( drawListOverlay, OverlayPosition(..) )
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents ( ppBinding )
import           Matterhorn.Events.Keybindings


drawThemeListOverlay :: ChatState -> TeamId -> Widget Name
drawThemeListOverlay st tId =
    let overlay = drawListOverlay (st^.csTeam(tId).tsThemeListOverlay)
                                  (const $ txt "Themes")
                                  (const $ txt "No matching themes found.")
                                  (const $ txt "Search built-in themes:")
                                  renderInternalTheme
                                  (Just footer)
                                  OverlayUpperRight
                                  50
        footer = hBorder <=>
                 (hCenter $ hBox [ enter
                                 , txt $ ":choose theme  "
                                 , close
                                 , txt ":close"
                                 ])
        enter = emph $ txt $ ppBinding (getFirstDefaultBinding ActivateListItemEvent)
        close = emph $ txt $ ppBinding (getFirstDefaultBinding CancelEvent)
        emph = withDefAttr clientEmphAttr
    in joinBorders overlay

renderInternalTheme :: Bool -> InternalTheme -> Widget Name
renderInternalTheme foc it =
    (if foc then forceAttr L.listSelectedFocusedAttr else id) $
    (padRight Max $
     withDefAttr clientEmphAttr $
     txt $ internalThemeName it) <=>
    (vLimit 2 $
     (padLeft (Pad 2) $ txtWrap $ internalThemeDesc it) <=> fill ' ')
