{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Matterhorn.Draw.ThemeListWindow
  ( drawThemeListWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Keybindings
import qualified Brick.Widgets.List as L
import           Brick.Widgets.Border ( hBorder )
import           Brick.Widgets.Center ( hCenter )

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.Draw.ListWindow ( drawListWindow, WindowPosition(..) )
import           Matterhorn.Themes
import           Matterhorn.Types


drawThemeListWindow :: ChatState -> TeamId -> Widget Name
drawThemeListWindow st tId =
    let window = drawListWindow (st^.csTeam(tId).tsThemeListWindow)
                                  (const $ txt "Themes")
                                  (const $ txt "No matching themes found.")
                                  (const $ txt "Search built-in themes:")
                                  renderInternalTheme
                                  (Just footer)
                                  WindowUpperRight
                                  50
        footer = hBorder <=>
                 (hCenter $ hBox [ enter
                                 , txt $ ":choose theme  "
                                 , close
                                 , txt ":close"
                                 ])
        enter = emph $ txt $ ppMaybeBinding (firstActiveBinding kc ActivateListItemEvent)
        close = emph $ txt $ ppMaybeBinding (firstActiveBinding kc CancelEvent)
        kc = st^.csResources.crConfiguration.configUserKeysL
        emph = withDefAttr clientEmphAttr
    in joinBorders window

renderInternalTheme :: Bool -> InternalTheme -> Widget Name
renderInternalTheme foc it =
    (if foc then forceAttr L.listSelectedFocusedAttr else id) $
    (padRight Max $
     withDefAttr clientEmphAttr $
     txt $ internalThemeName it) <=>
    (vLimit 2 $
     (padLeft (Pad 2) $ txtWrap $ internalThemeDesc it) <=> fill ' ')
