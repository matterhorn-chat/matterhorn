{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draw.ThemeListOverlay
  ( drawThemeListOverlay
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import qualified Brick.Widgets.List as L

import           Draw.Main
import           Draw.ListOverlay ( drawListOverlay )
import           Themes
import           Types


drawThemeListOverlay :: ChatState -> [Widget Name]
drawThemeListOverlay st =
    let overlay = drawListOverlay (st^.csThemeListOverlay)
                                  (const $ txt "Themes")
                                  (const $ txt "No matching themes found.")
                                  (const $ txt "Search built-in themes:")
                                  renderInternalTheme
    in joinBorders overlay : drawMain False st

renderInternalTheme :: Bool -> InternalTheme -> Widget Name
renderInternalTheme foc it =
    (if foc then forceAttr L.listSelectedFocusedAttr else id) $
    padRight Max $
    txt $ internalThemeName it
