module Draw.ShowHelp (drawShowHelp) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter, centerLayer)
import Lens.Micro.Platform

import Themes
import Types
import Command

drawShowHelp :: ChatState -> [Widget Name]
drawShowHelp = const [helpBox, fill ' ']

withMargins :: (Int, Int) -> Widget a -> Widget a
withMargins (hMargin, vMargin) w =
    Widget (hSize w) (vSize w) $ do
        ctx <- getContext
        let wl = ctx^.availWidthL - (2 * hMargin)
            hl = ctx^.availHeightL - (2 * vMargin)
        render $ hLimit wl $ vLimit hl w

helpBox :: Widget Name
helpBox =
    centerLayer $ withMargins (5, 2) $ withDefAttr helpAttr $
    borderWithLabel (withDefAttr helpEmphAttr $ str "Matterhorn Help") $
    viewport HelpViewport Vertical helpText
    where
    helpText = commandHelp

    commandHelp = vBox [ padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ str "Commands"
                       , mkHelpText commandList
                       ]

    mkHelpText :: [Cmd] -> Widget Name
    mkHelpText cs =
      let commandNameWidth = 4 + (maximum $ length <$> commandName <$> cs)
          padTo n s = s ++ replicate (n - length s) ' '
      in hCenter $
         vBox [ (withDefAttr helpEmphAttr $ str $ padTo commandNameWidth ('/':cmd)) <+> str desc
              | Cmd { commandName = cmd, commandDescr = desc } <- cs
              ]
