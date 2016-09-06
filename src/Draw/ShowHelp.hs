module Draw.ShowHelp (drawShowHelp) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer)
import Data.Monoid ((<>))
import Lens.Micro.Platform

import Themes
import Types
import Command

drawShowHelp :: ChatState -> [Widget Name]
drawShowHelp = const [helpBox]

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
    borderWithLabel (str "Matterhorn Help") $ viewport HelpViewport Both helpText
    where
    helpText = str $ "Commands\n========\n" <> mkHelpText commandList

    mkHelpText :: [Cmd] -> String
    mkHelpText cs =
      let commandNameWidth = 4 + (maximum $ length <$> commandName <$> cs)
          padTo n s = s ++ replicate (n - length s) ' '
      in unlines [ padTo commandNameWidth ('/':cmd) ++ desc
                 | Cmd { commandName = cmd, commandDescr = desc } <- cs
                 ]
