module Draw.ShowHelp (drawShowHelp) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter, centerLayer)
import Lens.Micro.Platform
import qualified Data.Text as T
import Data.Monoid ((<>))

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
    centerLayer $ withMargins (5, 3) $
      (withDefAttr helpAttr $ borderWithLabel (withDefAttr helpEmphAttr $ str "Matterhorn Help") $
       (viewport HelpViewport Vertical helpText)) <=>
      quitMessage
    where
    helpText = commandHelp

    quitMessage = padTop (Pad 1) $ hCenter $ txt "Press Esc to exit the help screen."

    commandHelp = vBox [ padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ str "Commands"
                       , mkHelpText commandList
                       ]

    mkHelpText :: [Cmd] -> Widget Name
    mkHelpText cs =
      let helpInfo = [ (info, desc)
                     | Cmd cmd desc args _ <- cs
                     , let argSpec = printArgSpec args
                           info = T.cons '/' cmd <> " " <> argSpec
                     ]
          commandNameWidth = 4 + (maximum $ T.length <$> fst <$> helpInfo)
          padTo n s = s <> T.replicate (n - T.length s) " "
      in hCenter $
         vBox [ (withDefAttr helpEmphAttr $ txt $ padTo commandNameWidth info) <+> txt desc
              | (info, desc) <- helpInfo
              ]
