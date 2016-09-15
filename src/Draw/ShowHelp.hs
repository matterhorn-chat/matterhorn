module Draw.ShowHelp (drawShowHelp) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter, centerLayer)
import Lens.Micro.Platform
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Graphics.Vty as Vty

import Themes
import Types
import Command
import Events

drawShowHelp :: ChatState -> [Widget Name]
drawShowHelp = const [helpBox, fill ' ']

withMargins :: (Int, Int) -> Widget a -> Widget a
withMargins (hMargin, vMargin) w =
    Widget (hSize w) (vSize w) $ do
        ctx <- getContext
        let wl = ctx^.availWidthL - (2 * hMargin)
            hl = ctx^.availHeightL - (2 * vMargin)
        render $ hLimit wl $ vLimit hl w

keybindSections :: [(T.Text, [Keybinding])]
keybindSections =
    [ ("This Help Page", helpKeybindings)
    , ("Main Interface", mainKeybindings)
    , ("Channel Select Mode", channelSelectKeybindings)
    ]

helpBox :: Widget Name
helpBox =
    centerLayer $ withMargins (2, 1) $
      (withDefAttr helpAttr $ borderWithLabel (withDefAttr helpEmphAttr $ txt "Matterhorn Help") $
       (viewport HelpViewport Vertical helpText)) <=>
      quitMessage
    where
    helpText = commandHelp

    quitMessage = padTop (Pad 1) $ hCenter $ txt "Press Esc to exit the help screen."

    commandHelp = vBox $ [ padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ txt "Commands"
                         , mkCommandHelpText $ sortBy (comparing commandName) commandList
                         ] <>
                         (mkKeybindingHelp <$> keybindSections)

    mkCommandHelpText :: [Cmd] -> Widget Name
    mkCommandHelpText cs =
      let helpInfo = [ (info, desc)
                     | Cmd cmd desc args _ <- cs
                     , let argSpec = printArgSpec args
                           info = T.cons '/' cmd <> " " <> argSpec
                     ]
          commandNameWidth = 4 + (maximum $ T.length <$> fst <$> helpInfo)
      in hCenter $
         vBox [ (withDefAttr helpEmphAttr $ txt $ padTo commandNameWidth info) <+> txt desc
              | (info, desc) <- helpInfo
              ]

kbColumnWidth :: Int
kbColumnWidth = 10

kbDescColumnWidth :: Int
kbDescColumnWidth = 60

mkKeybindingHelp :: (T.Text, [Keybinding]) -> Widget Name
mkKeybindingHelp (sectionName, kbs) =
    (hCenter $ padTop (Pad 1) $ withDefAttr helpEmphAttr $ txt $ "Keybindings: " <> sectionName) <=>
    (hCenter $ vBox $ mkKeybindHelp <$> (sortBy (comparing (ppKbEvent.kbEvent)) kbs))

mkKeybindHelp :: Keybinding -> Widget Name
mkKeybindHelp (KB desc ev _) =
    (withDefAttr helpEmphAttr $ txt $ padTo kbColumnWidth $ ppKbEvent ev) <+>
    (hLimit kbDescColumnWidth $ txt $ padTo kbDescColumnWidth desc)

ppKbEvent :: Vty.Event -> T.Text
ppKbEvent (Vty.EvKey k mods) =
    T.intercalate "-" $ (ppMod <$> mods) <> [ppKey k]
ppKbEvent _ = "<????>"

ppKey :: Vty.Key -> T.Text
ppKey (Vty.KChar c)   = ppChar c
ppKey (Vty.KFun n)    = "F" <> (T.pack $ show n)
ppKey Vty.KBackTab    = "S-Tab"
ppKey Vty.KEsc        = "Esc"
ppKey Vty.KBS         = "Backspace"
ppKey Vty.KEnter      = "Enter"
ppKey Vty.KUp         = "Up"
ppKey Vty.KDown       = "Down"
ppKey Vty.KLeft       = "Left"
ppKey Vty.KRight      = "Right"
ppKey Vty.KHome       = "Home"
ppKey Vty.KEnd        = "End"
ppKey Vty.KPageUp     = "PgUp"
ppKey Vty.KPageDown   = "PgDown"
ppKey Vty.KDel        = "Del"
ppKey _               = "???"

ppChar :: Char -> T.Text
ppChar '\t' = "Tab"
ppChar ' '  = "Space"
ppChar c    = T.singleton c

ppMod :: Vty.Modifier -> T.Text
ppMod Vty.MMeta  = "M"
ppMod Vty.MAlt   = "A"
ppMod Vty.MCtrl  = "C"
ppMod Vty.MShift = "S"

padTo :: Int -> T.Text -> T.Text
padTo n s = s <> T.replicate (n - T.length s) " "
