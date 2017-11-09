module Draw.ShowHelp (drawShowHelp) where

import Prelude ()
import Prelude.Compat

import Brick
import Brick.Themes (themeDescriptions)
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter, centerLayer)
import Brick.Widgets.List (listSelectedFocusedAttr)
import Lens.Micro.Platform
import Data.List (sortBy, intercalate, sort)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Graphics.Vty as Vty
import Network.Mattermost.Version (mmApiVersion)

import Themes
import Types
import Command
import Events.ShowHelp
import Events.ChannelScroll
import Events.ChannelSelect
import Events.UrlSelect
import Events.Main
import Events.MessageSelect
import Events.PostListOverlay
import State.Editing (editingKeybindings)
import Markdown (renderText)
import Options (mhVersion)
import HelpTopics (helpTopics)

drawShowHelp :: HelpTopic -> ChatState -> [Widget Name]
drawShowHelp topic st =
    [helpBox (helpTopicViewportName topic) $ helpTopicDraw topic st]

helpTopicDraw :: HelpTopic -> ChatState -> Widget Name
helpTopicDraw topic =
    case helpTopicScreen topic of
        MainHelp -> const mainHelp
        ScriptHelp -> const scriptHelp
        ThemeHelp -> const themeHelp

mainHelp :: Widget Name
mainHelp = commandHelp
  where
    commandHelp = vBox $ [ padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ str mhVersion
                         , hCenter $ withDefAttr helpEmphAttr $ str mmApiVersion
                         , padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ txt "Help Topics"
                         , drawHelpTopics
                         , padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ txt "Commands"
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
         vBox [ (withDefAttr helpEmphAttr $ txt $ padTo commandNameWidth info) <+> renderText desc
              | (info, desc) <- helpInfo
              ]

drawHelpTopics :: Widget Name
drawHelpTopics =
    let allHelpTopics = drawTopic <$> helpTopics
        topicNameWidth = 4 + (maximum $ T.length <$> helpTopicName <$> helpTopics)
        drawTopic t = (withDefAttr helpEmphAttr $ txt (padTo topicNameWidth $ helpTopicName t)) <+>
                      txt (helpTopicDescription t)
    in (padBottom (Pad 1) $
        hCenter $ renderText "These topics can be used with the `/help` command:") <=>
       (hCenter $ vBox allHelpTopics)

scriptHelp :: Widget Name
scriptHelp = vBox
  [ padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ txt "Using Scripts"
  , padTop (Pad 1) $ hCenter $ hLimit 100 $ vBox scriptHelpText
  ]
  where scriptHelpText = map (padTop (Pad 1) . renderText . mconcat)
          [ [ "Matterhorn has a special feature that allows you to use "
             , "prewritten shell scripts to preprocess messages. "
             , "For example, this can allow you to run various filters over "
             , "your written text, do certain kinds of automated formatting, "
             , "or just automatically cowsay-ify a message.\n" ]
           , [ "These scripts can be any kind of executable file, "
             , "as long as the file lives in "
             , "*~/.config/matterhorn/scripts* (unless you've explicitly "
             , "moved your XDG configuration directory elsewhere). "
             , "Those executables are given no arguments "
             , "on the command line and are passed your typed message on "
             , "*stdin*; whatever they produce on *stdout* is sent "
             , "as a message. If the script exits successfully, then everything "
             , "that appeared on *stderr* is discarded; if it instead exits with "
             , "a failing exit code, your message is *not* sent, and you are "
             , "presented with whatever was printed on stderr as a "
             , "local error message.\n" ]
           , [ "To run a script, simply type\n" ]
           , [ "> *> /sh [script-name] [my-message]*\n" ]
           , [ "And the script named *[script-name]* will be invoked with "
             , "the text of *[my-message]*. If the script does not exist, "
             , "or if it exists but is not marked as executable, you'll be "
             , "presented with an appropriate error message.\n" ]
           , [ "For example, if you want to use a basic script to "
             , "automatically ROT13 your message, you can write a shell "
             , "script using the standard Unix *tr* utility, like this:\n" ]
           , [ "> *#!/bin/bash -e*\n"
             , "> *tr '[A-Za-z]' '[N-ZA-Mn-za-m]'*\n\n" ]
           , [ "Move this script to *~/.config/matterhorn/scripts/rot13* "
             , "and be sure it's executable with\n" ]
           , [ "> *$ chmod u+x ~/.config/matterhorn/scripts/rot13*\n\n" ]
           , [ "after which you can send ROT13 messages with the "
             , "Matterhorn command " ]
           , [ "> *> /sh rot13 Hello, world!*\n" ]
           ]

themeHelp :: Widget a
themeHelp = overrideAttr codeAttr helpEmphAttr $ vBox
  [ padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ txt "Using Themes"
  , padTop (Pad 1) $ hCenter $ renderText "Matterhorn provides these built-in color themes:"
  , padTop (Pad 1) $ vBox $ hCenter <$> withDefAttr helpEmphAttr <$>
                            txt <$> internalThemeName <$> internalThemes
  , padTop (Pad 1) $ hCenter $ hLimit 72 $ renderText $
        "These themes can be selected with the */theme* command. To automatically " <>
        "select a theme at startup, set the *theme* configuration file option to one " <>
        "of the themes listed above."
  , padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ txt "Customizing the Theme"
  , padTop (Pad 1) $ hCenter $ hLimit 72 $ renderText $
        "Theme customization is also supported. To customize the selected theme, " <>
        "create a theme customization file and set the `themeCustomizationFile` " <>
        "configuration option to the path to the customization file. If the path " <>
        "to the file is relative, Matterhorn will look for it in the same directory " <>
        "as the Matterhorn configuration file.\n" <>
        "  \n" <>
        "Theme customization files are INI-style files that can customize any " <>
        "foreground color, background color, or style of any aspect of the " <>
        "Matterhorn user interface. Here is an example:\n" <>
        "```\n" <>
        "[default]\n" <>
        "default.fg = blue\n" <>
        "default.bg = black\n" <>
        "\n" <>
        "[other]\n" <>
        attrNameToConfig codeAttr <> ".fg = magenta\n" <>
        attrNameToConfig codeAttr <> ".style = bold\n" <>
        attrNameToConfig clientEmphAttr <> ".fg = cyan\n" <>
        attrNameToConfig clientEmphAttr <> ".style = [bold, underline]\n" <>
        attrNameToConfig listSelectedFocusedAttr <> ".fg = brightGreen\n" <>
        "```\n" <>
        "In the example above, the theme's default foreground and background colors " <>
        "are both customized to *blue* and *black*, respectively. The *default* section " <>
        "contains only customizations for the *default* attribute. All other customizations " <>
        "go in the *other* section. We can also set the style for attributes; we can either " <>
        "set just one style (as with the bold setting above) or multiple styles at once " <>
        "(as in the bold/underline example).\n\n" <>
        "Available colors are:\n" <>
        " * black\n" <>
        " * red\n" <>
        " * green\n" <>
        " * yellow\n" <>
        " * blue\n" <>
        " * magenta\n" <>
        " * cyan\n" <>
        " * white\n" <>
        " * brightBlack\n" <>
        " * brightRed\n" <>
        " * brightGreen\n" <>
        " * brightYellow\n" <>
        " * brightBlue\n" <>
        " * brightMagenta\n" <>
        " * brightCyan\n" <>
        " * brightWhite\n" <>
        "  \n" <>
        "Available styles are:\n" <>
        " * standout\n" <>
        " * underline\n" <>
        " * reverseVideo\n" <>
        " * blink\n" <>
        " * dim\n" <>
        " * bold\n"
  , padTop (Pad 1) $ hCenter $ withDefAttr helpEmphAttr $ txt "Theme Attributes"
  , padTop (Pad 1) $ hCenter $ hLimit 72 $ renderText $
        "This section lists all possible theme attributes for use in customization files along with a description of how each one is used in Matterhorn. Each option listed can be set in the *other* section of the customization file. Each provides three customization settings:\n" <>
        " * *<option>.fg = <color>*\n" <>
        " * *<option>.bg = <color>*\n" <>
        " * *<option>.style = <style>* or *<option>.style = [<style>, ...]*\n"

  , padTop (Pad 1) $ hCenter $ hLimit 72 $ renderText $
        let names = sort $
                    (\(n, msg) -> (attrNameToConfig n, msg)) <$>
                    (M.toList $ themeDescriptions themeDocs)
            mkEntry (opt, msg) = "*" <> opt <> "*\n" <> msg <> "\n"
        in T.concat $ mkEntry <$> names
  ]

attrNameToConfig :: AttrName -> T.Text
attrNameToConfig = T.pack . intercalate "." . attrNameComponents

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
    , ("URL Select Mode", urlSelectKeybindings)
    , ("Channel Scroll Mode", channelScrollKeybindings)
    , ("Message Select Mode", messageSelectKeybindings)
    , ("Text Editing", editingKeybindings)
    , ("Flagged Messages", postListOverlayKeybindings)
    ]

helpBox :: Name -> Widget Name -> Widget Name
helpBox n helpText =
    centerLayer $ withMargins (2, 1) $
      (withDefAttr helpAttr $ borderWithLabel (withDefAttr helpEmphAttr $ txt "Matterhorn Help") $
       (viewport HelpViewport Vertical $ cached n helpText)) <=>
      quitMessage
    where
    quitMessage = padTop (Pad 1) $ hCenter $ txt "Press Esc to exit the help screen."

kbColumnWidth :: Int
kbColumnWidth = 12

kbDescColumnWidth :: Int
kbDescColumnWidth = 60

mkKeybindingHelp :: (T.Text, [Keybinding]) -> Widget Name
mkKeybindingHelp (sectionName, kbs) =
    (hCenter $ padTop (Pad 1) $ withDefAttr helpEmphAttr $ txt $ "Keybindings: " <> sectionName) <=>
    (hCenter $ vBox $ mkKeybindHelp <$> (sortBy (comparing (ppKbEvent.kbEvent)) kbs))

mkKeybindHelp :: Keybinding -> Widget Name
mkKeybindHelp (KB desc ev _) =
    (withDefAttr helpEmphAttr $ txt $ padTo kbColumnWidth $ ppKbEvent ev) <+>
    (vLimit 1 $ hLimit kbDescColumnWidth $ renderText desc <+> fill ' ')

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
