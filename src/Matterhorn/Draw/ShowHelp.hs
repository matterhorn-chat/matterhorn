module Matterhorn.Draw.ShowHelp
  ( drawShowHelp
  , keybindingMarkdownTable
  , keybindingTextTable
  , commandTextTable
  , commandMarkdownTable
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Themes ( themeDescriptions )
import           Brick.Widgets.Center ( hCenter )
import           Brick.Widgets.List ( listSelectedFocusedAttr )
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( singular, _Just, _2 )

import           Network.Mattermost.Version ( mmApiVersion )

import           Matterhorn.Command
import           Matterhorn.Events
import           Matterhorn.Events.ChannelSelect
import           Matterhorn.Events.Keybindings
import           Matterhorn.Events.Main
import           Matterhorn.Events.MessageSelect
import           Matterhorn.Events.ThemeListOverlay
import           Matterhorn.Events.PostListOverlay
import           Matterhorn.Events.ShowHelp
import           Matterhorn.Events.UrlSelect
import           Matterhorn.Events.UserListOverlay
import           Matterhorn.Events.ChannelListOverlay
import           Matterhorn.Events.ReactionEmojiListOverlay
import           Matterhorn.Events.ManageAttachments
import           Matterhorn.Events.TabbedWindow
import           Matterhorn.Windows.ViewMessage
import           Matterhorn.HelpTopics ( helpTopics )
import           Matterhorn.Draw.RichText ( renderText )
import           Matterhorn.Options ( mhVersion )
import           Matterhorn.State.Editing ( editingKeyHandlers )
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.KeyEvents ( BindingState(..), Binding(..)
                                 , ppBinding, nonCharKeys, eventToBinding )


drawShowHelp :: HelpTopic -> ChatState -> [Widget Name]
drawShowHelp topic st =
    [helpBox (helpTopicViewportName topic) $ helpTopicDraw topic st]

helpTopicDraw :: HelpTopic -> ChatState -> Widget Name
helpTopicDraw topic st =
    overrideAttr codeAttr helpEmphAttr $
    hCenter $
    hLimit helpContentWidth $
    case helpTopicScreen topic of
        MainHelp -> mainHelp (configUserKeys (st^.csResources.crConfiguration))
        ScriptHelp -> scriptHelp
        ThemeHelp -> themeHelp
        SyntaxHighlightHelp -> syntaxHighlightHelp (configSyntaxDirs $ st^.csResources.crConfiguration)
        KeybindingHelp -> keybindingHelp (configUserKeys (st^.csResources.crConfiguration))

mainHelp :: KeyConfig -> Widget Name
mainHelp kc = summary
  where
    summary = vBox entries
    entries = [ heading $ T.pack mhVersion
              , headingNoPad $ T.pack mmApiVersion
              , heading "Help Topics"
              , drawHelpTopics
              , heading "Commands"
              , padTop (Pad 1) mkCommandHelpText
              , heading "Keybindings"
              ] <>
              (mkKeybindingHelp kc <$> keybindSections)

    mkCommandHelpText :: Widget Name
    mkCommandHelpText =
      let commandNameWidth = 2 + (maximum $ T.length <$> fst <$> commandHelpInfo)
      in vBox [ (emph $ txt $ padTo commandNameWidth info) <+> renderText desc
              | (info, desc) <- commandHelpInfo
              ]

commandHelpInfo :: [(T.Text, T.Text)]
commandHelpInfo = pairs
    where
        pairs = [ (info, desc)
                | Cmd cmd desc args _ <- cs
                , let argSpec = printArgSpec args
                      spc = if T.null argSpec then "" else " "
                      info = T.cons '/' cmd <> spc <> argSpec
                ]
        cs = sortWith commandName commandList

commandTextTable :: T.Text
commandTextTable =
    let commandNameWidth = 4 + (maximum $ T.length <$> fst <$> commandHelpInfo)
    in T.intercalate "\n" $
       [ padTo commandNameWidth info <> desc
       | (info, desc) <- commandHelpInfo
       ]

commandMarkdownTable :: T.Text
commandMarkdownTable =
    T.intercalate "\n" $
    [ "# Commands"
    , ""
    , "| Command | Description |"
    , "| ------- | ----------- |"
    ] <>
    [ "| `" <> info <> "` | " <> desc <> " |"
    | (info, desc) <- commandHelpInfo
    ]

drawHelpTopics :: Widget Name
drawHelpTopics =
    let allHelpTopics = drawTopic <$> helpTopics
        topicNameWidth = 4 + (maximum $ T.length <$> helpTopicName <$> helpTopics)
        drawTopic t = (emph $ txt (padTo topicNameWidth $ helpTopicName t)) <+>
                      txt (helpTopicDescription t)
    in vBox $ (padBottom (Pad 1) $
               para "Learn more about these topics with `/help <topic>`:")
            : allHelpTopics

helpContentWidth :: Int
helpContentWidth = 72

scriptHelp :: Widget Name
scriptHelp = heading "Using Scripts" <=> vBox scriptHelpText
  where scriptHelpText = map paraL
          [ [ "Matterhorn has a special feature that allows you to use "
             , "prewritten shell scripts to preprocess messages. "
             , "For example, this can allow you to run various filters over "
             , "your written text, do certain kinds of automated formatting, "
             , "or just automatically cowsay-ify a message." ]
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
             , "local error message." ]
           , [ "To run a script, simply type" ]
           , [ "> *> /sh [script-name] [my-message]*" ]
           , [ "And the script named *[script-name]* will be invoked with "
             , "the text of *[my-message]*. If the script does not exist, "
             , "or if it exists but is not marked as executable, you'll be "
             , "presented with an appropriate error message." ]
           , [ "For example, if you want to use a basic script to "
             , "automatically ROT13 your message, you can write a shell "
             , "script using the standard Unix *tr* utility, like this:" ]
           , [ "> *#!/bin/bash -e*"
             , "> *tr '[A-Za-z]' '[N-ZA-Mn-za-m]'*" ]
           , [ "Move this script to *~/.config/matterhorn/scripts/rot13* "
             , "and be sure it's executable with" ]
           , [ "> *$ chmod u+x ~/.config/matterhorn/scripts/rot13*" ]
           , [ "after which you can send ROT13 messages with the "
             , "Matterhorn command " ]
           , [ "> *> /sh rot13 Hello, world!*" ]
           ]

keybindingMarkdownTable :: KeyConfig -> Text
keybindingMarkdownTable kc = title <> keybindSectionStrings
    where title = "# Keybindings\n"
          keybindSectionStrings = T.concat $ sectionText <$> keybindSections
          sectionText = mkKeybindEventSectionHelp kc keybindEventHelpMarkdown T.unlines mkHeading
          mkHeading n =
              "\n# " <> n <>
              "\n| Keybinding | Event Name | Description |" <>
              "\n| ---------- | ---------- | ----------- |"

keybindingTextTable :: KeyConfig -> Text
keybindingTextTable kc = title <> keybindSectionStrings
    where title = "Keybindings\n===========\n"
          keybindSectionStrings = T.concat $ sectionText <$> keybindSections
          sectionText = mkKeybindEventSectionHelp kc (keybindEventHelpText keybindingWidth eventNameWidth) T.unlines mkHeading
          keybindingWidth = 15
          eventNameWidth = 30
          mkHeading n =
              "\n" <> n <>
              "\n" <> (T.replicate (T.length n) "=")

keybindingHelp :: KeyConfig -> Widget Name
keybindingHelp kc = vBox $
  [ heading "Configurable Keybindings"
  , padBottom (Pad 1) $ vBox keybindingHelpText
  ] ++ keybindSectionWidgets
    ++
  [ headingNoPad "Keybinding Syntax"
  , vBox validKeys
  ]
  where keybindSectionWidgets = sectionWidget <$> keybindSections
        sectionWidget = mkKeybindEventSectionHelp kc keybindEventHelpWidget vBox headingNoPad

        keybindingHelpText = map paraL
          [ [ "Many of the keybindings used in Matterhorn can be "
            , "modified from within Matterhorn's **config.ini** file. "
            , "To do this, include a section called **[KEYBINDINGS]** "
            , "in your config file and use the event names listed below as "
            , "keys and the desired key sequence as values. "
            , "See the end of this page for documentation on the valid "
            , "syntax for key sequences."
            ]
          , [ "For example, by default, the keybinding to move to the next "
            , "channel in the public channel list is **"
            , nextChanBinding
            , "**, and the corresponding "
            , "previous channel binding is **"
            , prevChanBinding
            , "**. You might want to remap these "
            , "to other keys: say, **C-j** and **C-k**. We can do this with the following "
            , "configuration snippet:"
            ]
          , [ "```ini\n"
            , "[KEYBINDINGS]\n"
            , "focus-next-channel = C-j\n"
            , "focus-prev-channel = C-k\n"
            , "```"
            ]
          , [ "You can remap a command to more than one key sequence, in which "
            , "case any one of the key sequences provided can be used to invoke "
            , "the relevant command. To do this, provide the desired bindings as "
            , "a comma-separated list. Additionally, some key combinations are "
            , "used in multiple modes (such as URL select or help viewing) and "
            , "therefore share the same name, such as **cancel** or **scroll-up**."
            ]
          , [ "Additionally, some keys simply cannot be remapped, mostly in the "
            , "case of editing keybindings. If you feel that a particular key "
            , "event should be rebindable and isn't, then please feel free to "
            , "let us know by posting an issue in the Matterhorn issue tracker."
            ]
          , [ "It is also possible to entirely unbind a key event by setting its "
            , "key to **unbound**, thus avoiding conflicts between default bindings "
            , "and new ones:"
            ]
          , [ "```ini\n"
            , "[KEYBINDINGS]\n"
            , "focus-next-channel = unbound\n"
            , "```"
            ]
          , [ "The rebindable key events, along with their **current** "
            , "values, are as follows:"
            ]
           ]
        nextChanBinding = ppBinding (getFirstDefaultBinding NextChannelEvent)
        prevChanBinding = ppBinding (getFirstDefaultBinding PrevChannelEvent)
        validKeys = map paraL
          [ [ "The syntax used for key sequences consists of zero or more "
            , "single-character modifier characters followed by a keystroke, "
            , "all separated by dashes. The available modifier keys are "
            , "**S** for Shift, **C** for Ctrl, **A** for Alt, and **M** for "
            , "Meta. So, for example, **"
            , ppBinding (Binding [] (Vty.KFun 2))
            , "** is the F2 key pressed with no "
            , "modifier keys; **"
            , ppBinding (Binding [Vty.MCtrl] (Vty.KChar 'x'))
            , "** is Ctrl and X pressed together, "
            , "and **"
            , ppBinding (Binding [Vty.MShift, Vty.MCtrl] (Vty.KChar 'x'))
            , "** is Shift, Ctrl, and X all pressed together. "
            , "Although Matterhorn will pretty-print all key combinations "
            , "with specific capitalization, the parser is **not** case-sensitive "
            , "and will ignore any capitalization."
            ]
          , [ "Your terminal emulator might not recognize some particular "
            , "keypress combinations, or it might reserve certain combinations of "
            , "keys for some terminal-specific operation. Matterhorn does not have a "
            , "reliable way of testing this, so it is up to you to avoid setting "
            , "keybindings that your terminal emulator does not deliver to applications."
            ]
          , [ "Letter keys, number keys, and function keys are specified with "
            , "their obvious name, such as **x** for the X key, **8** for the 8 "
            , "key, and **f5** for the F5 key. Other valid keys include: "
            , T.intercalate ", " [ "**" <> key <> "**" | key <- nonCharKeys ]
            , "."
            ]
          ]

emph :: Widget a -> Widget a
emph = withDefAttr helpEmphAttr

para :: Text -> Widget a
para t = padTop (Pad 1) $ renderText t

paraL :: [Text] -> Widget a
paraL = para . mconcat

heading :: Text -> Widget a
heading = padTop (Pad 1) . headingNoPad

headingNoPad :: Text -> Widget a
headingNoPad t = hCenter $ emph $ renderText t

syntaxHighlightHelp :: [FilePath] -> Widget a
syntaxHighlightHelp dirs = vBox
  [ heading "Syntax Highlighting"

  , para $ "Matterhorn supports syntax highlighting in Markdown code blocks when the " <>
           "name of the code block language follows the block opening sytnax:"
  , para $ "```<language>"
  , para $ "The possible values of `language` are determined by the available syntax " <>
           "definitions. The available definitions are loaded from the following " <>
           "directories according to the configuration setting `syntaxDirectories`. " <>
           "If the setting is omitted, it defaults to the following sequence of directories:"
  , para $ T.pack $ intercalate "\n" $ (\d -> "`" <> d <> "`") <$> dirs
  , para $ "Syntax definitions are in the Kate XML format. Files with an " <>
           "`xml` extension are loaded from each directory, with directories earlier " <>
           "in the list taking precedence over later directories when more than one " <>
           "directory provides a definition file for the same syntax."
  , para $ "To place custom definitions in a directory, place a Kate " <>
           "XML syntax definition in the directory and ensure that a copy of " <>
           "`language.dtd` is also present. The file `language.dtd` can be found in " <>
           "the `syntax/` directory of your Matterhorn distribution."
  ]

themeHelp :: Widget a
themeHelp = vBox
  [ heading "Using Themes"
  , para "Matterhorn provides these built-in color themes:"
  , padTop (Pad 1) $ vBox $ hCenter <$> emph <$>
                            txt <$> internalThemeName <$> internalThemes
  , para $
        "These themes can be selected with the */theme* command. To automatically " <>
        "select a theme at startup, set the *theme* configuration file option to one " <>
        "of the themes listed above."

  , heading "Customizing the Theme"
  , para $
        "Theme customization is also supported. To customize the selected theme, " <>
        "create a theme customization file and set the `themeCustomizationFile` " <>
        "configuration option to the path to the customization file. If the path " <>
        "to the file is relative, Matterhorn will look for it in the same directory " <>
        "as the Matterhorn configuration file."

  , para $
        "Theme customization files are INI-style files that can customize any " <>
        "foreground color, background color, or style of any aspect of the " <>
        "Matterhorn user interface. Here is an example:"

  , para $
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
        "```"

  , para $
        "In the example above, the theme's default foreground and background colors " <>
        "are both customized to *blue* and *black*, respectively. The *default* section " <>
        "contains only customizations for the *default* attribute. All other customizations " <>
        "go in the *other* section. We can also set the style for attributes; we can either " <>
        "set just one style (as with the bold setting above) or multiple styles at once " <>
        "(as in the bold/underline example).\n"

  , para $
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
        " * brightWhite"

  , para $
        "Available styles are:\n" <>
        " * standout\n" <>
        " * underline\n" <>
        " * italic\n" <>
        " * strikethrough\n" <>
        " * reverseVideo\n" <>
        " * blink\n" <>
        " * dim\n" <>
        " * bold\n"

  , para $
        "It is also possible to specify RGB values using HTML syntax: `#RRGGBB`. " <>
        "Bear in mind that such colors are clamped to the nearest 256-color palette " <>
        "entry, so it is not possible to get the exact color specified.\n\n" <>
        "In addition, a special value of *default* is possible for either color " <>
        "setting of an attribute. This value indicates that the attribute should " <>
        "use the terminal emulator's default foreground or background color of " <>
        "choice rather than a specific ANSI color."

  , heading "Username Highlighting"
  , para $
        "Username colors are chosen by hashing each username and then using the hash " <>
        "to choose a color from a list of predefined username colors. If you would like " <>
        "to change the color in a given entry of this list, we provide the " <>
        "\"username.N\" attributes, where N is the index in the username color list."

  , heading "Theme Attributes"
  , para $
        "This section lists all possible theme attributes for use in customization " <>
        "files along with a description of how each one is used in Matterhorn. Each " <>
        "option listed can be set in the *other* section of the customization file. " <>
        "Each provides three customization settings:"

  , para $
        " * *<option>.fg = <color>*\n" <>
        " * *<option>.bg = <color>*\n" <>
        " * *<option>.style = <style>* or *<option>.style = [<style>, ...]*\n"

  , let names = sort $
                 (\(n, msg) -> (n, attrNameToConfig n, msg)) <$>
                 (M.toList $ themeDescriptions themeDocs)
        mkEntry (n, opt, msg) =
            padTop (Pad 1) $
            vBox [ hBox [ withDefAttr clientEmphAttr $ txt opt
                        , padLeft Max $ forceAttr n $ txt "(demo)"
                        ]
                 , txt msg
                 ]
    in vBox $ mkEntry <$> names
  ]

keybindSections :: [(Text, [KeyEventHandler])]
keybindSections =
    [ ("Global Keybindings", globalKeyHandlers)
    , ("Help Page", helpKeyHandlers)
    , ("Main Interface", mainKeyHandlers)
    , ("Text Editing", editingKeyHandlers (csEditState.cedEditor))
    , ("Channel Select Mode", channelSelectKeyHandlers)
    , ("Message Select Mode", messageSelectKeyHandlers)
    , ("User Listings", userListOverlayKeyHandlers)
    , ("URL Select Mode", urlSelectKeyHandlers)
    , ("Theme List Window", themeListOverlayKeyHandlers)
    , ("Channel Search Window", channelListOverlayKeyHandlers)
    , ("Message Viewer: Common", tabbedWindowKeyHandlers (csViewedMessage.singular _Just._2))
    , ("Message Viewer: Message tab", viewMessageKeyHandlers)
    , ("Message Viewer: Reactions tab", viewMessageReactionsKeyHandlers)
    , ("Attachment List", attachmentListKeyHandlers)
    , ("Attachment File Browser", attachmentBrowseKeyHandlers)
    , ("Flagged Messages", postListOverlayKeyHandlers)
    , ("Reaction Emoji Search Window", reactionEmojiListOverlayKeyHandlers)
    ]

helpBox :: Name -> Widget Name -> Widget Name
helpBox n helpText =
    withDefAttr helpAttr $
    viewport HelpViewport Vertical $
    cached n helpText

kbColumnWidth :: Int
kbColumnWidth = 14

kbDescColumnWidth :: Int
kbDescColumnWidth = 60

mkKeybindingHelp :: KeyConfig -> (Text, [KeyEventHandler]) -> Widget Name
mkKeybindingHelp kc (sectionName, kbs) =
    (heading sectionName) <=>
    (padTop (Pad 1) $ hCenter $ vBox $ snd <$> sortWith fst results)
    where
        results = mkKeybindHelp kc <$> kbs

mkKeybindHelp :: KeyConfig -> KeyEventHandler -> (Text, Widget Name)
mkKeybindHelp kc h =
    let unbound = ["(unbound)"]
        label = case kehEventTrigger h of
            Static k -> ppBinding $ eventToBinding k
            ByEvent ev ->
                let bindings = case M.lookup ev kc of
                        Nothing ->
                            let bs = defaultBindings ev
                            in if not $ null bs
                               then ppBinding <$> defaultBindings ev
                               else unbound
                        Just Unbound -> unbound
                        Just (BindingList bs) | not (null bs) -> ppBinding <$> bs
                                              | otherwise -> unbound
                in T.intercalate ", " bindings

        rendering = (emph $ txt $ padTo kbColumnWidth $
                      label) <+> txt " " <+>
                    (hLimit kbDescColumnWidth $ padRight Max $ renderText $
                     ehDescription $ kehHandler h)
    in (label, rendering)

mkKeybindEventSectionHelp :: KeyConfig
                          -> ((Either Text Text, Text, [Text]) -> a)
                          -> ([a] -> a)
                          -> (Text -> a)
                          -> (Text, [KeyEventHandler])
                          -> a
mkKeybindEventSectionHelp kc mkKeybindHelpFunc vertCat mkHeading (sectionName, kbs) =
  vertCat $ (mkHeading sectionName) :
            (mkKeybindHelpFunc <$> (mkKeybindEventHelp kc <$> kbs))

keybindEventHelpWidget :: (Either Text Text, Text, [Text]) -> Widget Name
keybindEventHelpWidget (evName, desc, evs) =
    let evText = T.intercalate ", " evs
        label = case evName of
            Left s -> txt $ "; " <> s
            Right s -> emph $ txt s
    in padBottom (Pad 1) $
       vBox [ txtWrap ("; " <> desc)
            , label <+> txt (" = " <> evText)
            ]

keybindEventHelpMarkdown :: (Either Text Text, Text, [Text]) -> Text
keybindEventHelpMarkdown (evName, desc, evs) =
    let quote s = "`" <> s <> "`"
        name = case evName of
            Left s -> s
            Right s -> quote s
    in "| " <> (T.intercalate ", " $ quote <$> evs) <>
       " | " <> name <>
       " | " <> desc <>
       " |"

keybindEventHelpText :: Int -> Int -> (Either Text Text, Text, [Text]) -> Text
keybindEventHelpText width eventNameWidth (evName, desc, evs) =
    let name = case evName of
            Left s -> s
            Right s -> s
    in padTo width (T.intercalate ", " evs) <> " " <>
       padTo eventNameWidth name <> " " <>
       desc

mkKeybindEventHelp :: KeyConfig -> KeyEventHandler -> (Either Text Text, Text, [Text])
mkKeybindEventHelp kc h =
  let trig = kehEventTrigger h
      unbound = ["(unbound)"]
      (label, evText) = case trig of
          Static key -> (Left "(non-customizable key)", [ppBinding $ eventToBinding key])
          ByEvent ev -> case M.lookup ev kc of
              Nothing ->
                  let name = keyEventName ev
                  in if not (null (defaultBindings ev))
                     then (Right name, ppBinding <$> defaultBindings ev)
                     else (Left name, unbound)
              Just Unbound -> (Right $ keyEventName ev, unbound)
              Just (BindingList bs) -> (Right $ keyEventName ev,
                                        if not (null bs)
                                        then ppBinding <$> bs
                                        else unbound
                                        )
  in (label, ehDescription $ kehHandler h, evText)

padTo :: Int -> Text -> Text
padTo n s = s <> T.replicate (n - T.length s) " "
