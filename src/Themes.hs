{-# LANGUAGE OverloadedStrings #-}
module Themes
  ( InternalTheme(..)

  , defaultTheme
  , internalThemes
  , lookupTheme
  , themeDocs

  -- * Attribute names
  , currentUserAttr
  , timeAttr
  , channelHeaderAttr
  , channelListHeaderAttr
  , currentChannelNameAttr
  , unreadChannelAttr
  , unreadGroupMarkerAttr
  , mentionsChannelAttr
  , urlAttr
  , codeAttr
  , emailAttr
  , emojiAttr
  , channelNameAttr
  , clientMessageAttr
  , clientHeaderAttr
  , clientEmphAttr
  , clientStrongAttr
  , dateTransitionAttr
  , pinnedMessageIndicatorAttr
  , newMessageTransitionAttr
  , gapMessageAttr
  , errorMessageAttr
  , helpAttr
  , helpEmphAttr
  , channelSelectPromptAttr
  , channelSelectMatchAttr
  , completionAlternativeListAttr
  , completionAlternativeCurrentAttr
  , dialogAttr
  , dialogEmphAttr
  , recentMarkerAttr
  , replyParentAttr
  , loadMoreAttr
  , urlListSelectedAttr
  , messageSelectAttr
  , messageSelectStatusAttr
  , misspellingAttr
  , editedMarkingAttr
  , editedRecentlyMarkingAttr
  , tabSelectedAttr
  , tabUnselectedAttr

  -- * Username formatting
  , colorUsername
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Themes
import           Brick.Widgets.List
import qualified Brick.Widgets.FileBrowser as FB
import           Brick.Widgets.Skylighting ( attrNameForTokenType
                                           , attrMappingsForStyle
                                           , highlightedCodeBlockAttr
                                           )
import           Data.Hashable ( hash )
import qualified Data.Map as M
import qualified Data.Text as T
import           Graphics.Vty
import qualified Skylighting.Styles as Sky
import           Skylighting.Types ( TokenType(..) )

import           Types ( specialUserMentions )


helpAttr :: AttrName
helpAttr = "help"

helpEmphAttr :: AttrName
helpEmphAttr = "helpEmphasis"

recentMarkerAttr :: AttrName
recentMarkerAttr = "recentChannelMarker"

replyParentAttr :: AttrName
replyParentAttr = "replyParentPreview"

pinnedMessageIndicatorAttr :: AttrName
pinnedMessageIndicatorAttr = "pinnedMessageIndicator"

loadMoreAttr :: AttrName
loadMoreAttr = "loadMoreMessages"

urlListSelectedAttr :: AttrName
urlListSelectedAttr = "urlListCursor"

messageSelectAttr :: AttrName
messageSelectAttr = "messageSelectCursor"

editedMarkingAttr :: AttrName
editedMarkingAttr = "editedMarking"

editedRecentlyMarkingAttr :: AttrName
editedRecentlyMarkingAttr = "editedRecentlyMarking"

dialogAttr :: AttrName
dialogAttr = "dialog"

dialogEmphAttr :: AttrName
dialogEmphAttr = "dialogEmphasis"

channelSelectMatchAttr :: AttrName
channelSelectMatchAttr = "channelSelectMatch"

channelSelectPromptAttr :: AttrName
channelSelectPromptAttr = "channelSelectPrompt"

completionAlternativeListAttr :: AttrName
completionAlternativeListAttr = "tabCompletionAlternative"

completionAlternativeCurrentAttr :: AttrName
completionAlternativeCurrentAttr = "tabCompletionCursor"

timeAttr :: AttrName
timeAttr = "time"

currentUserAttr :: AttrName
currentUserAttr = "currentUser"

channelHeaderAttr :: AttrName
channelHeaderAttr = "channelHeader"

channelListHeaderAttr :: AttrName
channelListHeaderAttr = "channelListSectionHeader"

currentChannelNameAttr :: AttrName
currentChannelNameAttr = "currentChannelName"

channelNameAttr :: AttrName
channelNameAttr = "channelName"

unreadChannelAttr :: AttrName
unreadChannelAttr = "unreadChannel"

unreadGroupMarkerAttr :: AttrName
unreadGroupMarkerAttr = "unreadChannelGroupMarker"

mentionsChannelAttr :: AttrName
mentionsChannelAttr = "channelWithMentions"

tabSelectedAttr :: AttrName
tabSelectedAttr = "tabSelected"

tabUnselectedAttr :: AttrName
tabUnselectedAttr = "tabUnselected"

dateTransitionAttr :: AttrName
dateTransitionAttr = "dateTransition"

newMessageTransitionAttr :: AttrName
newMessageTransitionAttr = "newMessageTransition"

urlAttr :: AttrName
urlAttr = "url"

codeAttr :: AttrName
codeAttr = "codeBlock"

emailAttr :: AttrName
emailAttr = "email"

emojiAttr :: AttrName
emojiAttr = "emoji"

clientMessageAttr :: AttrName
clientMessageAttr = "clientMessage"

clientHeaderAttr :: AttrName
clientHeaderAttr = "markdownHeader"

clientEmphAttr :: AttrName
clientEmphAttr = "markdownEmph"

clientStrongAttr :: AttrName
clientStrongAttr = "markdownStrong"

errorMessageAttr :: AttrName
errorMessageAttr = "errorMessage"

gapMessageAttr :: AttrName
gapMessageAttr = "gapMessage"

misspellingAttr :: AttrName
misspellingAttr = "misspelling"

messageSelectStatusAttr :: AttrName
messageSelectStatusAttr = "messageSelectStatus"

data InternalTheme =
    InternalTheme { internalThemeName :: Text
                  , internalTheme     :: Theme
                  }

lookupTheme :: Text -> Maybe InternalTheme
lookupTheme n = find ((== n) . internalThemeName) internalThemes

internalThemes :: [InternalTheme]
internalThemes = validateInternalTheme <$>
    [ darkColorTheme
    , lightColorTheme
    ]

validateInternalTheme :: InternalTheme -> InternalTheme
validateInternalTheme it =
    let un = undocumentedAttrNames (internalTheme it)
    in if not $ null un
       then error $ "Internal theme " <> show (T.unpack (internalThemeName it)) <>
                    " references undocumented attribute names: " <> show un
       else it

undocumentedAttrNames :: Theme -> [AttrName]
undocumentedAttrNames t =
    let noDocs k = isNothing $ attrNameDescription themeDocs k
    in filter noDocs (M.keys $ themeDefaultMapping t)

defaultTheme :: InternalTheme
defaultTheme = darkColorTheme

lightColorTheme :: InternalTheme
lightColorTheme = InternalTheme name theme
    where
        theme = newTheme def lightAttrs
        name = "builtin:light"
        def = black `on` white

lightAttrs :: [(AttrName, Attr)]
lightAttrs =
    let sty = Sky.kate
    in [ (timeAttr,                         fg black)
       , (currentUserAttr,                  defAttr `withStyle` bold)
       , (channelHeaderAttr,                fg black `withStyle` underline)
       , (channelListHeaderAttr,            fg cyan)
       , (currentChannelNameAttr,           black `on` yellow `withStyle` bold)
       , (unreadChannelAttr,                black `on` cyan   `withStyle` bold)
       , (unreadGroupMarkerAttr,            fg black `withStyle` bold)
       , (mentionsChannelAttr,              black `on` red    `withStyle` bold)
       , (urlAttr,                          fg brightYellow)
       , (emailAttr,                        fg yellow)
       , (codeAttr,                         fg magenta)
       , (emojiAttr,                        fg yellow)
       , (channelNameAttr,                  fg blue)
       , (clientMessageAttr,                fg black)
       , (clientEmphAttr,                   fg black `withStyle` bold)
       , (clientStrongAttr,                 fg black `withStyle` bold `withStyle` underline)
       , (clientHeaderAttr,                 fg red `withStyle` bold)
       , (dateTransitionAttr,               fg green)
       , (newMessageTransitionAttr,         black `on` yellow)
       , (errorMessageAttr,                 fg red)
       , (gapMessageAttr,                   fg red)
       , (helpAttr,                         fg black)
       , (pinnedMessageIndicatorAttr,       black `on` cyan)
       , (helpEmphAttr,                     fg blue `withStyle` bold)
       , (channelSelectMatchAttr,           black `on` magenta)
       , (channelSelectPromptAttr,          fg black)
       , (completionAlternativeListAttr,    white `on` blue)
       , (completionAlternativeCurrentAttr, black `on` yellow)
       , (dialogAttr,                       black `on` cyan)
       , (dialogEmphAttr,                   fg white)
       , (listSelectedFocusedAttr,          black `on` yellow)
       , (recentMarkerAttr,                 fg black `withStyle` bold)
       , (loadMoreAttr,                     black `on` cyan)
       , (urlListSelectedAttr,              black `on` yellow)
       , (messageSelectAttr,                black `on` yellow)
       , (messageSelectStatusAttr,          fg black)
       , (misspellingAttr,                  fg red `withStyle` underline)
       , (editedMarkingAttr,                fg yellow)
       , (editedRecentlyMarkingAttr,        black `on` yellow)
       , (tabSelectedAttr,                  black `on` yellow)
       , (FB.fileBrowserCurrentDirectoryAttr, white `on` blue)
       , (FB.fileBrowserSelectionInfoAttr,  white `on` blue)
       , (FB.fileBrowserDirectoryAttr,      fg blue)
       , (FB.fileBrowserBlockDeviceAttr,    fg magenta)
       , (FB.fileBrowserCharacterDeviceAttr, fg green)
       , (FB.fileBrowserNamedPipeAttr,      fg yellow)
       , (FB.fileBrowserSymbolicLinkAttr,   fg cyan)
       , (FB.fileBrowserUnixSocketAttr,     fg red)
       ] <>
       ((\(i, a) -> (usernameAttr i, a)) <$> zip [0..] usernameColors) <>
       (filter skipBaseCodeblockAttr $ attrMappingsForStyle sty)

darkAttrs :: [(AttrName, Attr)]
darkAttrs =
  let sty = Sky.espresso
  in [ (timeAttr,                         fg white)
     , (currentUserAttr,                  defAttr `withStyle` bold)
     , (channelHeaderAttr,                fg white `withStyle` underline)
     , (channelListHeaderAttr,            fg cyan)
     , (currentChannelNameAttr,           black `on` yellow `withStyle` bold)
     , (unreadChannelAttr,                black `on` cyan   `withStyle` bold)
     , (unreadGroupMarkerAttr,            fg white `withStyle` bold)
     , (mentionsChannelAttr,              black `on` brightMagenta `withStyle` bold)
     , (urlAttr,                          fg yellow)
     , (emailAttr,                        fg yellow)
     , (codeAttr,                         fg magenta)
     , (emojiAttr,                        fg yellow)
     , (channelNameAttr,                  fg cyan)
     , (pinnedMessageIndicatorAttr,       fg cyan `withStyle` bold)
     , (clientMessageAttr,                fg white)
     , (clientEmphAttr,                   fg white `withStyle` bold)
     , (clientStrongAttr,                 fg white `withStyle` bold `withStyle` underline)
     , (clientHeaderAttr,                 fg red `withStyle` bold)
     , (dateTransitionAttr,               fg green)
     , (newMessageTransitionAttr,         fg yellow `withStyle` bold)
     , (errorMessageAttr,                 fg red)
     , (gapMessageAttr,                   black `on` yellow)
     , (helpAttr,                         fg white)
     , (helpEmphAttr,                     fg cyan `withStyle` bold)
     , (channelSelectMatchAttr,           black `on` magenta)
     , (channelSelectPromptAttr,          fg white)
     , (completionAlternativeListAttr,    white `on` blue)
     , (completionAlternativeCurrentAttr, black `on` yellow)
     , (dialogAttr,                       black `on` cyan)
     , (dialogEmphAttr,                   fg white)
     , (listSelectedFocusedAttr,          black `on` yellow)
     , (recentMarkerAttr,                 fg yellow `withStyle` bold)
     , (loadMoreAttr,                     black `on` cyan)
     , (urlListSelectedAttr,              black `on` yellow)
     , (messageSelectAttr,                black `on` yellow)
     , (messageSelectStatusAttr,          fg white)
     , (misspellingAttr,                  fg red `withStyle` underline)
     , (editedMarkingAttr,                fg yellow)
     , (editedRecentlyMarkingAttr,        black `on` yellow)
     , (tabSelectedAttr,                  black `on` yellow)
     , (FB.fileBrowserCurrentDirectoryAttr, white `on` blue)
     , (FB.fileBrowserSelectionInfoAttr,  white `on` blue)
     , (FB.fileBrowserDirectoryAttr,      fg blue)
     , (FB.fileBrowserBlockDeviceAttr,    fg magenta)
     , (FB.fileBrowserCharacterDeviceAttr, fg green)
     , (FB.fileBrowserNamedPipeAttr,      fg yellow)
     , (FB.fileBrowserSymbolicLinkAttr,   fg cyan)
     , (FB.fileBrowserUnixSocketAttr,     fg red)
     ] <>
     ((\(i, a) -> (usernameAttr i, a)) <$> zip [0..] usernameColors) <>
     (filter skipBaseCodeblockAttr $ attrMappingsForStyle sty)

skipBaseCodeblockAttr :: (AttrName, Attr) -> Bool
skipBaseCodeblockAttr = ((/= highlightedCodeBlockAttr) . fst)

darkColorTheme :: InternalTheme
darkColorTheme = InternalTheme name theme
    where
        theme = newTheme def darkAttrs
        name = "builtin:dark"
        def = defAttr

usernameAttr :: Int -> AttrName
usernameAttr i = "username" <> (attrName $ show i)

-- | Render a string with a color chosen based on the text of a
-- username.
--
-- This function takes some display text and renders it using an
-- attribute based on the username associated with the text. If the
-- username associated with the text is equal to the username of
-- the user running Matterhorn, the display text is formatted with
-- 'currentAttr'. Otherwise it is formatted with an attribute chosen
-- by hashing the associated username and choosing from amongst the
-- username color hash buckets with 'usernameAttr'.
--
-- Usually the first argument to this function will be @myUsername st@,
-- where @st@ is a 'ChatState'.
--
-- The most common way to call this function is
--
-- @colorUsername (myUsername st) u u
--
-- The third argument is allowed to vary from the second since sometimes
-- we call this with the user's status sigil as the third argument.
colorUsername :: Text
              -- ^ The username for the user currently running
              -- Matterhorn
              -> Text
              -- ^ The username associated with the text to render
              -> Text
              -- ^ The text to render
              -> Widget a
colorUsername current username display =
    let normalizedUsername = T.toLower username
        aName = if normalizedUsername `elem` specialUserMentions
                then clientEmphAttr
                else usernameAttr h
        h = hash normalizedUsername `mod` length usernameColors
        maybeWithCurrentAttr = if current == username
                               then withAttr currentUserAttr
                               else id
    in withDefAttr aName $
       maybeWithCurrentAttr $
       txt (display)

usernameColors :: [Attr]
usernameColors =
    [ fg red
    , fg green
    , fg yellow
    , fg blue
    , fg magenta
    , fg cyan
    , fg brightRed
    , fg brightGreen
    , fg brightYellow
    , fg brightBlue
    , fg brightMagenta
    , fg brightCyan
    ]

-- Functions for dealing with Skylighting styles

attrNameDescription :: ThemeDocumentation -> AttrName -> Maybe Text
attrNameDescription td an = M.lookup an (themeDescriptions td)

themeDocs :: ThemeDocumentation
themeDocs = ThemeDocumentation $ M.fromList $
    [ ( timeAttr
      , "Timestamps on chat messages"
      )
    , ( channelHeaderAttr
      , "Channel headers displayed above chat message lists"
      )
    , ( channelListHeaderAttr
      , "The heading of the channel list sections"
      )
    , ( currentChannelNameAttr
      , "The currently selected channel in the channel list"
      )
    , ( unreadChannelAttr
      , "A channel in the channel list with unread messages"
      )
    , ( unreadGroupMarkerAttr
      , "The channel group marker indicating unread messages"
      )
    , ( mentionsChannelAttr
      , "A channel in the channel list with unread mentions"
      )
    , ( urlAttr
      , "A URL in a chat message"
      )
    , ( codeAttr
      , "A code block in a chat message with no language indication"
      )
    , ( emailAttr
      , "An e-mail address in a chat message"
      )
    , ( emojiAttr
      , "A text emoji indication in a chat message"
      )
    , ( channelNameAttr
      , "A channel name in a chat message"
      )
    , ( clientMessageAttr
      , "A Matterhorn diagnostic or informative message"
      )
    , ( clientHeaderAttr
      , "Markdown heading"
      )
    , ( clientEmphAttr
      , "Markdown 'emphasized' text"
      )
    , ( clientStrongAttr
      , "Markdown 'strong' text"
      )
    , ( dateTransitionAttr
      , "Date transition lines between chat messages on different days"
      )
    , ( pinnedMessageIndicatorAttr
      , "The indicator for messages that have been pinned"
      )
    , ( newMessageTransitionAttr
      , "The 'New Messages' line that appears above unread messages"
      )
    , ( tabSelectedAttr
      , "Selected tabs in tabbed windows"
      )
    , ( tabUnselectedAttr
      , "Unselected tabs in tabbed windows"
      )
    , ( errorMessageAttr
      , "Matterhorn error messages"
      )
    , ( gapMessageAttr
      , "Matterhorn message gap information"
      )
    , ( helpAttr
      , "The help screen text"
      )
    , ( helpEmphAttr
      , "The help screen's emphasized text"
      )
    , ( channelSelectPromptAttr
      , "Channel selection: prompt"
      )
    , ( channelSelectMatchAttr
      , "Channel selection: the portion of a channel name that matches"
      )
    , ( completionAlternativeListAttr
      , "Tab completion alternatives"
      )
    , ( completionAlternativeCurrentAttr
      , "The currently-selected tab completion alternative"
      )
    , ( dialogAttr
      , "Dialog box text"
      )
    , ( dialogEmphAttr
      , "Dialog box emphasized text"
      )
    , ( recentMarkerAttr
      , "The marker indicating the channel last visited"
      )
    , ( replyParentAttr
      , "The first line of parent messages appearing above reply messages"
      )
    , ( loadMoreAttr
      , "The 'Load More' line that appears at the top of a chat message list"
      )
    , ( urlListSelectedAttr
      , "URL list: the selected URL"
      )
    , ( messageSelectAttr
      , "Message selection: the currently-selected message"
      )
    , ( messageSelectStatusAttr
      , "Message selection: the message selection actions"
      )
    , ( misspellingAttr
      , "A misspelled word in the chat message editor"
      )
    , ( editedMarkingAttr
      , "The 'edited' marking that appears on edited messages"
      )
    , ( editedRecentlyMarkingAttr
      , "The 'edited' marking that appears on newly-edited messages"
      )
    , ( highlightedCodeBlockAttr
      , "The base attribute for syntax-highlighted code blocks"
      )
    , ( attrNameForTokenType KeywordTok
      , "Syntax highlighting: Keyword"
      )
    , ( attrNameForTokenType DataTypeTok
      , "Syntax highlighting: DataType"
      )
    , ( attrNameForTokenType DecValTok
      , "Syntax highlighting: Declaration"
      )
    , ( attrNameForTokenType BaseNTok
      , "Syntax highlighting: BaseN"
      )
    , ( attrNameForTokenType FloatTok
      , "Syntax highlighting: Float"
      )
    , ( attrNameForTokenType ConstantTok
      , "Syntax highlighting: Constant"
      )
    , ( attrNameForTokenType CharTok
      , "Syntax highlighting: Char"
      )
    , ( attrNameForTokenType SpecialCharTok
      , "Syntax highlighting: Special Char"
      )
    , ( attrNameForTokenType StringTok
      , "Syntax highlighting: String"
      )
    , ( attrNameForTokenType VerbatimStringTok
      , "Syntax highlighting: Verbatim String"
      )
    , ( attrNameForTokenType SpecialStringTok
      , "Syntax highlighting: Special String"
      )
    , ( attrNameForTokenType ImportTok
      , "Syntax highlighting: Import"
      )
    , ( attrNameForTokenType CommentTok
      , "Syntax highlighting: Comment"
      )
    , ( attrNameForTokenType DocumentationTok
      , "Syntax highlighting: Documentation"
      )
    , ( attrNameForTokenType AnnotationTok
      , "Syntax highlighting: Annotation"
      )
    , ( attrNameForTokenType CommentVarTok
      , "Syntax highlighting: Comment"
      )
    , ( attrNameForTokenType OtherTok
      , "Syntax highlighting: Other"
      )
    , ( attrNameForTokenType FunctionTok
      , "Syntax highlighting: Function"
      )
    , ( attrNameForTokenType VariableTok
      , "Syntax highlighting: Variable"
      )
    , ( attrNameForTokenType ControlFlowTok
      , "Syntax highlighting: Control Flow"
      )
    , ( attrNameForTokenType OperatorTok
      , "Syntax highlighting: Operator"
      )
    , ( attrNameForTokenType BuiltInTok
      , "Syntax highlighting: Built-In"
      )
    , ( attrNameForTokenType ExtensionTok
      , "Syntax highlighting: Extension"
      )
    , ( attrNameForTokenType PreprocessorTok
      , "Syntax highlighting: Preprocessor"
      )
    , ( attrNameForTokenType AttributeTok
      , "Syntax highlighting: Attribute"
      )
    , ( attrNameForTokenType RegionMarkerTok
      , "Syntax highlighting: Region Marker"
      )
    , ( attrNameForTokenType InformationTok
      , "Syntax highlighting: Information"
      )
    , ( attrNameForTokenType WarningTok
      , "Syntax highlighting: Warning"
      )
    , ( attrNameForTokenType AlertTok
      , "Syntax highlighting: Alert"
      )
    , ( attrNameForTokenType ErrorTok
      , "Syntax highlighting: Error"
      )
    , ( attrNameForTokenType NormalTok
      , "Syntax highlighting: Normal text"
      )
    , ( listSelectedFocusedAttr
      , "The selected channel"
      )
    , ( FB.fileBrowserAttr
      , "The base file browser attribute"
      )
    , ( FB.fileBrowserCurrentDirectoryAttr
      , "The file browser current directory attribute"
      )
    , ( FB.fileBrowserSelectionInfoAttr
      , "The file browser selection information attribute"
      )
    , ( FB.fileBrowserDirectoryAttr
      , "Attribute for directories in the file browser"
      )
    , ( FB.fileBrowserBlockDeviceAttr
      , "Attribute for block devices in the file browser"
      )
    , ( FB.fileBrowserRegularFileAttr
      , "Attribute for regular files in the file browser"
      )
    , ( FB.fileBrowserCharacterDeviceAttr
      , "Attribute for character devices in the file browser"
      )
    , ( FB.fileBrowserNamedPipeAttr
      , "Attribute for named pipes in the file browser"
      )
    , ( FB.fileBrowserSymbolicLinkAttr
      , "Attribute for symbolic links in the file browser"
      )
    , ( FB.fileBrowserUnixSocketAttr
      , "Attribute for Unix sockets in the file browser"
      )
    , ( currentUserAttr
      , "Attribute for the username of the user running Matterhorn"
      )
    ] <> [ (usernameAttr i, T.pack $ "Username color " <> show i)
         | i <- [0..(length usernameColors)-1]
         ]
