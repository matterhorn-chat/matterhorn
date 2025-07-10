{-# LANGUAGE OverloadedStrings #-}
module Matterhorn.Themes
  ( InternalTheme(..)

  , defaultTheme
  , internalThemes
  , lookupTheme
  , themeDocs

  -- * Attribute names
  , currentUserAttr
  , timeAttr
  , verbatimTruncateMessageAttr
  , channelHeaderAttr
  , channelListHeaderAttr
  , currentChannelNameAttr
  , unreadChannelAttr
  , unreadGroupMarkerAttr
  , mentionsChannelAttr
  , currentTeamAttr
  , urlAttr
  , codeAttr
  , emailAttr
  , emojiAttr
  , reactionAttr
  , myReactionAttr
  , channelNameAttr
  , clientMessageAttr
  , clientHeaderAttr
  , strikeThroughAttr
  , clientEmphAttr
  , clientStrongAttr
  , dateTransitionAttr
  , pinnedMessageIndicatorAttr
  , newMessageTransitionAttr
  , gapMessageAttr
  , errorMessageAttr
  , helpAttr
  , helpEmphAttr
  , helpKeyEventAttr
  , channelSelectPromptAttr
  , channelSelectMatchAttr
  , completionAlternativeListAttr
  , completionAlternativeCurrentAttr
  , permalinkAttr
  , dialogAttr
  , dialogEmphAttr
  , recentMarkerAttr
  , replyParentAttr
  , loadMoreAttr
  , urlListSelectedAttr
  , messageSelectAttr
  , messageSelectStatusAttr
  , urlSelectStatusAttr
  , misspellingAttr
  , editedMarkingAttr
  , editedRecentlyMarkingAttr
  , tabSelectedAttr
  , tabUnselectedAttr
  , buttonAttr
  , buttonFocusedAttr
  , threadAttr
  , focusedEditorPromptAttr

  -- * Username formatting
  , colorUsername
  , attrForUsername
  , usernameColorHashBuckets
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Themes
import           Brick.Widgets.List
import qualified Brick.Widgets.FileBrowser as FB
import           Brick.Widgets.Skylighting ( attrNameForTokenType
                                           , attrMappingsForStyle
                                           , highlightedCodeBlockAttr
                                           )
import           Brick.Forms ( focusedFormInputAttr )
import qualified Data.Digest.CRC32 as CRC
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Graphics.Vty
import qualified Skylighting.Styles as Sky
import           Skylighting.Types ( TokenType(..) )

import           Matterhorn.Types ( InternalTheme(..), specialUserMentions )


helpAttr :: AttrName
helpAttr = attrName "help"

helpEmphAttr :: AttrName
helpEmphAttr = attrName "helpEmphasis"

helpKeyEventAttr :: AttrName
helpKeyEventAttr = attrName "helpKeyEvent"

recentMarkerAttr :: AttrName
recentMarkerAttr = attrName "recentChannelMarker"

replyParentAttr :: AttrName
replyParentAttr = attrName "replyParentPreview"

pinnedMessageIndicatorAttr :: AttrName
pinnedMessageIndicatorAttr = attrName "pinnedMessageIndicator"

loadMoreAttr :: AttrName
loadMoreAttr = attrName "loadMoreMessages"

urlListSelectedAttr :: AttrName
urlListSelectedAttr = attrName "urlListCursor"

messageSelectAttr :: AttrName
messageSelectAttr = attrName "messageSelectCursor"

editedMarkingAttr :: AttrName
editedMarkingAttr = attrName "editedMarking"

editedRecentlyMarkingAttr :: AttrName
editedRecentlyMarkingAttr = attrName "editedRecentlyMarking"

permalinkAttr :: AttrName
permalinkAttr = attrName "permalink"

dialogAttr :: AttrName
dialogAttr = attrName "dialog"

dialogEmphAttr :: AttrName
dialogEmphAttr = attrName "dialogEmphasis"

channelSelectMatchAttr :: AttrName
channelSelectMatchAttr = attrName "channelSelectMatch"

channelSelectPromptAttr :: AttrName
channelSelectPromptAttr = attrName "channelSelectPrompt"

completionAlternativeListAttr :: AttrName
completionAlternativeListAttr = attrName "tabCompletionAlternative"

completionAlternativeCurrentAttr :: AttrName
completionAlternativeCurrentAttr = attrName "tabCompletionCursor"

timeAttr :: AttrName
timeAttr = attrName "time"

currentUserAttr :: AttrName
currentUserAttr = attrName "currentUser"

channelHeaderAttr :: AttrName
channelHeaderAttr = attrName "channelHeader"

verbatimTruncateMessageAttr :: AttrName
verbatimTruncateMessageAttr = attrName "verbatimTruncateMessage"

channelListHeaderAttr :: AttrName
channelListHeaderAttr = attrName "channelListSectionHeader"

currentChannelNameAttr :: AttrName
currentChannelNameAttr = attrName "currentChannelName"

channelNameAttr :: AttrName
channelNameAttr = attrName "channelName"

unreadChannelAttr :: AttrName
unreadChannelAttr = attrName "unreadChannel"

unreadGroupMarkerAttr :: AttrName
unreadGroupMarkerAttr = attrName "unreadChannelGroupMarker"

mentionsChannelAttr :: AttrName
mentionsChannelAttr = attrName "channelWithMentions"

currentTeamAttr :: AttrName
currentTeamAttr = attrName "currentTeam"

tabSelectedAttr :: AttrName
tabSelectedAttr = attrName "tabSelected"

tabUnselectedAttr :: AttrName
tabUnselectedAttr = attrName "tabUnselected"

dateTransitionAttr :: AttrName
dateTransitionAttr = attrName "dateTransition"

newMessageTransitionAttr :: AttrName
newMessageTransitionAttr = attrName "newMessageTransition"

urlAttr :: AttrName
urlAttr = attrName "url"

codeAttr :: AttrName
codeAttr = attrName "codeBlock"

emailAttr :: AttrName
emailAttr = attrName "email"

emojiAttr :: AttrName
emojiAttr = attrName "emoji"

reactionAttr :: AttrName
reactionAttr = attrName "reaction"

myReactionAttr :: AttrName
myReactionAttr = reactionAttr <> attrName "mine"

clientMessageAttr :: AttrName
clientMessageAttr = attrName "clientMessage"

clientHeaderAttr :: AttrName
clientHeaderAttr = attrName "markdownHeader"

strikeThroughAttr :: AttrName
strikeThroughAttr = attrName "markdownStrikethrough"

clientEmphAttr :: AttrName
clientEmphAttr = attrName "markdownEmph"

clientStrongAttr :: AttrName
clientStrongAttr = attrName "markdownStrong"

errorMessageAttr :: AttrName
errorMessageAttr = attrName "errorMessage"

gapMessageAttr :: AttrName
gapMessageAttr = attrName "gapMessage"

misspellingAttr :: AttrName
misspellingAttr = attrName "misspelling"

messageSelectStatusAttr :: AttrName
messageSelectStatusAttr = attrName "messageSelectStatus"

urlSelectStatusAttr :: AttrName
urlSelectStatusAttr = attrName "urlSelectStatus"

buttonAttr :: AttrName
buttonAttr = attrName "button"

buttonFocusedAttr :: AttrName
buttonFocusedAttr = buttonAttr <> attrName "focused"

threadAttr :: AttrName
threadAttr = attrName "thread"

focusedEditorPromptAttr :: AttrName
focusedEditorPromptAttr = attrName "focusedEditorPrompt"

lookupTheme :: Text -> Maybe InternalTheme
lookupTheme n = find ((== n) . internalThemeName) internalThemes

internalThemes :: [InternalTheme]
internalThemes = validateInternalTheme <$>
    [ darkColorTheme
    , darkColor256Theme
    , lightColorTheme
    , lightColor256Theme
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
lightColorTheme = InternalTheme name theme desc
    where
        theme = newTheme def $ lightAttrs usernameColors16
        name = "builtin:light"
        def = black `on` white
        desc = "A 16-color theme for terminal windows with light background colors"

lightColor256Theme :: InternalTheme
lightColor256Theme = InternalTheme name theme desc
    where
        theme = newTheme def $ lightAttrs usernameColors256
        name = "builtin:light256"
        def = black `on` white
        desc = "Like builtin:light, but with 256-color username colors"

lightAttrs :: [Attr] -> [(AttrName, Attr)]
lightAttrs usernameColors =
    let sty = Sky.kate
    in [ (timeAttr,                         fg black)
       , (buttonAttr,                       black `on` cyan)
       , (buttonFocusedAttr,                black `on` yellow)
       , (threadAttr,                       defAttr)
       , (focusedEditorPromptAttr,          fg yellow `withStyle` bold)
       , (currentUserAttr,                  defAttr `withStyle` bold)
       , (channelHeaderAttr,                fg black)
       , (verbatimTruncateMessageAttr,      fg blue)
       , (scrollbarAttr,                    defAttr)
       , (scrollbarHandleAttr,              defAttr `withStyle` reverseVideo)
       , (scrollbarTroughAttr,              defAttr)
       , (channelListHeaderAttr,            fg cyan)
       , (currentChannelNameAttr,           black `on` yellow `withStyle` bold)
       , (unreadChannelAttr,                black `on` cyan   `withStyle` bold)
       , (unreadGroupMarkerAttr,            fg black `withStyle` bold)
       , (mentionsChannelAttr,              black `on` red    `withStyle` bold)
       , (urlAttr,                          fg brightYellow)
       , (emailAttr,                        fg yellow)
       , (codeAttr,                         fg magenta)
       , (emojiAttr,                        fg yellow)
       , (reactionAttr,                     fg yellow)
       , (myReactionAttr,                   fg yellow `withStyle` underline)
       , (channelNameAttr,                  fg blue)
       , (clientMessageAttr,                fg black)
       , (clientEmphAttr,                   fg black `withStyle` bold)
       , (clientStrongAttr,                 fg black `withStyle` bold `withStyle` underline)
       , (clientHeaderAttr,                 fg red `withStyle` bold)
       , (strikeThroughAttr,                defAttr `withStyle` strikethrough)
       , (dateTransitionAttr,               fg green)
       , (newMessageTransitionAttr,         black `on` yellow)
       , (errorMessageAttr,                 fg red)
       , (gapMessageAttr,                   fg red)
       , (helpAttr,                         fg black)
       , (pinnedMessageIndicatorAttr,       black `on` cyan)
       , (helpEmphAttr,                     fg blue `withStyle` bold)
       , (helpKeyEventAttr,                 fg magenta)
       , (channelSelectMatchAttr,           black `on` magenta `withStyle` underline)
       , (channelSelectPromptAttr,          fg black)
       , (completionAlternativeListAttr,    white `on` blue)
       , (completionAlternativeCurrentAttr, black `on` yellow)
       , (dialogAttr,                       black `on` cyan)
       , (dialogEmphAttr,                   fg white)
       , (permalinkAttr,                    fg green)
       , (listSelectedFocusedAttr,          black `on` yellow)
       , (recentMarkerAttr,                 fg black `withStyle` bold)
       , (loadMoreAttr,                     black `on` cyan)
       , (urlListSelectedAttr,              black `on` yellow)
       , (messageSelectAttr,                black `on` yellow)
       , (messageSelectStatusAttr,          fg black)
       , (urlSelectStatusAttr,              fg black)
       , (misspellingAttr,                  fg red `withStyle` underline)
       , (editedMarkingAttr,                fg yellow)
       , (editedRecentlyMarkingAttr,        black `on` yellow)
       , (tabSelectedAttr,                  black `on` yellow)
       , (focusedFormInputAttr,             black `on` yellow)
       , (currentTeamAttr,                  black `on` yellow)
       , (FB.fileBrowserCurrentDirectoryAttr, white `on` blue)
       , (FB.fileBrowserSelectionInfoAttr,  white `on` blue)
       , (FB.fileBrowserDirectoryAttr,      fg blue)
       , (FB.fileBrowserBlockDeviceAttr,    fg magenta)
       , (FB.fileBrowserCharacterDeviceAttr, fg green)
       , (FB.fileBrowserNamedPipeAttr,      fg yellow)
       , (FB.fileBrowserSymbolicLinkAttr,   fg cyan)
       , (FB.fileBrowserUnixSocketAttr,     fg red)
       ] <>
       ((\(i, a) -> (usernameAttr i, a)) <$> zip [0..usernameColorHashBuckets-1] (cycle usernameColors)) <>
       (filter skipBaseCodeblockAttr $ attrMappingsForStyle sty)

darkAttrs :: [Attr] -> [(AttrName, Attr)]
darkAttrs usernameColors =
  let sty = Sky.espresso
  in [ (timeAttr,                         fg white)
     , (buttonAttr,                       black `on` cyan)
     , (buttonFocusedAttr,                black `on` yellow)
     , (threadAttr,                       defAttr)
     , (focusedEditorPromptAttr,          fg yellow `withStyle` bold)
     , (currentUserAttr,                  defAttr `withStyle` bold)
     , (channelHeaderAttr,                fg white)
     , (verbatimTruncateMessageAttr,      fg cyan)
     , (channelListHeaderAttr,            fg cyan)
     , (currentChannelNameAttr,           black `on` yellow `withStyle` bold)
     , (unreadChannelAttr,                black `on` cyan   `withStyle` bold)
     , (unreadGroupMarkerAttr,            fg white `withStyle` bold)
     , (mentionsChannelAttr,              black `on` brightMagenta `withStyle` bold)
     , (scrollbarAttr,                    defAttr)
     , (scrollbarHandleAttr,              defAttr `withStyle` reverseVideo)
     , (scrollbarTroughAttr,              defAttr)
     , (urlAttr,                          fg yellow)
     , (emailAttr,                        fg yellow)
     , (codeAttr,                         fg magenta)
     , (emojiAttr,                        fg yellow)
     , (reactionAttr,                     fg yellow)
     , (myReactionAttr,                   fg yellow `withStyle` underline)
     , (channelNameAttr,                  fg cyan)
     , (pinnedMessageIndicatorAttr,       fg cyan `withStyle` bold)
     , (clientMessageAttr,                fg white)
     , (clientEmphAttr,                   fg white `withStyle` bold)
     , (clientStrongAttr,                 fg white `withStyle` bold `withStyle` underline)
     , (clientHeaderAttr,                 fg red `withStyle` bold)
     , (strikeThroughAttr,                defAttr `withStyle` strikethrough)
     , (dateTransitionAttr,               fg green)
     , (newMessageTransitionAttr,         fg yellow `withStyle` bold)
     , (errorMessageAttr,                 fg red)
     , (gapMessageAttr,                   black `on` yellow)
     , (helpAttr,                         fg white)
     , (helpEmphAttr,                     fg cyan `withStyle` bold)
     , (helpKeyEventAttr,                 fg yellow)
     , (channelSelectMatchAttr,           black `on` magenta `withStyle` underline)
     , (channelSelectPromptAttr,          fg white)
     , (completionAlternativeListAttr,    white `on` blue)
     , (completionAlternativeCurrentAttr, black `on` yellow)
     , (dialogAttr,                       black `on` cyan)
     , (dialogEmphAttr,                   fg white)
     , (permalinkAttr,                    fg brightCyan)
     , (listSelectedFocusedAttr,          black `on` yellow)
     , (recentMarkerAttr,                 fg yellow `withStyle` bold)
     , (loadMoreAttr,                     black `on` cyan)
     , (urlListSelectedAttr,              black `on` yellow)
     , (messageSelectAttr,                black `on` yellow)
     , (messageSelectStatusAttr,          fg white)
     , (urlSelectStatusAttr,              fg white)
     , (misspellingAttr,                  fg red `withStyle` underline)
     , (editedMarkingAttr,                fg yellow)
     , (editedRecentlyMarkingAttr,        black `on` yellow)
     , (tabSelectedAttr,                  black `on` yellow)
     , (focusedFormInputAttr,             black `on` yellow)
     , (currentTeamAttr,                  black `on` yellow)
     , (FB.fileBrowserCurrentDirectoryAttr, white `on` blue)
     , (FB.fileBrowserSelectionInfoAttr,  white `on` blue)
     , (FB.fileBrowserDirectoryAttr,      fg blue)
     , (FB.fileBrowserBlockDeviceAttr,    fg magenta)
     , (FB.fileBrowserCharacterDeviceAttr, fg green)
     , (FB.fileBrowserNamedPipeAttr,      fg yellow)
     , (FB.fileBrowserSymbolicLinkAttr,   fg cyan)
     , (FB.fileBrowserUnixSocketAttr,     fg red)
     ] <>
     ((\(i, a) -> (usernameAttr i, a)) <$> zip [0..usernameColorHashBuckets-1] (cycle usernameColors)) <>
     (filter skipBaseCodeblockAttr $ attrMappingsForStyle sty)

skipBaseCodeblockAttr :: (AttrName, Attr) -> Bool
skipBaseCodeblockAttr = ((/= highlightedCodeBlockAttr) . fst)

darkColorTheme :: InternalTheme
darkColorTheme = InternalTheme name theme desc
    where
        theme = newTheme def $ darkAttrs usernameColors16
        name = "builtin:dark"
        def = defAttr
        desc = "A 16-color theme for terminal windows with dark background colors"

darkColor256Theme :: InternalTheme
darkColor256Theme = InternalTheme name theme desc
    where
        theme = newTheme def $ darkAttrs usernameColors256
        name = "builtin:dark256"
        def = defAttr
        desc = "Like builtin:dark, but with 256-color username colors"

usernameAttr :: Int -> AttrName
usernameAttr i = attrName "username" <> (attrName $ show i)

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
    let aName = attrForUsername username
        maybeWithCurrentAttr = if current == username
                               then withAttr currentUserAttr
                               else id
    in withDefAttr aName $
       maybeWithCurrentAttr $
       txt display

-- | Return the attribute name to use for the specified username.
-- The input username is expected to be the username only (i.e. no
-- sigil).
--
-- If the input username is a special reserved username such as "all",
-- the @clientEmphAttr@ attribute name will be returned. Otherwise
-- a hash-bucket username attribute name will be returned based on
-- the hash value of the username and the number of hash buckets
-- (@usernameColorHashBuckets@).
attrForUsername :: Text
                -- ^ The username to get an attribute for
                -> AttrName
attrForUsername username =
    let normalizedUsername = T.toLower username
        aName = if normalizedUsername `elem` specialUserMentions
                then clientEmphAttr
                else usernameAttr h
        normalizedUsernameBytes = TE.encodeUtf8 username
        -- NB: we use CRC32 because it is a stable hash. Unstable
        -- hashes (such as 'hash' from the 'hashable' package) result
        -- in username color choices drifting over time, so we need a
        -- stable hash here (along with being careful not to change the
        -- number of buckets).
        crc = CRC.crc32 normalizedUsernameBytes
        h = fromEnum $ crc `mod` toEnum usernameColorHashBuckets
    in aName

-- | The number of hash buckets to use when hashing usernames to choose
-- their colors.
usernameColorHashBuckets :: Int
usernameColorHashBuckets = 50

usernameColors16 :: [Attr]
usernameColors16 =
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

usernameColors256 :: [Attr]
usernameColors256 = mkColor <$> username256ColorChoices
    where
        mkColor (r, g, b) = defAttr `withForeColor` rgbColor r g b

username256ColorChoices :: [(Integer, Integer, Integer)]
username256ColorChoices =
    [ (255, 0, 86)
    , (158, 0, 142)
    , (14, 76, 161)
    , (255, 229, 2)
    , (149, 0, 58)
    , (255, 147, 126)
    , (164, 36, 0)
    , (98, 14, 0)
    , (0, 0, 255)
    , (106, 130, 108)
    , (0, 174, 126)
    , (194, 140, 159)
    , (0, 143, 156)
    , (95, 173, 78)
    , (255, 2, 157)
    , (255, 116, 163)
    , (152, 255, 82)
    , (167, 87, 64)
    , (254, 137, 0)
    , (1, 208, 255)
    , (187, 136, 0)
    , (117, 68, 177)
    , (165, 255, 210)
    , (122, 71, 130)
    , (0, 71, 84)
    , (181, 0, 255)
    , (144, 251, 146)
    , (189, 211, 147)
    , (229, 111, 254)
    , (222, 255, 116)
    , (0, 255, 120)
    , (0, 155, 255)
    , (0, 100, 1)
    , (0, 118, 255)
    , (133, 169, 0)
    , (0, 185, 23)
    , (120, 130, 49)
    , (0, 255, 198)
    , (255, 110, 65)
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
    , ( reactionAttr
      , "An emoji reaction on a chat message"
      )
    , ( myReactionAttr
      , "An emoji reaction that the current user has posted on a chat message"
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
    , ( strikeThroughAttr
      , "Markdown strikethrough text"
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
    , ( helpKeyEventAttr
      , "The help screen's mention of key event names"
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
    , ( permalinkAttr
      , "A post permalink"
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
    , ( urlSelectStatusAttr
      , "Link selection: the message selection actions"
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
    , ( focusedFormInputAttr
      , "A form input that has focus"
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
    , ( buttonAttr
      , "Attribute for input form buttons"
      )
    , ( buttonFocusedAttr
      , "Attribute for focused input form buttons"
      )
    , ( currentUserAttr
      , "Attribute for the username of the user running Matterhorn"
      )
    , ( currentTeamAttr
      , "The currently-selected team"
      )
    , ( verbatimTruncateMessageAttr
      , "Attribute for a message indicating that a verbatim or code block has been only partially displayed"
      )
    , ( scrollbarAttr
      , "Base aAttribute for scroll bars"
      )
    , ( scrollbarTroughAttr
      , "Attribute for scroll bar troughs"
      )
    , ( scrollbarHandleAttr
      , "Attribute for scroll bar handles"
      )
    , ( threadAttr
      , "Base attribute for the thread window"
      )
    , ( focusedEditorPromptAttr
      , "The attribute for the prompt of the focused message editor"
      )
    ] <> [ (usernameAttr i, T.pack $ "Username color " <> show i)
         | i <- [0..usernameColorHashBuckets-1]
         ]
