module Draw.Autocomplete
  ( autocompleteLayer
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.List ( renderList, listElementsL, listSelectedFocusedAttr
                                    , listSelectedElement
                                    )
import qualified Data.Text as T

import           Network.Mattermost.Types ( User(..), Channel(..) )

import           Draw.Util
import           Themes
import           Types
import           Types.Common ( sanitizeUserText )


autocompleteLayer :: ChatState -> Widget Name
autocompleteLayer st =
    case st^.csEditState.cedAutocomplete of
        Nothing ->
            emptyWidget
        Just ac ->
            renderAutocompleteBox st ac

userNotInChannelMarker :: T.Text
userNotInChannelMarker = "*"

renderAutocompleteBox :: ChatState -> AutocompleteState -> Widget Name
renderAutocompleteBox st ac =
    let matchList = _acCompletionList ac
        maxListHeight = 5
        visibleHeight = min maxListHeight numResults
        numResults = length elements
        elements = matchList^.listElementsL
        label = withDefAttr clientMessageAttr $
                txt $ _acListElementType ac <> ": " <> (T.pack $ show numResults) <>
                      " match" <> if numResults == 1 then "" else "es"

        selElem = snd <$> listSelectedElement matchList
        curChan = st^.csCurrentChannel
        footer = case renderAutocompleteFooterFor curChan =<< selElem of
            Just w -> hBorderWithLabel w
            _ -> hBorder

    in if numResults == 0
       then emptyWidget
       else Widget Greedy Greedy $ do
           ctx <- getContext
           let rowOffset = ctx^.availHeightL - 3 - editorOffset - visibleHeight
               editorOffset = if st^.csEditState.cedEphemeral.eesMultiline
                              then multilineHeightLimit
                              else 0
           render $ translateBy (Location (0, rowOffset)) $
                    vBox [ hBorderWithLabel label
                         , vLimit visibleHeight $
                           renderList renderAutocompleteAlternative True matchList
                         , footer
                         ]

renderAutocompleteFooterFor :: ClientChannel -> AutocompleteAlternative -> Maybe (Widget Name)
renderAutocompleteFooterFor _ (SpecialMention MentionChannel) = Nothing
renderAutocompleteFooterFor _ (SpecialMention MentionAll) = Nothing
renderAutocompleteFooterFor ch (UserCompletion _ False) =
    Just $ hBox [ txt $ "("
                , withDefAttr clientEmphAttr (txt userNotInChannelMarker)
                , txt ": not a member of "
                , withDefAttr channelNameAttr (txt $ normalChannelSigil <> ch^.ccInfo.cdName)
                , txt ")"
                ]
renderAutocompleteFooterFor _ (ChannelCompletion False ch) =
    Just $ hBox [ txt $ "("
                , withDefAttr clientEmphAttr (txt userNotInChannelMarker)
                , txt ": you are not a member of "
                , withDefAttr channelNameAttr (txt $ normalChannelSigil <> sanitizeUserText (channelName ch))
                , txt ")"
                ]
renderAutocompleteFooterFor _ _ = Nothing

renderAutocompleteAlternative :: Bool -> AutocompleteAlternative -> Widget Name
renderAutocompleteAlternative sel (EmojiCompletion e) =
    padRight Max $ renderEmojiCompletion sel e
renderAutocompleteAlternative sel (SpecialMention m) =
    padRight Max $ renderSpecialMention m sel
renderAutocompleteAlternative sel (UserCompletion u inChan) =
    padRight Max $ renderUserCompletion u inChan sel
renderAutocompleteAlternative sel (ChannelCompletion inChan c) =
    padRight Max $ renderChannelCompletion c inChan sel
renderAutocompleteAlternative _ (SyntaxCompletion t) =
    padRight Max $ txt t
renderAutocompleteAlternative _ (CommandCompletion n args desc) =
    padRight Max $ renderCommandCompletion n args desc

renderSpecialMention :: SpecialMention -> Bool -> Widget Name
renderSpecialMention m sel =
    let usernameWidth = 18
        padTo n a = hLimit n $ vLimit 1 (a <+> fill ' ')
        maybeForce = if sel
                     then forceAttr listSelectedFocusedAttr
                     else id
        t = autocompleteAlternativeReplacement $ SpecialMention m
        desc = case m of
            MentionChannel -> "Notifies all users in this channel"
            MentionAll     -> "Mentions all users in this channel"
    in maybeForce $
       hBox [ txt "  "
            , padTo usernameWidth $ withDefAttr clientEmphAttr $ txt t
            , txt desc
            ]

renderEmojiCompletion :: Bool -> T.Text -> Widget Name
renderEmojiCompletion sel e =
    let maybeForce = if sel
                     then forceAttr listSelectedFocusedAttr
                     else id
    in maybeForce $
       padLeft (Pad 2) $
       withDefAttr emojiAttr $
       txt $
       autocompleteAlternativeReplacement $ EmojiCompletion e

renderUserCompletion :: User -> Bool -> Bool -> Widget Name
renderUserCompletion u inChan selected =
    let usernameWidth = 18
        fullNameWidth = 25
        padTo n a = hLimit n $ vLimit 1 (a <+> fill ' ')
        username = userUsername u
        fullName = (sanitizeUserText $ userFirstName u) <> " " <>
                   (sanitizeUserText $ userLastName u)
        nickname = sanitizeUserText $ userNickname u
        maybeForce = if selected
                     then forceAttr listSelectedFocusedAttr
                     else id
        memberDisplay = if inChan
                        then txt "  "
                        else withDefAttr clientEmphAttr $
                             txt $ userNotInChannelMarker <> " "
    in maybeForce $
       hBox [ memberDisplay
            , padTo usernameWidth $ colorUsername username ("@" <> username)
            , padTo fullNameWidth $ txt fullName
            , txt nickname
            ]

renderChannelCompletion :: Channel -> Bool -> Bool -> Widget Name
renderChannelCompletion c inChan selected =
    let nameWidth = 30
        padTo n a = hLimit n $ vLimit 1 (a <+> fill ' ')
        maybeForce = if selected
                     then forceAttr listSelectedFocusedAttr
                     else id
        memberDisplay = if inChan
                        then txt "  "
                        else withDefAttr clientEmphAttr $
                             txt $ userNotInChannelMarker <> " "
    in maybeForce $
       hBox [ memberDisplay
            , padTo nameWidth $
              withDefAttr channelNameAttr $
              txt $ normalChannelSigil <> (sanitizeUserText $ channelName c)
            , txt $ sanitizeUserText $ channelHeader c
            ]

renderCommandCompletion :: Text -> Text -> Text -> Widget Name
renderCommandCompletion name args desc =
    withDefAttr clientMessageAttr
        (txt $ "/" <> name <> if T.null args then "" else " " <> args) <+>
    (txt $ " - " <> desc)
