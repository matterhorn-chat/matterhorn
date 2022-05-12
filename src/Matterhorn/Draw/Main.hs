{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
module Matterhorn.Draw.Main
  ( drawMain
  , drawDeleteMessageConfirm
  , drawChannelSelectPrompt
  , drawMessageInterface
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center ( centerLayer, center )
import           Brick.Widgets.List ( listElements )
import           Brick.Widgets.Edit ( editContentsL, renderEditor, getEditContents )
import           Control.Arrow ( (>>>) )
import           Data.Char ( isSpace, isPunctuation )
import qualified Data.Foldable as F
import           Data.List ( intersperse )
import           Data.Maybe ( fromJust )
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Zipper ( cursorPosition, insertChar, getText, gotoEOL )
import           Data.Time.Calendar ( fromGregorian )
import           Data.Time.Clock ( UTCTime(..) )
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( (.~), (^?!), to, view, Lens', Traversal', SimpleGetter )

import           Network.Mattermost.Types ( ChannelId, Type(Direct, Private, Group)
                                          , ServerTime(..), UserId, TeamId, teamDisplayName
                                          , teamId
                                          )


import           Matterhorn.Constants
import           Matterhorn.Draw.ChannelList ( renderChannelList )
import           Matterhorn.Draw.Messages
import           Matterhorn.Draw.Autocomplete
import           Matterhorn.Draw.Util
import           Matterhorn.Draw.RichText
import           Matterhorn.Events.Keybindings
import           Matterhorn.Events.MessageSelect
import           Matterhorn.State.MessageSelect
import           Matterhorn.Themes
import           Matterhorn.TimeUtils ( justAfter, justBefore )
import           Matterhorn.Types
import           Matterhorn.Types.Common ( sanitizeUserText )
import           Matterhorn.Types.DirectionalSeq ( emptyDirSeq )
import           Matterhorn.Types.RichText ( parseMarkdown, TeamBaseURL )
import           Matterhorn.Types.KeyEvents
import qualified Matterhorn.Zipper as Z


previewFromInput :: TeamBaseURL -> Maybe MessageType -> UserId -> Text -> Maybe Message
previewFromInput _ _ _ s | s == T.singleton cursorSentinel = Nothing
previewFromInput baseUrl overrideTy uId s =
    -- If it starts with a slash but not /me, this has no preview
    -- representation
    let isCommand = "/" `T.isPrefixOf` s
        isEmoteCmd = "/me " `T.isPrefixOf` s
        content = if isEmoteCmd
                  then T.stripStart $ T.drop 3 s
                  else s
        msgTy = fromMaybe (if isEmoteCmd then CP Emote else CP NormalPost) overrideTy
    in if isCommand && not isEmoteCmd
       then Nothing
       else Just $ Message { _mText          = parseMarkdown (Just baseUrl) content
                           , _mMarkdownSource = content
                           , _mUser          = UserI False uId
                           , _mDate          = ServerTime $ UTCTime (fromGregorian 1970 1 1) 0
                           -- The date is not used for preview
                           -- rendering, but we need to provide one.
                           -- Ideally we'd just use today's date, but
                           -- the rendering function is pure so we
                           -- can't.
                           , _mType          = msgTy
                           , _mPending       = False
                           , _mDeleted       = False
                           , _mAttachments   = mempty
                           , _mInReplyToMsg  = NotAReply
                           , _mMessageId     = Nothing
                           , _mReactions     = mempty
                           , _mOriginalPost  = Nothing
                           , _mFlagged       = False
                           , _mPinned        = False
                           , _mChannelId     = Nothing
                           }

-- | Tokens in spell check highlighting.
data Token =
    Ignore Text
    -- ^ This bit of text is to be ignored for the purposes of
    -- spell-checking.
    | Check Text
    -- ^ This bit of text should be checked against the spell checker's
    -- misspelling list.
    deriving (Show)

drawEditorContents :: ChatState -> SimpleGetter ChatState EditState -> TeamId -> HighlightSet -> [Text] -> Widget Name
drawEditorContents st editWhich tId hs =
    let noHighlight = txt . T.unlines
    in case st^.csTeam(tId).tsGlobalEditState.gedSpellChecker of
        Nothing -> noHighlight
        Just _ ->
            let ms = st^.editWhich.cedMisspellings
            in case S.null ms of
                True -> noHighlight
                False -> doHighlightMisspellings hs ms

-- | This function takes a set of misspellings from the spell
-- checker, the editor lines, and builds a rendering of the text with
-- misspellings highlighted.
--
-- This function processes each line of text from the editor as follows:
--
-- * Tokenize the line based on our rules for what constitutes
--   whitespace. We do this because we need to check "words" in the
--   user's input against the list of misspellings returned by the spell
--   checker. But to do this we need to ignore the same things that
--   Aspell ignores, and it ignores whitespace and lots of puncutation.
--   We also do this because once we have identified the misspellings
--   present in the input, we need to reconstruct the user's input and
--   that means preserving whitespace so that the input looks as it was
--   originally typed.
--
-- * Once we have a list of tokens -- the whitespace tokens to be
--   preserved but ignored and the tokens to be checked -- we check
--   each non-whitespace token for presence in the list of misspellings
--   reported by the checker.
--
-- * Having indicated which tokens correspond to misspelled words, we
--   then need to coallesce adjacent tokens that are of the same
--   "misspelling status", i.e., two neighboring tokens (of whitespace
--   or check type) need to be coallesced if they both correspond to
--   text that is a misspelling or if they both are NOT a misspelling.
--   We do this so that the final Brick widget is optimal in that it
--   uses a minimal number of box cells to display substrings that have
--   the same attribute.
--
-- * Finally we build a widget out of these coallesced tokens and apply
--   the misspellingAttr attribute to the misspelled tokens.
--
-- Note that since we have to come to our own conclusion about which
-- words are worth checking in the checker's output, sometimes our
-- algorithm will differ from aspell in what is considered "part of a
-- word" and what isn't. In particular, Aspell is smart about sometimes
-- noticing that "'" is an apostrophe and at other times that it is
-- a single quote as part of a quoted string. As a result there will
-- be cases where Markdown formatting characters interact poorly
-- with Aspell's checking to result in misspellings that are *not*
-- highlighted.
--
-- One way to deal with this would be to *not* parse the user's input
-- as done here, complete with all its Markdown metacharacters, but to
-- instead 1) parse the input as Markdown, 2) traverse the Markdown AST
-- and extract the words from the relevant subtrees, and 3) spell-check
-- those words. The reason we don't do it that way in the first place is
-- because 1) the user's input might not be valid markdown and 2) even
-- if we did that, we'd still have to do this tokenization operation to
-- annotate misspellings and reconstruct the user's raw input.
doHighlightMisspellings :: HighlightSet -> S.Set Text -> [Text] -> Widget Name
doHighlightMisspellings hSet misspellings contents =
    -- Traverse the input, gathering non-whitespace into tokens and
    -- checking if they appear in the misspelling collection
    let whitelist = S.union (hUserSet hSet) (hChannelSet hSet)

        handleLine t | t == "" = txt " "
        handleLine t =
            -- For annotated tokens, coallesce tokens of the same type
            -- and add attributes for misspellings.
            let mkW (Left tok) =
                    let s = getTokenText tok
                    in if T.null s
                       then emptyWidget
                       else withDefAttr misspellingAttr $ txt $ getTokenText tok
                mkW (Right tok) =
                    let s = getTokenText tok
                    in if T.null s
                       then emptyWidget
                       else txt s

                go :: Either Token Token -> [Either Token Token] -> [Either Token Token]
                go lst [] = [lst]
                go lst (tok:toks) =
                    case (lst, tok) of
                        (Left a, Left b)   -> go (Left $ combineTokens a b) toks
                        (Right a, Right b) -> go (Right $ combineTokens a b) toks
                        _                  -> lst : go tok toks

            in hBox $ mkW <$> (go (Right $ Ignore "") $ annotatedTokens t)

        combineTokens (Ignore a) (Ignore b) = Ignore $ a <> b
        combineTokens (Check a) (Check b) = Check $ a <> b
        combineTokens (Ignore a) (Check b) = Check $ a <> b
        combineTokens (Check a) (Ignore b) = Check $ a <> b

        getTokenText (Ignore a) = a
        getTokenText (Check a) = a

        annotatedTokens t =
            -- For every token, check on whether it is a misspelling.
            -- The result is Either Token Token where the Left is a
            -- misspelling and the Right is not.
            checkMisspelling <$> tokenize t (Ignore "")

        checkMisspelling t@(Ignore _) = Right t
        checkMisspelling t@(Check s) =
            if s `S.member` whitelist
            then Right t
            else if s `S.member` misspellings
                 then Left t
                 else Right t

        ignoreChar c = isSpace c || isPunctuation c || c == '`' || c == '/' ||
                       T.singleton c == userSigil || T.singleton c == normalChannelSigil

        tokenize t curTok
            | T.null t = [curTok]
            | ignoreChar $ T.head t =
                case curTok of
                    Ignore s -> tokenize (T.tail t) (Ignore $ s <> (T.singleton $ T.head t))
                    Check s -> Check s : tokenize (T.tail t) (Ignore $ T.singleton $ T.head t)
            | otherwise =
                case curTok of
                    Ignore s -> Ignore s : tokenize (T.tail t) (Check $ T.singleton $ T.head t)
                    Check s -> tokenize (T.tail t) (Check $ s <> (T.singleton $ T.head t))

    in vBox $ handleLine <$> contents

userInputArea :: ChatState -> SimpleGetter ChatState EditState -> TeamId -> HighlightSet -> Widget Name
userInputArea st editWhich tId hs =
    let replyPrompt = "reply> "
        normalPrompt = "> "
        editPrompt = "edit> "
        showReplyPrompt = st^.editWhich.cedShowReplyPrompt
        prompt = txt $ case st^.editWhich.cedEditMode of
            Replying {} ->
                if showReplyPrompt then replyPrompt else normalPrompt
            Editing {}  ->
                editPrompt
            NewPost ->
                normalPrompt
        editor = st^.editWhich.cedEditor
        inputBox = renderEditor (drawEditorContents st editWhich tId hs) True editor
        curContents = getEditContents editor
        multilineContent = length curContents > 1
        multilineHints =
            hBox [ borderElem bsHorizontal
                 , str $ "[" <> (show $ (+1) $ fst $ cursorPosition $
                                        editor^.editContentsL) <>
                         "/" <> (show $ length curContents) <> "]"
                 , hBorderWithLabel $ withDefAttr clientEmphAttr $
                   txt $ "In multi-line mode. Press " <> multiLineToggleKey <>
                         " to finish."
                 ]

        replyDisplay = case st^.editWhich.cedEditMode of
            Replying msg _ | showReplyPrompt ->
                let msgWithoutParent = msg & mInReplyToMsg .~ NotAReply
                in hBox [ replyArrow
                        , addEllipsis $ renderMessage MessageData
                          { mdMessage           = msgWithoutParent
                          , mdUserName          = msgWithoutParent^.mUser.to (printableNameForUserRef st)
                          , mdParentMessage     = Nothing
                          , mdParentUserName    = Nothing
                          , mdHighlightSet      = hs
                          , mdEditThreshold     = Nothing
                          , mdShowOlderEdits    = False
                          , mdRenderReplyParent = True
                          , mdRenderReplyIndent = True
                          , mdIndentBlocks      = False
                          , mdThreadState       = NoThread
                          , mdShowReactions     = True
                          , mdMessageWidthLimit = Nothing
                          , mdMyUsername        = myUsername st
                          , mdMyUserId          = myUserId st
                          , mdWrapNonhighlightedCodeBlocks = True
                          , mdTruncateVerbatimBlocks = Nothing
                          , mdClickableNameTag  = getName editor
                          }
                        ]
            _ -> emptyWidget

        kc = st^.csResources.crConfiguration.configUserKeysL
        multiLineToggleKey = ppBinding $ firstActiveBinding kc ToggleMultiLineEvent

        commandBox = case st^.editWhich.cedEphemeral.eesMultiline of
            False ->
                let linesStr = "line" <> if numLines == 1 then "" else "s"
                    numLines = length curContents
                in vLimit 1 $ hBox $
                   prompt : if multilineContent
                            then [ withDefAttr clientEmphAttr $
                                   str $ "[" <> show numLines <> " " <> linesStr <>
                                         "; Enter: send, " <> T.unpack multiLineToggleKey <>
                                         ": edit, Backspace: cancel] "
                                 , txt $ head curContents
                                 , showCursor (MessageInput tId) (Location (0,0)) $ str " "
                                 ]
                            else [inputBox]
            True -> vLimit multilineHeightLimit inputBox <=> multilineHints
    in replyDisplay <=> commandBox

renderChannelHeader :: ChatState -> TeamId -> HighlightSet -> Maybe ClientChannel -> Widget Name
renderChannelHeader _ _ _ Nothing =
    txt " "
renderChannelHeader st tId hs (Just chan) =
    let chnType = chan^.ccInfo.cdType
        topicStr = chan^.ccInfo.cdHeader
        userHeader u = let s = T.intercalate " " $ filter (not . T.null) parts
                           parts = [ chanName
                                   , if (all T.null names)
                                     then mempty
                                     else "is"
                                   ] <> names <> [
                                     if T.null (u^.uiEmail)
                                     then mempty
                                     else "(" <> u^.uiEmail <> ")"
                                   ]
                           names = [ u^.uiFirstName
                                   , nick
                                   , u^.uiLastName
                                   ]
                           quote n = "\"" <> n <> "\""
                           nick = maybe "" quote $ u^.uiNickName
                       in s
        firstTopicLine = case T.lines topicStr of
            [h] -> h
            (h:_:_) -> h
            _ -> ""
        maybeTopic = if T.null topicStr
                     then ""
                     else " - " <> if st^.csResources.crConfiguration.configShowExpandedChannelTopicsL
                                   then topicStr
                                   else firstTopicLine
        channelNameString = case chnType of
            Direct ->
                case chan^.ccInfo.cdDMUserId >>= flip userById st of
                    Nothing -> chanName
                    Just u -> userHeader u
            Private ->
                channelNamePair <> " (Private)"
            Group ->
                channelNamePair <> " (Private group)"
            _ ->
                channelNamePair
        channelNamePair = chanName <> " - " <> (chan^.ccInfo.cdDisplayName)
        chanName = mkChannelName st (chan^.ccInfo)
        baseUrl = serverBaseUrl st tId

    in renderText' (Just baseUrl) (myUsername st)
         hs (Just (mkClickableInline Nothing (ChannelTopic $ chan^.ccInfo.cdChannelId)))
         (channelNameString <> maybeTopic)

renderMessageListing :: ChatState
                     -> Bool
                     -> Bool
                     -> TeamId
                     -> HighlightSet
                     -> Traversal' ChatState Messages
                     -> Lens' ChatState MessageSelectState
                     -> Bool
                     -> Name
                     -> Widget Name
renderMessageListing st inMsgSelect showNewMsgLine tId hs getMessages selWhich renderReplyIndent region =
    messages
    where
    mcId = st^.(csCurrentChannelId tId)

    messages = padTop Max chatText

    chatText =
        case mcId of
            Nothing -> fill ' '
            Just cId ->
                if inMsgSelect
                then freezeBorders $
                     renderMessagesWithSelect cId (st^.selWhich) (buildMessages cId)
                else cached (ChannelMessages cId) $
                     freezeBorders $
                     renderLastMessages st hs (getEditedMessageCutoff cId st) renderReplyIndent region $
                     retrogradeMsgsWithThreadStates $
                     reverseMessages $
                     buildMessages cId

    renderMessagesWithSelect cId (MessageSelectState selMsgId) msgs =
        -- In this case, we want to fill the message list with messages
        -- but use the post ID as a cursor. To do this efficiently we
        -- only want to render enough messages to fill the screen.
        --
        -- If the message area is H rows high, this actually renders at
        -- most 2H rows' worth of messages and then does the appropriate
        -- cropping. This way we can simplify the math needed to figure
        -- out how to crop while bounding the number of messages we
        -- render around the cursor.
        --
        -- First, we sanity-check the application state because under
        -- some conditions, the selected message might be gone (e.g.
        -- deleted).
        let (s, (before, after)) = splitDirSeqOn (\(m, _) -> m^.mMessageId == selMsgId) msgsWithStates
            msgsWithStates = chronologicalMsgsWithThreadStates msgs
        in case s of
             Nothing ->
                 renderLastMessages st hs (getEditedMessageCutoff cId st) renderReplyIndent region before
             Just m ->
                 unsafeRenderMessageSelection (m, (before, after))
                     (renderSingleMessage st hs renderReplyIndent Nothing) region

    buildMessages cId =
        -- If the message list is empty, add an informative message to
        -- the message listing to make it explicit that this listing is
        -- empty.
        let cutoff = if showNewMsgLine
                     then getNewMessageCutoff cId st
                     else Nothing
            ms = filterMessageListing st getMessages
        in if F.null ms
           then addMessage (emptyChannelFillerMessage st cId) emptyDirSeq
           else insertTransitions ms
                                  cutoff
                                  (getDateFormat st)
                                  (st ^. timeZone)

-- | Construct a single message to be displayed in the specified channel
-- when it does not yet have any user messages posted to it.
emptyChannelFillerMessage :: ChatState -> ChannelId -> Message
emptyChannelFillerMessage st cId =
    newMessageOfType msg (C Informative) ts
    where
        -- This is a bogus timestamp, but its value does not matter
        -- because it is only used to create a message that will be
        -- shown in a channel with no date transitions (which would
        -- otherwise include this bogus date) or other messages (which
        -- would make for a broken message sorting).
        ts = ServerTime $ UTCTime (toEnum 0) 0
        chan = fromJust $ findChannelById cId (st^.csChannels)
        chanName = mkChannelName st (chan^.ccInfo)
        msg = case chan^.ccInfo.cdType of
            Direct ->
                let u = chan^.ccInfo.cdDMUserId >>= flip userById st
                in case u of
                    Nothing -> userMsg Nothing
                    Just _ -> userMsg (Just chanName)
            Group ->
                groupMsg (chan^.ccInfo.cdDisplayName)
            _ ->
                chanMsg chanName
        userMsg (Just cn) = "You have not yet sent any direct messages to " <> cn <> "."
        userMsg Nothing   = "You have not yet sent any direct messages to this user."
        groupMsg us = "There are not yet any direct messages in the group " <> us <> "."
        chanMsg cn = "There are not yet any messages in the " <> cn <> " channel."

filterMessageListing :: ChatState -> Traversal' ChatState Messages -> Messages
filterMessageListing st msgsWhich =
    st ^?! msgsWhich . to (filterMessages isShown)
    where isShown m
            | st^.csResources.crUserPreferences.userPrefShowJoinLeave = True
            | otherwise = not $ isJoinLeave m

insertTransitions :: Messages -> Maybe NewMessageIndicator -> Text -> TimeZoneSeries -> Messages
insertTransitions ms cutoff = insertDateMarkers $ foldr addMessage ms newMessagesT
    where anyNondeletedNewMessages t =
              isJust $ findLatestUserMessage (not . view mDeleted) (messagesAfter t ms)
          newMessagesT = case cutoff of
              Nothing -> []
              Just Hide -> []
              Just (NewPostsAfterServerTime t)
                  | anyNondeletedNewMessages t -> [newMessagesMsg $ justAfter t]
                  | otherwise -> []
              Just (NewPostsStartingAt t)
                  | anyNondeletedNewMessages (justBefore t) -> [newMessagesMsg $ justBefore t]
                  | otherwise -> []
          newMessagesMsg d = newMessageOfType (T.pack "New Messages")
                             (C NewMessagesTransition) d

drawChannelSelectPrompt :: ChatState -> TeamId -> Widget Name
drawChannelSelectPrompt st tId =
    Widget Greedy Greedy $ do
       ctx <- getContext
       let rowOffset = ctx^.availHeightL - 1
           e = st^.csTeam(tId).tsChannelSelectState.channelSelectInput
       render $ translateBy (Location (0, rowOffset)) $
                withDefAttr channelSelectPromptAttr $
                (txt "Switch to channel [use ^ and $ to anchor]: ") <+>
                (renderEditor (txt . T.concat) True e)

drawMain :: ChatState -> Mode -> [Widget Name]
drawMain st mode =
    [ connectionLayer st
    , maybe emptyWidget (\tId -> autocompleteLayer st (channelEditor(tId))) (st^.csCurrentTeamId)
    , joinBorders $ mainInterface st mode (st^.csCurrentTeamId)
    ]

teamList :: ChatState -> Widget Name
teamList st =
    let curTid = st^.csCurrentTeamId
        z = st^.csTeamZipper
        pos = fromJust $ Z.position z
        teams = (\tId -> st^.csTeam(tId)) <$> (concat $ snd <$> Z.toList z)
        numTeams = length teams
        entries = mkEntry <$> teams
        mkEntry ts =
            let tId = teamId $ _tsTeam ts
                unread = uCount > 0
                uCount = unreadCount tId
                tName  = ClickableTeamListEntry tId
            in (if Just tId == curTid
                   then visible . withDefAttr currentTeamAttr
                   else if unread
                        then withDefAttr unreadChannelAttr
                        else id) $
               clickable tName $ txt $
               (T.strip $ sanitizeUserText $ teamDisplayName $ _tsTeam ts)
        unreadCount tId = sum $ fmap (nonDMChannelListGroupUnread . fst) $
                          Z.toList $ st^.csTeam(tId).tsFocus
    in if numTeams == 1
       then emptyWidget
       else vBox [ hBox [ padRight (Pad 1) $ txt $ T.pack $ "Teams (" <> show (pos + 1) <> "/" <> show numTeams <> "):"
                        , vLimit 1 $ viewport TeamList Horizontal $
                          hBox $
                          intersperse (txt " ") entries
                        ]
                 , hBorder
                 ]

connectionLayer :: ChatState -> Widget Name
connectionLayer st =
    case st^.csConnectionStatus of
        Connected -> emptyWidget
        Disconnected ->
            Widget Fixed Fixed $ do
                ctx <- getContext
                let aw = ctx^.availWidthL
                    w = length msg + 2
                    msg = "NOT CONNECTED"
                render $ translateBy (Location (max 0 (aw - w), 0)) $
                         withDefAttr errorMessageAttr $
                         border $ str msg

messageSelectBottomBar :: ChatState
                       -> TeamId
                       -> Lens' ChatState MessageSelectState
                       -> Lens' ChatState EditState
                       -> Traversal' ChatState Messages
                       -> Widget Name
messageSelectBottomBar st tId selWhich editWhich msgsWhich =
    case getSelectedMessage selWhich msgsWhich st of
        Nothing -> emptyWidget
        Just postMsg ->
            let optionList = if null usableOptions
                             then txt "(no actions available for this message)"
                             else hBox $ intersperse (txt " ") usableOptions
                usableOptions = catMaybes $ mkOption <$> options
                mkOption (f, k, desc) = if f postMsg
                                        then Just $ withDefAttr messageSelectStatusAttr (txt k) <+>
                                                    txt (":" <> desc)
                                        else Nothing
                numURLs = Seq.length $ msgURLs postMsg
                s = if numURLs == 1 then "" else "s"
                hasURLs = numURLs > 0
                openUrlsMsg = "open " <> (T.pack $ show numURLs) <> " URL" <> s
                hasVerb = isJust (findVerbatimChunk (postMsg^.mText))
                ev = keyEventBindings st (messageSelectKeybindings tId selWhich msgsWhich editWhich)
                -- make sure these keybinding pieces are up-to-date!
                options = [ ( not . isGap
                            , ev YankWholeMessageEvent
                            , "yank-all"
                            )
                          , ( \m -> isFlaggable m && not (m^.mFlagged)
                            , ev FlagMessageEvent
                            , "flag"
                            )
                          , ( \m -> isFlaggable m && m^.mFlagged
                            , ev FlagMessageEvent
                            , "unflag"
                            )
                          , ( isReplyable
                            , ev OpenThreadEvent
                            , "thread"
                            )
                          , ( isPostMessage
                            , ev CopyPostLinkEvent
                            , "copy-link"
                            )
                          , ( \m -> isPinnable m && not (m^.mPinned)
                            , ev PinMessageEvent
                            , "pin"
                            )
                          , ( \m -> isPinnable m && m^.mPinned
                            , ev PinMessageEvent
                            , "unpin"
                            )
                          , ( isReplyable
                            , ev ReplyMessageEvent
                            , "reply"
                            )
                          , ( not . isGap
                            , ev ViewMessageEvent
                            , "view"
                            )
                          , ( isGap
                            , ev FillGapEvent
                            , "load messages"
                            )
                          , ( \m -> isMine st m && isEditable m
                            , ev EditMessageEvent
                            , "edit"
                            )
                          , ( \m -> isMine st m && isDeletable m
                            , ev DeleteMessageEvent
                            , "delete"
                            )
                          , ( const hasURLs
                            , ev OpenMessageURLEvent
                            , openUrlsMsg
                            )
                          , ( const hasVerb
                            , ev YankMessageEvent
                            , "yank-code"
                            )
                          , ( isReactable
                            , ev ReactToMessageEvent
                            , "react"
                            )
                          ]

            in hBox [ borderElem bsHorizontal
                    , txt "["
                    , optionList
                    , txt "]"
                    , hBorder
                    ]

maybePreviewViewport :: Name -> Widget Name -> Widget Name
maybePreviewViewport n w =
    Widget Greedy Fixed $ do
        result <- render w
        case (Vty.imageHeight $ result^.imageL) > previewMaxHeight of
            False -> return result
            True ->
                render $ vLimit previewMaxHeight $ viewport n Vertical $
                         (resultToWidget result)

inputPreview :: ChatState -> SimpleGetter ChatState EditState -> TeamId -> Name -> HighlightSet -> Widget Name
inputPreview st editWhich tId vpName hs
    | not $ st^.csResources.crConfiguration.configShowMessagePreviewL = emptyWidget
    | otherwise = thePreview
    where
    uId = myUserId st
    -- Insert a cursor sentinel into the input text just before
    -- rendering the preview. We use the inserted sentinel (which is
    -- not rendered) to get brick to ensure that the line the cursor is
    -- on is visible in the preview viewport. We put the sentinel at
    -- the *end* of the line because it will still influence markdown
    -- parsing and can create undesirable/confusing churn in the
    -- rendering while the cursor moves around. If the cursor is at the
    -- end of whatever line the user is editing, that is very unlikely
    -- to be a problem.
    curContents = getText $ (gotoEOL >>> insertChar cursorSentinel) $
                  st^.editWhich.cedEditor.editContentsL
    curStr = T.intercalate "\n" curContents
    overrideTy = case st^.editWhich.cedEditMode of
        Editing _ ty -> Just ty
        _ -> Nothing
    baseUrl = serverBaseUrl st tId
    previewMsg = previewFromInput baseUrl overrideTy uId curStr
    thePreview = let noPreview = str "(No preview)"
                     msgPreview = case previewMsg of
                       Nothing -> noPreview
                       Just pm -> if T.null curStr
                                  then noPreview
                                  else prview pm $ getParentMessage st pm
                     prview m p = renderMessage MessageData
                                  { mdMessage           = m
                                  , mdUserName          = m^.mUser.to (printableNameForUserRef st)
                                  , mdParentMessage     = p
                                  , mdParentUserName    = p >>= (^.mUser.to (printableNameForUserRef st))
                                  , mdHighlightSet      = hs
                                  , mdEditThreshold     = Nothing
                                  , mdShowOlderEdits    = False
                                  , mdRenderReplyParent = True
                                  , mdRenderReplyIndent = True
                                  , mdIndentBlocks      = True
                                  , mdThreadState       = NoThread
                                  , mdShowReactions     = True
                                  , mdMessageWidthLimit = Nothing
                                  , mdMyUsername        = myUsername st
                                  , mdMyUserId          = myUserId st
                                  , mdWrapNonhighlightedCodeBlocks = True
                                  , mdTruncateVerbatimBlocks = Nothing
                                  , mdClickableNameTag  = MessagePreviewViewport tId
                                  }
                 in (maybePreviewViewport vpName msgPreview) <=>
                    hBorderWithLabel (withDefAttr clientEmphAttr $ str "[Preview ↑]")

drawDeleteMessageConfirm :: Widget Name
drawDeleteMessageConfirm =
    let msg = "Are you sure you want to delete the selected message? (y/n)"
    in centerLayer $
       borderWithLabel (withAttr channelListHeaderAttr $ txt "Confirm") $
       hLimit (T.length msg + 4) $
       vLimit 3 $
       center $
       withDefAttr errorMessageAttr $
       txt msg

mainInterface :: ChatState -> Mode -> Maybe TeamId -> Widget Name
mainInterface st mode mtId =
    vBox [ teamList st
         , body
         ]
    where
    showChannelList =
        st^.csResources.crConfiguration.configShowChannelListL ||
        case mtId of
            Nothing -> True
            Just {} -> mode == ChannelSelect
    body = if showChannelList
           then case st^.csChannelListOrientation of
               ChannelListLeft ->
                   hBox [channelList, vBorder, mainDisplay]
               ChannelListRight ->
                   hBox [mainDisplay, vBorder, channelList]
           else mainDisplay
    channelList = channelListMaybeVlimit mode $
                  hLimit channelListWidth $ case mtId of
                      Nothing -> fill ' '
                      Just tId -> renderChannelList st tId
    channelListWidth = configChannelListWidth $ st^.csResources.crConfiguration
    channelListMaybeVlimit ChannelSelect w =
        Widget (hSize w) (vSize w) $ do
            ctx <- getContext
            render $ vLimit (ctx^.availHeightL - 1) w
    channelListMaybeVlimit _ w = w

    mainDisplay =
        case mtId of
            Nothing ->
                vBox [ fill ' '
                     , hBorder
                     , vLimit 1 $ fill ' '
                     ]
            Just tId ->
                let
                    hs = getHighlightSet st tId

                    channelHeader mChan =
                        withDefAttr channelHeaderAttr $
                        padRight Max $
                        renderChannelHeader st tId hs mChan

                    inMsgSelect = case mode of
                        ChannelMessageSelect {} -> True
                        _ -> False
                    maybeSubdue = if mode == ChannelSelect
                                  then forceAttr ""
                                  else id
                in case st^.csCurrentChannelId(tId) of
                    Nothing -> fill ' '
                    Just cId ->
                        let ch = st^?csChannel(cId)
                        in vBox [ channelHeader ch
                                , hBorder
                                , maybeSubdue $
                                  drawMessageInterface st hs
                                                       (ChannelMessages cId)
                                                       tId inMsgSelect
                                                       True
                                                       (channelMessageSelect(tId))
                                                       (channelEditor(tId))
                                                       (csChannelMessages(cId))
                                                       True
                                                       (MessagePreviewViewport tId)
                                ]

drawMessageInterface :: ChatState
                     -> HighlightSet
                     -> Name
                     -> TeamId
                     -> Bool
                     -> Bool
                     -> Lens' ChatState MessageSelectState
                     -> Lens' ChatState EditState
                     -> Traversal' ChatState Messages
                     -> Bool
                     -> Name
                     -> Widget Name
drawMessageInterface st hs region tId inMsgSelect showNewMsgLine selWhich editWhich msgsWhich renderReplyIndent previewVpName =
    vBox [ interfaceContents
         , bottomBorder
         , inputPreview st editWhich tId previewVpName hs
         , userInputArea st editWhich tId hs
         ]
    where
    interfaceContents =
        renderMessageListing st inMsgSelect showNewMsgLine tId hs msgsWhich selWhich renderReplyIndent region

    bottomBorder =
        if inMsgSelect
        then messageSelectBottomBar st tId selWhich editWhich msgsWhich
        else hBox [ showAttachmentCount
                  , hBorder
                  , showTypingUsers
                  , showBusy
                  ]

    showBusy = case st^.csWorkerIsBusy of
                 Just (Just n) -> hLimit 2 hBorder <+> txt (T.pack $ "*" <> show n)
                 Just Nothing -> hLimit 2 hBorder <+> txt "*"
                 Nothing -> emptyWidget

    showTypingUsers =
        case st^.csCurrentChannelId(tId) of
            Nothing -> emptyWidget
            Just cId ->
                case st^?csChannel(cId) of
                    Nothing -> emptyWidget
                    Just chan ->
                        let format = renderText' Nothing (myUsername st) hs Nothing
                        in case allTypingUsers (chan^.ccEphemeralEditState.eesTypingUsers) of
                            [] -> emptyWidget
                            [uId] | Just un <- usernameForUserId uId st ->
                               format $ "[" <> addUserSigil un <> " is typing]"
                            [uId1, uId2] | Just un1 <- usernameForUserId uId1 st
                                         , Just un2 <- usernameForUserId uId2 st ->
                               format $ "[" <> addUserSigil un1 <> " and " <> addUserSigil un2 <> " are typing]"
                            _ -> format "[several people are typing]"

    kc = st^.csResources.crConfiguration.configUserKeysL
    showAttachmentCount =
        let count = length $ listElements $ st^.editWhich.cedAttachmentList
        in if count == 0
           then emptyWidget
           else hBox [ borderElem bsHorizontal
                     , withDefAttr clientMessageAttr $
                       txt $ "(" <> (T.pack $ show count) <> " attachment" <>
                             (if count == 1 then "" else "s") <> "; "
                     , withDefAttr clientEmphAttr $
                       txt $ ppBinding (firstActiveBinding kc ShowAttachmentListEvent)
                     , txt " to manage)"
                     ]

replyArrow :: Widget a
replyArrow =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let bs = ctx^.ctxBorderStyleL
        render $ str [' ', bsCornerTL bs, '▸']
