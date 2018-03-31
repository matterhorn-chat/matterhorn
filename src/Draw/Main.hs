{-# LANGUAGE MultiWayIf #-}
module Draw.Main (drawMain) where

import           Prelude ()
import           Prelude.Compat

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center (hCenter)
import           Brick.Widgets.Edit (editContentsL, renderEditor, getEditContents)
import           Brick.Widgets.List (renderList)
import           Control.Arrow ((>>>))
import           Control.Monad (foldM)
import           Control.Monad.Trans.Reader (withReaderT)
import           Data.Time.Clock (UTCTime(..))
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Foldable as F
import           Data.List (intersperse)
import           Data.Maybe (catMaybes, isJust)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Zipper (cursorPosition, insertChar, getText, gotoEOL)
import           Data.Char (isSpace, isPunctuation)
import           Lens.Micro.Platform

import           Network.Mattermost.Types (ChannelId, Type(Direct), ServerTime(..), UserId)

import qualified Graphics.Vty as Vty

import           Draw.ChannelList (renderChannelList)
import           Draw.Messages
import           Draw.Util
import           Markdown
import           Completion (Completer(..), currentAlternative)
import qualified Zipper as Z
import           State
import           Themes
import           TimeUtils (justAfter, justBefore)
import           Types
import           Types.Channels ( NewMessageIndicator(..)
                                , ClientChannel
                                , ccInfo, ccContents
                                , cdTypingUsers
                                , cdName, cdType, cdHeader, cdMessages
                                , findChannelById)
import           Types.Messages
import           Types.Posts
import           Types.Users
import           Types.KeyEvents
import           Events.Keybindings
import           Events.MessageSelect

previewFromInput :: UserId -> T.Text -> Maybe Message
previewFromInput _ s | s == T.singleton cursorSentinel = Nothing
previewFromInput uId s =
    -- If it starts with a slash but not /me, this has no preview
    -- representation
    let isCommand = "/" `T.isPrefixOf` s
        isEmote = "/me " `T.isPrefixOf` s
        content = if isEmote
                  then T.stripStart $ T.drop 3 s
                  else s
        msgTy = if isEmote then CP Emote else CP NormalPost
    in if isCommand && not isEmote
       then Nothing
       else Just $ Message { _mText          = getBlocks content
                           , _mUser          = UserI uId
                           , _mDate          = ServerTime $ UTCTime (fromGregorian 1970 1 1) 0
                           -- The date is not used for preview
                           -- rendering, but we need to provide one.
                           -- Ideally we'd just today's date, but the
                           -- rendering function is pure so we can't.
                           , _mType          = msgTy
                           , _mPending       = False
                           , _mDeleted       = False
                           , _mAttachments   = mempty
                           , _mInReplyToMsg  = NotAReply
                           , _mPostId        = Nothing
                           , _mReactions     = mempty
                           , _mOriginalPost  = Nothing
                           , _mFlagged       = False
                           , _mChannelId     = Nothing
                           }

-- | Tokens in spell check highlighting.
data Token =
    Ignore T.Text
    -- ^ This bit of text is to be ignored for the purposes of
    -- spell-checking.
    | Check T.Text
    -- ^ This bit of text should be checked against the spell checker's
    -- misspelling list.
    deriving (Show)

drawEditorContents :: ChatState -> HighlightSet -> [T.Text] -> Widget Name
drawEditorContents st hs =
    let noHighlight = txt . T.unlines
    in case st^.csEditState.cedSpellChecker of
        Nothing -> noHighlight
        Just _ ->
            case S.null (st^.csEditState.cedMisspellings) of
                True -> noHighlight
                False -> doHighlightMisspellings
                           hs
                           (st^.csEditState.cedMisspellings)

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
doHighlightMisspellings :: HighlightSet -> S.Set T.Text -> [T.Text] -> Widget Name
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

        ignoreChar c = isSpace c || isPunctuation c || c == '`'

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

renderUserCommandBox :: ChatState -> HighlightSet -> Widget Name
renderUserCommandBox st hs =
    let prompt = txt $ case st^.csEditState.cedEditMode of
            Replying _ _ -> "reply> "
            Editing _    ->  "edit> "
            NewPost      ->      "> "
        inputBox = renderEditor (drawEditorContents st hs) True (st^.csEditState.cedEditor)
        curContents = getEditContents $ st^.csEditState.cedEditor
        multilineContent = length curContents > 1
        multilineHints =
            hBox [ borderElem bsHorizontal
                 , str $ "[" <> (show $ (+1) $ fst $ cursorPosition $
                                        st^.csEditState.cedEditor.editContentsL) <>
                         "/" <> (show $ length curContents) <> "]"
                 , hBorderWithLabel $ withDefAttr clientEmphAttr $
                   txt $ "In multi-line mode. Press " <>
                         (ppBinding (getFirstDefaultBinding ToggleMultiLineEvent)) <>
                         " to finish."
                 ]

        replyDisplay = case st^.csEditState.cedEditMode of
            Replying msg _ ->
                let msgWithoutParent = msg & mInReplyToMsg .~ NotAReply
                in hBox [ replyArrow
                        , addEllipsis $ renderMessage MessageData
                          { mdMessage           = msgWithoutParent
                          , mdUserName          = msgWithoutParent^.mUser.to (nameForUserRef st)
                          , mdParentMessage     = Nothing
                          , mdParentUserName    = Nothing
                          , mdHighlightSet      = hs
                          , mdEditThreshold     = Nothing
                          , mdShowOlderEdits    = False
                          , mdRenderReplyParent = True
                          , mdIndentBlocks      = False
                          }
                        ]
            _ -> emptyWidget

        commandBox = case st^.csEditState.cedMultiline of
            False ->
                let linesStr = "line" <> if numLines == 1 then "" else "s"
                    numLines = length curContents
                in vLimit 1 $ hBox $
                   prompt : if multilineContent
                            then [ withDefAttr clientEmphAttr $
                                   str $ "[" <> show numLines <> " " <> linesStr <>
                                         "; Enter: send, M-e: edit, Backspace: cancel] "
                                 , txt $ head curContents
                                 , showCursor MessageInput (Location (0,0)) $ str " "
                                 ]
                            else [inputBox]
            True -> vLimit 5 inputBox <=> multilineHints
    in replyDisplay <=> commandBox

renderChannelHeader :: ChatState -> HighlightSet -> ClientChannel -> Widget Name
renderChannelHeader st hs chan =
    let chnType = chan^.ccInfo.cdType
        chnName = chan^.ccInfo.cdName
        topicStr = chan^.ccInfo.cdHeader
        userHeader u = let s = T.intercalate " " $ filter (not . T.null) parts
                           parts = [ u^.uiName
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
        foundUser = userByDMChannelName chnName (myUserId st) st
        maybeTopic = if T.null topicStr
                     then ""
                     else " - " <> topicStr
        channelNameString = case chnType of
            Direct ->
                case foundUser of
                    Nothing -> mkChannelName (chan^.ccInfo)
                    Just u  -> userHeader u
            _ -> mkChannelName (chan^.ccInfo)
        newlineToSpace '\n' = ' '
        newlineToSpace c = c

    in renderText'
         hs
         (T.map newlineToSpace (channelNameString <> maybeTopic))

renderCurrentChannelDisplay :: ChatState -> HighlightSet -> Widget Name
renderCurrentChannelDisplay st hs = (header <+> conn) <=> messages
    where
    conn = case st^.csConnectionStatus of
      Connected -> emptyWidget
      Disconnected  -> withDefAttr errorMessageAttr (str "[NOT CONNECTED]")
    header = withDefAttr channelHeaderAttr $
             padRight Max $
             renderChannelHeader st hs chan
    messages = padTop Max $ padRight (Pad 1) body

    body = chatText

    chatText = case appMode st of
        ChannelScroll ->
            -- n.b., In this mode, the output is cached and scrolled
            -- via the viewport.  This means that newly received
            -- messages are *not* displayed, but this preserves the
            -- stability of the scrolling, which provides a better
            -- user experience.
            viewport (ChannelMessages cId) Vertical $
            cached (ChannelMessages cId) $
            vBox $ (withDefAttr loadMoreAttr $ hCenter $
                    str "<< Press C-b to load more messages >>") :
                   (F.toList $ renderSingleMessage st hs editCutoff <$> channelMessages)
        MessageSelect ->
            renderMessagesWithSelect (st^.csMessageSelect) channelMessages
        MessageSelectDeleteConfirm ->
            renderMessagesWithSelect (st^.csMessageSelect) channelMessages
        _ -> renderLastMessages $ reverseMessages channelMessages

    renderMessagesWithSelect (MessageSelectState selPostId) msgs =
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
        let (s, (before, after)) = splitMessages selPostId msgs
        in case s of
             Nothing -> renderLastMessages before
             Just m ->
               unsafeRenderMessageSelection (m, (before, after)) (renderSingleMessage st hs Nothing)

    cutoff = getNewMessageCutoff cId st
    editCutoff = getEditedMessageCutoff cId st
    channelMessages =
        insertTransitions (getMessageListing cId st)
                          cutoff
                          (getDateFormat st)
                          (st ^. timeZone)


    renderLastMessages :: RetrogradeMessages -> Widget Name
    renderLastMessages msgs =
        Widget Greedy Greedy $ do
            ctx <- getContext
            let targetHeight = ctx^.availHeightL
                renderBuild = render1HLimit (flip Vty.vertJoin) targetHeight
            img <- foldM renderBuild Vty.emptyImage msgs
            return $ emptyResult & imageL .~ (Vty.cropTop targetHeight img)

    relaxHeight c = c & availHeightL .~ (max maxMessageHeight (c^.availHeightL))

    render1HLimit fjoin lim img msg
      | Vty.imageHeight img >= lim = return img
      | otherwise = fjoin img <$> render1 msg

    render1 msg = case msg^.mDeleted of
                    True -> return Vty.emptyImage
                    False -> do
                      r <- withReaderT relaxHeight $
                           render $ padRight Max $
                                  renderSingleMessage st hs editCutoff msg
                      return $ r^.imageL

    cId = st^.csCurrentChannelId
    chan = st^.csCurrentChannel


getMessageListing :: ChannelId -> ChatState -> Messages
getMessageListing cId st =
    st ^?! csChannels.folding (findChannelById cId) . ccContents . cdMessages . to (filterMessages isShown)
    where isShown m
            | st^.csResources.crUserPreferences.userPrefShowJoinLeave = True
            | otherwise = m^.mType /= CP Join && m^.mType /= CP Leave

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

renderChannelSelect :: ChatState -> Widget Name
renderChannelSelect st =
    let cstr = st^.csChannelSelectState.channelSelectInput
    in withDefAttr channelSelectPromptAttr $
       (txt "Switch to channel [use ^ and $ to anchor]: ") <+>
        (showCursor ChannelSelectString (Location (T.length cstr, 0)) $
         txt $
         (if T.null cstr
          then " "
          else cstr))

drawMain :: ChatState -> [Widget Name]
drawMain st = [joinBorders $ mainInterface st]

messageSelectBottomBar :: ChatState -> Widget Name
messageSelectBottomBar st =
    let optionStr = if null usableOptions
                    then "(no actions available for this message)"
                    else T.intercalate " " usableOptions
        usableOptions = catMaybes $ mkOption <$> options
        mkOption (f, k, desc) = if f postMsg
                                then Just $ k <> ":" <> desc
                                else Nothing
        numURLs = Seq.length $ msgURLs postMsg
        s = if numURLs == 1 then "" else "s"
        hasURLs = numURLs > 0
        openUrlsMsg = "open " <> (T.pack $ show numURLs) <> " URL" <> s
        hasVerb = isJust (findVerbatimChunk (postMsg^.mText))
        -- make sure these keybinding pieces are up-to-date!
        ev e =
          let keyconf = st^.csResources.crConfiguration.to configUserKeys
              keymap = messageSelectKeybindings keyconf
          in T.intercalate ","
               [ ppBinding (eventToBinding b)
               | KB { kbBindingInfo = Just e'
                    , kbEvent       = b
                    } <- keymap
               , e' == e ]
        options = [ ( isReplyable
                    , ev ReplyMessageEvent
                    , "reply" )
                  , ( \m -> isMine st m && isEditable m
                    , ev EditMessageEvent
                    , "edit" )
                  , ( \m -> isMine st m && isDeletable m
                    , ev DeleteMessageEvent
                    , "delete" )
                  , ( const hasURLs
                    , ev OpenMessageURLEvent
                    , openUrlsMsg )
                  , ( const hasVerb
                    , ev YankMessageEvent
                    , "yank" )
                  , ( \m -> not (m^.mFlagged)
                    , ev FlagMessageEvent
                    , "flag" )
                  , ( \m ->      m^.mFlagged
                    , ev FlagMessageEvent
                    , "unflag" )
                  ]
        Just postMsg = getSelectedMessage st

    in hBox [ borderElem bsHorizontal
            , txt "["
            , withDefAttr messageSelectStatusAttr $
              txt $ "Message select: " <> optionStr
            , txt "]"
            , hBorder
            ]

drawCompletionAlternatives :: Completer -> Widget Name
drawCompletionAlternatives c =
    let alternatives = intersperse (txt " ") $ mkAlternative <$> Z.toList (completionAlternatives c)
        mkAlternative (displayVal, _) =
            let format = if displayVal == (fst $ currentAlternative c)
                         then visible . withDefAttr completionAlternativeCurrentAttr
                         else id
            in format $ txt displayVal
    in hBox [ borderElem bsHorizontal
            , txt "["
            , withDefAttr completionAlternativeListAttr $
              vLimit 1 $ viewport CompletionAlternatives Horizontal $ hBox alternatives
            , txt "]"
            , borderElem bsHorizontal
            ]

previewMaxHeight :: Int
previewMaxHeight = 5

maybePreviewViewport :: Widget Name -> Widget Name
maybePreviewViewport w =
    Widget Greedy Fixed $ do
        result <- render w
        case (Vty.imageHeight $ result^.imageL) > previewMaxHeight of
            False -> return result
            True ->
                render $ vLimit previewMaxHeight $ viewport MessagePreviewViewport Vertical $
                         (Widget Fixed Fixed $ return result)

inputPreview :: ChatState -> HighlightSet -> Widget Name
inputPreview st hs | not $ st^.csShowMessagePreview = emptyWidget
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
                  st^.csEditState.cedEditor.editContentsL
    curStr = T.intercalate "\n" curContents
    previewMsg = previewFromInput uId curStr
    thePreview = let noPreview = str "(No preview)"
                     msgPreview = case previewMsg of
                       Nothing -> noPreview
                       Just pm -> if T.null curStr
                                  then noPreview
                                  else prview pm $ getParentMessage st pm
                     prview m p = renderMessage MessageData
                                  { mdMessage           = m
                                  , mdUserName          = m^.mUser.to (nameForUserRef st)
                                  , mdParentMessage     = p
                                  , mdParentUserName    = p >>= (^.mUser.to (nameForUserRef st))
                                  , mdHighlightSet      = hs
                                  , mdEditThreshold     = Nothing
                                  , mdShowOlderEdits    = False
                                  , mdRenderReplyParent = True
                                  , mdIndentBlocks      = True
                                  }
                 in (maybePreviewViewport msgPreview) <=>
                    hBorderWithLabel (withDefAttr clientEmphAttr $ str "[Preview ↑]")

userInputArea :: ChatState -> HighlightSet -> Widget Name
userInputArea st hs =
    case appMode st of
        ChannelSelect -> renderChannelSelect st
        UrlSelect     -> hCenter $ hBox [ txt "Press "
                                        , withDefAttr clientEmphAttr $ txt "Enter"
                                        , txt " to open the selected URL or "
                                        , withDefAttr clientEmphAttr $ txt "Escape"
                                        , txt " to cancel."
                                        ]
        ChannelScroll -> hCenter $ hBox [ txt "Press "
                                        , withDefAttr clientEmphAttr $ txt "Escape"
                                        , txt " to stop scrolling and resume chatting."
                                        ]
        MessageSelectDeleteConfirm -> renderDeleteConfirm
        _             -> renderUserCommandBox st hs

renderDeleteConfirm :: Widget Name
renderDeleteConfirm =
    hCenter $ txt "Are you sure you want to delete the selected message? (y/n)"

mainInterface :: ChatState -> Widget Name
mainInterface st =
    vBox [ hBox [hLimit channelListWidth (renderChannelList st), vBorder, mainDisplay]
         , bottomBorder
         , inputPreview st hs
         , userInputArea st hs
         ]
    where
    hs = getHighlightSet st
    channelListWidth = configChannelListWidth $ st^.csResources.crConfiguration
    mainDisplay = case appMode st of
        UrlSelect -> renderUrlList st
        _         -> maybeSubdue $ renderCurrentChannelDisplay st hs

    bottomBorder = case appMode st of
        MessageSelect -> messageSelectBottomBar st
        _ -> case st^.csEditState.cedCompleter of
            Just c -> drawCompletionAlternatives c
            _ -> maybeSubdue $ hBox
                 [ hBorder
                 , showTypingUsers
                 , showBusy
                 ]

    showTypingUsers = case allTypingUsers (st^.csCurrentChannel.ccInfo.cdTypingUsers) of
                        [] -> emptyWidget
                        [uId] | Just un <- usernameForUserId uId st ->
                           txt $ un <> " is typing"
                        [uId1, uId2] | Just un1 <- usernameForUserId uId1 st
                                     , Just un2 <- usernameForUserId uId2 st ->
                           txt $ un1 <> " and " <> un2 <> " are typing"
                        _ -> txt "several people are typing"

    showBusy = case st^.csWorkerIsBusy of
                 Just (Just n) -> hLimit 2 hBorder <+> txt (T.pack $ "*" <> show n)
                 Just Nothing -> hLimit 2 hBorder <+> txt "*"
                 Nothing -> emptyWidget

    maybeSubdue = if appMode st == ChannelSelect
                  then forceAttr ""
                  else id

renderUrlList :: ChatState -> Widget Name
renderUrlList st =
    header <=> urlDisplay
    where
        header = withDefAttr channelHeaderAttr $ vLimit 1 $
                 (txt $ "URLs: " <> (st^.csCurrentChannel.ccInfo.cdName)) <+>
                 fill ' '

        urlDisplay = if F.length urls == 0
                     then str "No URLs found in this channel."
                     else renderList renderItem True urls

        urls = st^.csUrlList

        renderItem sel link =
          let time = link^.linkTime
          in attr sel $ vLimit 2 $
            (vLimit 1 $
             hBox [ let u = maybe "" id (link^.linkUser.to (nameForUserRef st)) in colorUsername u u
                  , if link^.linkName == link^.linkURL
                      then emptyWidget
                      else (txt ": " <+> (renderText $ link^.linkName))
                  , fill ' '
                  , renderDate st $ withServerTime time
                  , str " "
                  , renderTime st $ withServerTime time
                  ] ) <=>
            (vLimit 1 (renderText $ link^.linkURL))

        attr True = forceAttr "urlListSelectedAttr"
        attr False = id
