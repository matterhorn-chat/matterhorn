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
import           Data.Time.Format ( formatTime
                                  , defaultTimeLocale )
import           Data.Time.LocalTime ( TimeZone, utcToLocalTime
                                     , localTimeToUTC, localDay
                                     , LocalTime(..), midnight )
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Foldable as F
import           Data.List (intersperse)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, isJust)
import           Data.Monoid ((<>))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Zipper (cursorPosition, insertChar, getText, gotoEOL)
import           Data.Char (isSpace, isPunctuation)
import           Lens.Micro.Platform

import           Network.Mattermost
import           Network.Mattermost.Lenses

import qualified Graphics.Vty as Vty

import           Markdown
import           State
import           Themes
import           Types
import           Types.Channels ( ChannelState(..)
                                , ccInfo, ccContents
                                , cdCurrentState
                                , cdName, cdType, cdHeader, cdMessages
                                , findChannelById)
import           Types.Posts
import           Types.Messages
import           Types.Users
import           Draw.ChannelList (renderChannelList)
import           Draw.Util

renderChatMessage :: UserSet -> ChannelSet -> (UTCTime -> Widget Name) -> Message -> Widget Name
renderChatMessage uSet cSet renderTimeFunc msg =
    let m = renderMessage msg True uSet cSet
        msgAtch = if Seq.null (msg^.mAttachments)
          then Nothing
          else Just $ withDefAttr clientMessageAttr $ vBox
                 [ txt ("  [attached: `" <> a^.attachmentName <> "`]")
                 | a <- F.toList (msg^.mAttachments)
                 ]
        msgReac = if Map.null (msg^.mReactions)
          then Nothing
          else let renderR e 1 = " [" <> e <> "]"
                   renderR e n
                     | n > 1     = " [" <> e <> " " <> T.pack (show n) <> "]"
                     | otherwise = ""
                   reacMsg = Map.foldMapWithKey renderR (msg^.mReactions)
               in Just $ withDefAttr emojiAttr $ txt ("   " <> reacMsg)
        msgTxt =
          case msg^.mUserName of
            Just _
              | msg^.mType == CP Join || msg^.mType == CP Leave ->
                  withDefAttr clientMessageAttr m
              | otherwise -> m
            Nothing ->
                case msg^.mType of
                    C DateTransition -> withDefAttr dateTransitionAttr (hBorderWithLabel m)
                    C NewMessagesTransition -> withDefAttr newMessageTransitionAttr (hBorderWithLabel m)
                    C Error -> withDefAttr errorMessageAttr m
                    _ -> withDefAttr clientMessageAttr m
        fullMsg = hBox $ msgTxt : catMaybes [msgAtch, msgReac]
        maybeRenderTime w = hBox [renderTimeFunc (msg^.mDate), txt " ", w]
        maybeRenderTimeWith f = case msg^.mType of
            C DateTransition -> id
            C NewMessagesTransition -> id
            _ -> f
    in maybeRenderTimeWith maybeRenderTime fullMsg

channelListWidth :: Int
channelListWidth = 20

previewFromInput :: T.Text -> T.Text -> Maybe Message
previewFromInput _ s | s == T.singleton cursorSentinel = Nothing
previewFromInput uname s =
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
                           , _mUserName      = Just uname
                           , _mDate          = UTCTime (fromGregorian 1970 1 1) 0
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

drawEditorContents :: UserSet -> ChannelSet -> ChatState -> [T.Text] -> Widget Name
drawEditorContents uSet cSet st =
    let noHighlight = txt . T.unlines
    in case st^.csEditState.cedSpellChecker of
        Nothing -> noHighlight
        Just _ ->
            case S.null (st^.csEditState.cedMisspellings) of
                True -> noHighlight
                False -> doHighlightMisspellings uSet cSet (st^.csEditState.cedMisspellings)

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
doHighlightMisspellings :: UserSet -> ChannelSet -> S.Set T.Text -> [T.Text] -> Widget Name
doHighlightMisspellings uSet cSet misspellings contents =
    -- Traverse the input, gathering non-whitespace into tokens and
    -- checking if they appear in the misspelling collection
    let whitelist = S.union uSet cSet

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

renderUserCommandBox :: UserSet -> ChannelSet -> ChatState -> Widget Name
renderUserCommandBox uSet cSet st =
    let prompt = txt $ case st^.csEditState.cedEditMode of
            Replying _ _ -> "reply> "
            Editing _    ->  "edit> "
            NewPost      ->      "> "
        inputBox = renderEditor (drawEditorContents uSet cSet st) True (st^.csCmdLine)
        curContents = getEditContents $ st^.csCmdLine
        multilineContent = length curContents > 1
        multilineHints =
            hBox [ borderElem bsHorizontal
                 , str $ "[" <> (show $ (+1) $ fst $ cursorPosition $
                                        st^.csCmdLine.editContentsL) <>
                         "/" <> (show $ length curContents) <> "]"
                 , hBorderWithLabel $ withDefAttr clientEmphAttr $
                   str "In multi-line mode. Press M-e to finish."
                 ]

        replyDisplay = case st^.csEditState.cedEditMode of
            Replying msg _ ->
                let msgWithoutParent = msg & mInReplyToMsg .~ NotAReply
                in hBox [ replyArrow
                        , addEllipsis $ renderMessage msgWithoutParent True uSet cSet
                        ]
            _ -> emptyWidget

        commandBox = case st^.csEditState.cedMultiline of
            False ->
                let linesStr = if numLines == 1
                               then "line"
                               else "lines"
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

maxMessageHeight :: Int
maxMessageHeight = 200

renderSingleMessage :: ChatState -> UserSet -> ChannelSet -> Message -> Widget Name
renderSingleMessage st uSet cSet = renderChatMessage uSet cSet (withBrackets . renderTime st)

renderCurrentChannelDisplay :: UserSet -> ChannelSet -> ChatState -> Widget Name
renderCurrentChannelDisplay uSet cSet st = (header <+> conn) <=> messages
    where
    conn = case st^.csConnectionStatus of
      Connected -> emptyWidget
      Disconnected -> withDefAttr errorMessageAttr (str "[NOT CONNECTED]")
    header = withDefAttr channelHeaderAttr $
             padRight Max $
             case T.null topicStr of
                 True -> case chnType of
                   Direct ->
                     case findUserByDMChannelName (st^.csUsers)
                                                  chnName
                                                  (st^.csMe^.userIdL) of
                       Nothing -> txt $ mkChannelName (chan^.ccInfo)
                       Just u  -> userHeader u
                   _        -> txt $ mkChannelName (chan^.ccInfo)
                 False -> renderText $
                          mkChannelName (chan^.ccInfo) <> " - " <> topicStr
    userHeader u = let p1 = (colorUsername $ mkDMChannelName u)
                       p2 = txt $ T.intercalate " " $ filter (not . T.null) parts
                       parts = [ " is"
                               , u^.uiFirstName
                               , nick
                               , u^.uiLastName
                               , "<" <> u^.uiEmail <> ">"
                               ]
                       quote n = "\"" <> n <> "\""
                       nick = maybe "" quote $ u^.uiNickName
                   in p1 <+> p2
    messages = padTop Max $ padRight (Pad 1) body

    body = chatText
      <=> case chan^.ccInfo.cdCurrentState.to stateMessage of
            Nothing -> emptyWidget
            Just msg -> withDefAttr clientMessageAttr $ txt msg

    chatText = case st^.csMode of
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
                   (F.toList $ renderSingleMessage st uSet cSet <$> channelMessages)
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
             Just m -> unsafeMessageSelectList before after m

    unsafeMessageSelectList before after curMsg = Widget Greedy Greedy $ do
        ctx <- getContext

        -- Render the message associated with the current post ID.
        curMsgResult <- withReaderT relaxHeight $ render $
            forceAttr messageSelectAttr $
            padRight Max $ renderSingleMessage st uSet cSet curMsg

        let targetHeight = ctx^.availHeightL
            upperHeight = targetHeight `div` 2
            lowerHeight = targetHeight - upperHeight

            lowerRender = render1HLimit Vty.vertJoin targetHeight
            upperRender = render1HLimit (flip Vty.vertJoin) targetHeight

        lowerHalf <- foldM lowerRender Vty.emptyImage after
        upperHalf <- foldM upperRender Vty.emptyImage before

        let curHeight = Vty.imageHeight $ curMsgResult^.imageL
            uncropped = upperHalf Vty.<-> curMsgResult^.imageL Vty.<-> lowerHalf
            img = if Vty.imageHeight lowerHalf < (lowerHeight - curHeight)
                  then Vty.cropTop targetHeight uncropped
                  else if Vty.imageHeight upperHalf < upperHeight
                       then Vty.cropBottom targetHeight uncropped
                       else Vty.cropTop upperHeight upperHalf Vty.<->
                            curMsgResult^.imageL Vty.<->
                            (if curHeight < lowerHeight
                             then Vty.cropBottom (lowerHeight - curHeight) lowerHalf
                             else Vty.cropBottom lowerHeight lowerHalf)

        return $ emptyResult & imageL .~ img

    channelMessages =
        insertTransitions (getDateFormat st)
                          (st ^. timeZone)
                          (getNewMessageCutoff cId st)
                          (getMessageListing cId st)

    renderLastMessages :: RetrogradeMessages -> Widget Name
    renderLastMessages msgs =
        Widget Greedy Greedy $ do
            ctx <- getContext
            let targetHeight = ctx^.availHeightL
                renderBuild = render1HLimit (flip Vty.vertJoin) targetHeight
            img <- foldM renderBuild Vty.emptyImage msgs
            return $ emptyResult & imageL .~ (Vty.cropTop targetHeight img)

    relaxHeight c = c & availHeightL .~ (max maxMessageHeight (c^.availHeightL))

    render1HLimit fjoin lim img msg = if Vty.imageHeight img >= lim
                                      then return img
                                      else fjoin img <$> render1 msg

    render1 :: Message -> RenderM Name Vty.Image
    render1 msg = case msg^.mDeleted of
                    True -> return Vty.emptyImage
                    False -> do
                      r <- withReaderT relaxHeight $
                           render $ padRight Max $
                                  renderSingleMessage st uSet cSet msg
                      return $ r^.imageL

    cId = st^.csCurrentChannelId
    chan = st^.csCurrentChannel
    chnName = chan^.ccInfo.cdName
    chnType = chan^.ccInfo.cdType
    topicStr = chan^.ccInfo.cdHeader


-- | When displaying channel contents, it may be convenient to display
-- information about the current state of the channel.
stateMessage :: ChannelState -> Maybe T.Text
stateMessage ChanGettingInfo   = Just "[Fetching channel information...]"
stateMessage ChanUnloaded      = Just "[Channel content pending...]"
stateMessage ChanGettingPosts  = Just "[Fetching channel content...]"
stateMessage ChanInitialSelect = Just "[channel initial content pending...]"
stateMessage ChanLoaded        = Nothing

getMessageListing :: ChannelId -> ChatState -> Messages
getMessageListing cId st =
    st ^?! csChannels.folding (findChannelById cId) . ccContents . cdMessages

insertTransitions :: Text -> TimeZone -> Maybe UTCTime -> Messages -> Messages
insertTransitions datefmt tz cutoff ms = foldr addMessage ms transitions
    where transitions = newMessagesT <> dateT
          newMessagesT = case cutoff of
                           Nothing -> []
                           Just t -> [newMessagesMsg $ justAfter t
                                     | isJust $ findLatestUserMessage
                                                (not . view mDeleted)
                                                (messagesAfter t ms) ]
          dateT = fmap dateMsg dateRange
          dateRange = let dr = foldr checkDateChange [] ms
                      in if length dr > 1 then tail dr else []
          checkDateChange m [] | m^.mDeleted = []
                               | otherwise = [dayStart $ m^.mDate]
          checkDateChange m dl = if dayOf (head dl) == dayOf (m^.mDate) || m^.mDeleted
                                 then dl
                                 else dayStart (m^.mDate) : dl
          dayOf = localDay . utcToLocalTime tz
          dayStart dt = localTimeToUTC tz $ LocalTime (dayOf dt) $ midnight
          justAfter (UTCTime d t) = UTCTime d $ succ t
          dateMsg d = Message (getBlocks (T.pack $ formatTime defaultTimeLocale
                                          (T.unpack datefmt)
                                          (utcToLocalTime tz d)))
                      Nothing d (C DateTransition) False False
                      Seq.empty NotAReply Nothing mempty Nothing
          newMessagesMsg d = Message (getBlocks (T.pack "New Messages"))
                             Nothing d (C NewMessagesTransition)
                             False False Seq.empty NotAReply
                             Nothing mempty Nothing


renderChannelSelect :: ChatState -> Widget Name
renderChannelSelect st =
    withDefAttr channelSelectPromptAttr $
    (txt "Switch to channel [use ^ and $ to anchor]: ") <+>
     (showCursor ChannelSelectString (Location (T.length $ st^.csChannelSelectString, 0)) $
      txt $
      (if T.null $ st^.csChannelSelectString
       then " "
       else st^.csChannelSelectString))

drawMain :: ChatState -> [Widget Name]
drawMain st = [mainInterface st]

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
        options = [ (isReplyable, "r", "reply")
                  , (\m -> isMine st m && isEditable m, "e", "edit")
                  , (\m -> isMine st m && isDeletable m, "d", "delete")
                  , (const hasURLs, "o", openUrlsMsg)
                  , (const hasVerb, "y", "yank")
                  ]
        Just postMsg = getSelectedMessage st

    in hBox [ borderElem bsHorizontal
            , txt "["
            , withDefAttr messageSelectStatusAttr $
              txt $ "Message select: " <> optionStr
            , txt "]"
            , hBorder
            ]

completionAlternatives :: ChatState -> Widget Name
completionAlternatives st =
    let alternatives = intersperse (txt " ") $ mkAlternative <$> st^.csEditState.cedCompletionAlternatives
        mkAlternative val = let format = if val == st^.csEditState.cedCurrentAlternative
                                         then visible . withDefAttr completionAlternativeCurrentAttr
                                         else id
                            in format $ txt val
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

inputPreview :: UserSet -> ChannelSet -> ChatState -> Widget Name
inputPreview uSet cSet st | not $ st^.csShowMessagePreview = emptyWidget
                          | otherwise = thePreview
    where
    uname = st^.csMe.userUsernameL
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
                  st^.csCmdLine.editContentsL
    curStr = T.intercalate "\n" curContents
    previewMsg = previewFromInput uname curStr
    thePreview = let noPreview = str "(No preview)"
                     msgPreview = case previewMsg of
                       Nothing -> noPreview
                       Just pm -> if T.null curStr
                                  then noPreview
                                  else renderMessage pm True uSet cSet
                 in (maybePreviewViewport msgPreview) <=>
                    hBorderWithLabel (withDefAttr clientEmphAttr $ str "[Preview ↑]")

userInputArea :: UserSet -> ChannelSet -> ChatState -> Widget Name
userInputArea uSet cSet st =
    case st^.csMode of
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
        _             -> renderUserCommandBox uSet cSet st

renderDeleteConfirm :: Widget Name
renderDeleteConfirm =
    hCenter $ txt "Are you sure you want to delete the selected message? (y/n)"

mainInterface :: ChatState -> Widget Name
mainInterface st =
    vBox [ hBox [hLimit channelListWidth (renderChannelList st), vBorder, mainDisplay]
         , bottomBorder
         , inputPreview uSet cSet st
         , userInputArea uSet cSet st
         ]
    where
    mainDisplay = case st^.csMode of
        UrlSelect -> renderUrlList st
        _         -> maybeSubdue $ renderCurrentChannelDisplay uSet cSet st
    uSet = Set.fromList (st^..csUsers.to allUsers.folded.uiName)
    cSet = Set.fromList (st^..csChannels.folded.ccInfo.cdName)

    bottomBorder = case st^.csMode of
        MessageSelect -> messageSelectBottomBar st
        _ -> case st^.csCurrentCompletion of
            Just _ | length (st^.csEditState.cedCompletionAlternatives) > 1 -> completionAlternatives st
            _ -> maybeSubdue $ hBox
                 [hLimit channelListWidth hBorder, borderElem bsIntersectB, hBorder]

    maybeSubdue = if st^.csMode == ChannelSelect
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
             hBox [ colorUsername (link^.linkUser)
                  , if link^.linkName == link^.linkURL
                      then emptyWidget
                      else (txt ": " <+> (renderText $ link^.linkName))
                  , fill ' '
                  , renderDate st time
                  , str " "
                  , renderTime st time
                  ] ) <=>
            (vLimit 1 (renderText $ link^.linkURL))

        attr True = forceAttr "urlListSelectedAttr"
        attr False = id
