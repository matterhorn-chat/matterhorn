{-# LANGUAGE RankNTypes #-}
module Matterhorn.Draw.MessageInterface
  ( drawMessageInterface
  , renderMessageListing
  , messageListingBottomBar
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Keybindings
import           Brick.Focus ( withFocusRing )
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import           Brick.Widgets.List ( listElements, listSelectedElement, renderList )
import           Brick.Widgets.Edit ( editContentsL, renderEditor, getEditContents )
import           Data.Char ( isSpace, isPunctuation )
import qualified Data.Foldable as F
import           Data.List ( intersperse )
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text.Zipper ( cursorPosition )
import           Data.Time.Clock ( UTCTime(..) )
import           Lens.Micro.Platform ( (.~), (^?!), to, view, Lens', Traversal', SimpleGetter )

import           Network.Mattermost.Types ( ServerTime(..), TeamId, idString
                                          )

import           Matterhorn.Constants
import           Matterhorn.Draw.Buttons
import           Matterhorn.Draw.Messages
import           Matterhorn.Draw.ManageAttachments
import           Matterhorn.Draw.InputPreview
import           Matterhorn.Draw.Util
import           Matterhorn.Draw.RichText
import           Matterhorn.Events.MessageSelect
import           Matterhorn.Events.MessageListing
import           Matterhorn.Events.UrlSelect
import           Matterhorn.State.MessageListing ( getListingSelectedMessage )
import           Matterhorn.Themes
import           Matterhorn.TimeUtils ( DateTimeFormat, justAfter, justBefore )
import           Matterhorn.Types
import           Matterhorn.Types.DirectionalSeq ( emptyDirSeq )
import           Matterhorn.Types.RichText


drawMessageInterface :: ChatState
                     -> HighlightSet
                     -> TeamId
                     -> Bool
                     -> Lens' ChatState (MessageInterface Name i)
                     -> Bool
                     -> Bool
                     -> Widget Name
drawMessageInterface st hs tId showNewMsgLine which renderReplyIndent focused =
    interfaceContents
    where
    cId = st^.which.miChannelId
    eName = getName $ st^.which.miEditor.esEditor
    region = MessageInterfaceMessages eName
    previewVpName = MessagePreviewViewport eName

    interfaceContents =
        case st^.which.miListing.mlMode of
            MessageSelect -> renderMessages True
            ShowUrlList   -> drawUrlSelectWindow st hs (which.miListing)
            SaveAttachment {} -> drawSaveAttachmentWindow st (which.miListing)
            ShowingTail   ->
                case st^.which.miMode of
                    Compose           -> renderMessages False
                    ManageAttachments -> drawAttachmentList st which
                    BrowseFiles       -> drawFileBrowser st which

    editCutoff = getEditedMessageCutoff cId st
    newMsgCutoff = if not showNewMsgLine
                   then Nothing
                   else getNewMessageCutoff cId st

    insertTransitions = insertAllTransitions newMsgCutoff (getDateFormat st) (st ^. timeZone)

    renderMessages inMsgSel =
        vBox [ renderMessageListing st inMsgSel editCutoff hs (which.miListing)
                   renderReplyIndent region insertTransitions
             , bottomBorder inMsgSel
             , inputPreview st (which.miEditor) tId previewVpName hs
             , inputArea st (which.miEditor) focused hs
             ]

    bottomBorder inMsgSel =
        if inMsgSel
        then messageInterfaceBottomBar st tId which
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
        let format = renderText' Nothing (myUsername st) hs Nothing
        in case allTypingUsers (st^.which.miEditor.esEphemeral.eesTypingUsers) of
            [] -> emptyWidget
            [uId] | Just un <- usernameForUserId uId st ->
               format $ "[" <> addUserSigil un <> " is typing]"
            [uId1, uId2] | Just un1 <- usernameForUserId uId1 st
                         , Just un2 <- usernameForUserId uId2 st ->
               format $ "[" <> addUserSigil un1 <> " and " <> addUserSigil un2 <> " are typing]"
            _ -> format "[several people are typing]"

    kc = st^.csResources.crConfiguration.configUserKeysL
    showAttachmentCount =
        let count = length $ listElements $ st^.which.miEditor.esAttachmentList
        in if count == 0
           then emptyWidget
           else hBox [ hLimit 1 hBorder
                     , withDefAttr clientMessageAttr $
                       txt $ "(" <> (T.pack $ show count) <> " attachment" <>
                             (if count == 1 then "" else "s") <> "; "
                     , withDefAttr clientEmphAttr $
                       txt $ ppMaybeBinding (firstActiveBinding kc ShowAttachmentListEvent)
                     , txt " to manage)"
                     ]

messageInterfaceBottomBar :: ChatState
                          -> TeamId
                          -> Lens' ChatState (MessageInterface Name i)
                          -> Widget Name
messageInterfaceBottomBar st tId which =
    messageListingBottomBar st tId (which.miListing) mkExtraOptions
    where
        mkExtraOptions postMsg = messageInterfaceSelectionKeyOptions st tId which postMsg

messageListingBottomBar :: ChatState
                        -> TeamId
                        -> Lens' ChatState (MessageListing Name)
                        -> (Message -> [(T.Text, T.Text)])
                        -> Widget Name
messageListingBottomBar st tId which mkExtraOptions =
    case getListingSelectedMessage which st of
        Nothing -> emptyWidget
        Just postMsg ->
            let optionList = if null usableOptions
                             then txt "(no actions available for this message)"
                             else hBox $ intersperse (txt " ") usableOptions
                usableOptions = mkOption <$> allOptions
                allOptions = messageListingSelectionKeyOptions st tId which postMsg <>
                             mkExtraOptions postMsg
                mkOption (k, desc) = withDefAttr messageSelectStatusAttr (txt k) <+>
                                     txt (":" <> desc)
            in hBox [ hLimit 1 hBorder
                    , txt "["
                    , optionList
                    , txt "]"
                    , hBorder
                    ]

messageInterfaceSelectionKeyOptions :: ChatState
                                    -> TeamId
                                    -> Lens' ChatState (MessageInterface Name i)
                                    -> Message
                                    -> [(T.Text, T.Text)]
messageInterfaceSelectionKeyOptions st tId which msg =
    let ev = keyEventBindings st (messageSelectKeybindings tId which)
        myId = myUserId st
        getUsable (eventVal, label, _, isUsable, _) =
            if isUsable myId msg
            then Just (ev eventVal, label)
            else Nothing
        options = editingContextSensitiveOptions tId which
    in catMaybes $ getUsable <$> options

messageListingSelectionKeyOptions :: ChatState
                                  -> TeamId
                                  -> Lens' ChatState (MessageListing Name)
                                  -> Message
                                  -> [(T.Text, T.Text)]
messageListingSelectionKeyOptions st tId which msg =
    let ev = keyEventBindings st (messageListingKeybindings tId which)
        myId = myUserId st
        getUsable (eventVal, label, _, isUsable, _) =
            if isUsable myId msg
            then Just (ev eventVal, label)
            else Nothing
        options = contextSensitiveOptions tId which
    in catMaybes $ getUsable <$> options

renderMessageListing :: ChatState
                     -> Bool
                     -> Maybe ServerTime
                     -> HighlightSet
                     -> Lens' ChatState (MessageListing Name)
                     -> Bool
                     -> Name
                     -> (Messages -> Messages)
                     -> Widget Name
renderMessageListing st inMsgSelect editCutoff hs which renderReplyIndent region insertTransitions =
    freezeBorders messages
    where
    messages = padTop Max chatText

    chatText = if inMsgSelect
               then freezeBorders $
                    renderMessagesWithSelect (st^.which.mlMessageSelect) messagesWithTransitions
               else cached region $
                    freezeBorders $
                    renderMostRecentMessages st hs editCutoff renderReplyIndent region $
                    retrogradeMsgsWithThreadStates $
                    reverseMessages messagesWithTransitions

    renderMessagesWithSelect (MessageSelectState selMsgId) msgs =
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
                 renderMostRecentMessages st hs editCutoff renderReplyIndent region before
             Just m ->
                 unsafeRenderMessageSelection (m, (before, after))
                     (renderSingleMessage st hs renderReplyIndent Nothing) region

    messagesWithTransitions =
        -- If the message list is empty, add an informative message to
        -- the message listing to make it explicit that this listing is
        -- empty.
        let ms = filterMessageListing st (which.mlMessages)
        in if F.null ms
           then addMessage emptyChannelFillerMessage emptyDirSeq
           else insertTransitions ms

insertAllTransitions :: Maybe NewMessageIndicator -> DateTimeFormat -> TimeZoneSeries -> Messages -> Messages
insertAllTransitions cutoff fmt tz =
    insertDateLines fmt tz .
    insertNewMessagesLine cutoff

insertNewMessagesLine :: Maybe NewMessageIndicator -> Messages -> Messages
insertNewMessagesLine Nothing ms = ms
insertNewMessagesLine (Just val) ms = fromMaybe ms $ do
    newMsg <- case val of
        NewPostsAfterServerTime t
            | anyNondeletedNewMessages t ->
                return $ newMessagesMsg $ justAfter t
            | otherwise ->
                Nothing
        NewPostsStartingAt t
            | anyNondeletedNewMessages (justBefore t) ->
                return $ newMessagesMsg $ justBefore t
            | otherwise ->
                Nothing
        Hide ->
            Nothing
    return $ addMessage newMsg ms
    where
        anyNondeletedNewMessages t =
            isJust $ findLatestUserMessage (not . view mDeleted) (messagesAfter t ms)
        newMessagesMsg d = newMessageOfType (T.pack "New Messages")
                           (C NewMessagesTransition) d

-- | Construct a single message to be displayed in the specified channel
-- when it does not yet have any user messages posted to it.
emptyChannelFillerMessage :: Message
emptyChannelFillerMessage =
    newMessageOfType msg (C Informative) ts
    where
        -- This is a bogus timestamp, but its value does not matter
        -- because it is only used to create a message that will be
        -- shown in a channel with no date transitions (which would
        -- otherwise include this bogus date) or other messages (which
        -- would make for a broken message sorting).
        ts = ServerTime $ UTCTime (toEnum 0) 0
        msg = "There are not yet any messages in this channel."

filterMessageListing :: ChatState -> Traversal' ChatState Messages -> Messages
filterMessageListing st msgsWhich =
    st ^?! msgsWhich . to (filterMessages isShown)
    where isShown m
            | st^.csResources.crUserPreferences.userPrefShowJoinLeave = True
            | otherwise = not $ isJoinLeave m

inputArea :: ChatState
          -> Lens' ChatState (EditState Name)
          -> Bool
          -> HighlightSet
          -> Widget Name
inputArea st which focused hs =
    let replyPrompt = "reply"
        normalPrompt = ""
        editPrompt = "edit"
        addDelimiter = (<> "> ")
        showReplyPrompt = st^.which.esShowReplyPrompt
        maybeHighlight = if focused
                         then withDefAttr focusedEditorPromptAttr
                         else id
        prompt = maybeHighlight $
                 reportExtent (MessageInputPrompt $ getName editor) $
                 txt $ addDelimiter $ case st^.which.esEditMode of
                     Replying {} -> if showReplyPrompt then replyPrompt else normalPrompt
                     Editing {}  -> editPrompt
                     NewPost     -> normalPrompt
        editor = st^.which.esEditor
        inputBox = renderEditor (drawEditorContents st which hs) True editor
        curContents = getEditContents editor
        multilineContent = length curContents > 1
        multilineHints =
            hBox [ hLimit 1 hBorder
                 , str $ "[" <> (show $ (+1) $ fst $ cursorPosition $
                                        editor^.editContentsL) <>
                         "/" <> (show $ length curContents) <> "]"
                 , hBorderWithLabel $ withDefAttr clientEmphAttr $
                   txt $ "In multi-line mode. Press " <> multiLineToggleKey <>
                         " to finish."
                 ]

        replyDisplay = case st^.which.esEditMode of
            Replying msg _ | showReplyPrompt ->
                let msgWithoutParent = msg & mInReplyToMsg .~ NotAReply
                in hBox [ replyArrow
                        , addEllipsis $ renderMessage MessageData
                          { mdMessage           = msgWithoutParent
                          , mdUserName          = msgWithoutParent^.mUser.to (printableNameForAuthor st)
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
        multiLineToggleKey = ppMaybeBinding $ firstActiveBinding kc ToggleMultiLineEvent

        commandBox = case st^.which.esEphemeral.eesMultiline of
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
                                 , showCursor (getName editor) (Location (0,0)) $ str " "
                                 ]
                            else [inputBox]
            True -> vLimit multilineHeightLimit inputBox <=> multilineHints
    in replyDisplay <=> commandBox

drawEditorContents :: ChatState
                   -> SimpleGetter ChatState (EditState Name)
                   -> HighlightSet
                   -> [Text]
                   -> Widget Name
drawEditorContents st editWhich hs =
    let noHighlight = txt . T.unlines
        ms = st^.editWhich.esMisspellings
    in case S.null ms of
        True -> noHighlight
        False -> doHighlightMisspellings hs ms

replyArrow :: Widget a
replyArrow =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let bs = ctx^.ctxBorderStyleL
        render $ str [' ', bsCornerTL bs, '▸']

-- | Tokens in spell check highlighting.
data Token =
    Ignore Text
    -- ^ This bit of text is to be ignored for the purposes of
    -- spell-checking.
    | Check Text
    -- ^ This bit of text should be checked against the spell checker's
    -- misspelling list.
    deriving (Show)

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
doHighlightMisspellings hs misspellings contents =
    -- Traverse the input, gathering non-whitespace into tokens and
    -- checking if they appear in the misspelling collection
    let whitelist = S.union (hUserSet hs) (hChannelSet hs)

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
                    Ignore s -> tokenize (T.tail t) (Ignore $ s <> (T.take 1 t))
                    Check s -> Check s : tokenize (T.tail t) (Ignore $ T.take 1 t)
            | otherwise =
                case curTok of
                    Ignore s -> Ignore s : tokenize (T.tail t) (Check $ T.take 1 t)
                    Check s -> tokenize (T.tail t) (Check $ s <> (T.take 1 t))

    in vBox $ handleLine <$> contents

drawSaveAttachmentWindow :: ChatState
                         -> Lens' ChatState (MessageListing Name)
                         -> Widget Name
drawSaveAttachmentWindow st which =
    center $
    padAll 2 $
    borderWithLabel (withDefAttr clientEmphAttr $ txt "Save Attachment") $
    vBox [ padAll 1 $
           txt "Path: " <+>
           (vLimit editorHeight $
            withFocusRing foc (renderEditor drawEditorTxt) ed)
         , hBox [ padRight Max $
                  padLeft (Pad 1) $
                  drawButton foc (AttachmentPathSaveButton listName) "Save"
                , padRight (Pad 1) $
                  drawButton foc (AttachmentPathCancelButton listName) "Cancel"
                ]
         ]
    where
        editorHeight = 1
        listName = getName $ st^.which.mlUrlList.ulList
        foc = st^.which.mlSaveAttachmentDialog.attachmentPathDialogFocus
        ed = st^.which.mlSaveAttachmentDialog.attachmentPathEditor
        drawEditorTxt = txt . T.unlines

drawUrlSelectWindow :: ChatState -> HighlightSet -> Lens' ChatState (MessageListing Name) -> Widget Name
drawUrlSelectWindow st hs which =
    vBox [ renderUrlList st hs which
         , urlSelectBottomBar st which
         , urlSelectInputArea st which
         ]

renderUrlList :: ChatState -> HighlightSet -> Lens' ChatState (MessageListing Name) -> Widget Name
renderUrlList st hs which =
    urlDisplay
    where
        urlDisplay = if F.length urls == 0
                     then str "No links found." <=> fill ' '
                     else renderList renderItem True urls

        urls = st^.which.mlUrlList.ulList

        me = myUsername st

        renderItem sel (i, link) =
          let time = link^.linkTime
          in attr sel $ vLimit 2 $
            (vLimit 1 $
             hBox $ [ let u = maybe "<server>" id (link^.linkUser.to (printableNameForAuthor st))
                      in colorUsername me u u
                    , case link^.linkLabel of
                        Nothing -> emptyWidget
                        Just label ->
                            case Seq.null (unInlines label) of
                                True -> emptyWidget
                                False -> txt ": " <+> renderRichText me hs Nothing False Nothing Nothing
                                                      (Blocks $ Seq.singleton $ Para label)
                    , fill ' '
                    ] <>
                    case time of
                        Nothing -> []
                        Just t ->
                            [ renderDate st $ withServerTime t
                            , str " "
                            , renderTime st $ withServerTime t
                            ]
            ) <=>
            (vLimit 1 (clickable (ClickableURLListEntry i (link^.linkTarget)) $ renderLinkTarget (link^.linkTarget)))

        renderLinkTarget (LinkPermalink (TeamURLName tName) pId) =
            renderText $ "Team: " <> tName <> ", post " <> idString pId
        renderLinkTarget (LinkURL url) = renderText $ unURL url
        renderLinkTarget (LinkFileId _) = txt " "

        attr True = forceAttr urlListSelectedAttr
        attr False = id

urlSelectBottomBar :: ChatState -> Lens' ChatState (MessageListing Name) -> Widget Name
urlSelectBottomBar st which =
    case listSelectedElement $ st^.which.mlUrlList.ulList of
        Nothing -> hBorder
        Just (_, (_, link)) ->
            let options = [ ( isFile
                            , ev SaveAttachmentEvent
                            , "save attachment"
                            )
                          ]
                ev = keyEventBindings st (urlSelectKeybindings which)
                isFile entry = case entry^.linkTarget of
                    LinkFileId {} -> True
                    _ -> False
                optionList = hBox $ intersperse (txt " ") usableOptions
                usableOptions = catMaybes $ mkOption <$> options
                mkOption (f, k, desc) = if f link
                                        then Just $ withDefAttr urlSelectStatusAttr (txt k) <+>
                                                    txt (":" <> desc)
                                        else Nothing
            in if null usableOptions
               then hBorder
               else hBox [ hLimit 1 hBorder
                         , txt "["
                         , txt "Options: "
                         , optionList
                         , txt "]"
                         , hBorder
                         ]

urlSelectInputArea :: ChatState -> Lens' ChatState (MessageListing Name) -> Widget Name
urlSelectInputArea st which =
    let getBinding = keyEventBindings st (urlSelectKeybindings which)
    in hCenter $ hBox [ withDefAttr clientEmphAttr $ txt "Enter"
                      , txt ":open  "
                      , withDefAttr clientEmphAttr $ txt $ getBinding CancelEvent
                      , txt ":close"
                      ]
