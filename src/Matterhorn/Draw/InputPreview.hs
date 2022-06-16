{-# LANGUAGE RankNTypes #-}
module Matterhorn.Draw.InputPreview
  ( inputPreview
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Control.Arrow ( (>>>) )
import qualified Data.Text as T
import           Data.Text.Zipper ( insertChar, getText, gotoEOL )
import           Data.Time.Calendar ( fromGregorian )
import           Data.Time.Clock ( UTCTime(..) )
import qualified Graphics.Vty as Vty
import           Lens.Micro.Platform ( SimpleGetter, to )

import           Network.Mattermost.Types ( ServerTime(..), UserId, TeamId
                                          )

import           Matterhorn.Constants
import           Matterhorn.Draw.Messages
import           Matterhorn.Draw.RichText
import           Matterhorn.Themes
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( parseMarkdown, TeamBaseURL )


inputPreview :: ChatState
             -> SimpleGetter ChatState (EditState Name)
             -> TeamId
             -> Name
             -> HighlightSet
             -> Widget Name
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
                  st^.editWhich.esEditor.editContentsL
    eName = getName $ st^.editWhich.esEditor
    curStr = T.intercalate "\n" curContents
    overrideTy = case st^.editWhich.esEditMode of
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
                     tag = MessagePreviewViewport eName
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
                                  , mdClickableNameTag  = tag
                                  }
                 in (maybePreviewViewport vpName msgPreview) <=>
                    hBorderWithLabel (withDefAttr clientEmphAttr $ str "[Preview ↑]")

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

maybePreviewViewport :: Name -> Widget Name -> Widget Name
maybePreviewViewport n w =
    Widget Greedy Fixed $ do
        result <- render w
        case (Vty.imageHeight $ result^.imageL) > previewMaxHeight of
            False -> return result
            True ->
                render $ vLimit previewMaxHeight $ viewport n Vertical $
                         (resultToWidget result)
