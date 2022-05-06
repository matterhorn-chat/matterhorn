{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Matterhorn.Draw.PostListOverlay where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Control.Monad.Trans.Reader ( withReaderT )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (%~), to )

import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           Matterhorn.Draw.Messages
import           Matterhorn.Draw.Util
import           Matterhorn.Themes
import           Matterhorn.Types


hLimitWithPadding :: Int -> Widget n -> Widget n
hLimitWithPadding pad contents = Widget
  { hSize  = Fixed
  , vSize  = (vSize contents)
  , render =
      withReaderT (& availWidthL  %~ (\ n -> n - (2 * pad))) $ render $ cropToContext contents
  }

drawPostListOverlay :: PostListContents -> ChatState -> TeamId -> Widget Name
drawPostListOverlay contents st tId = joinBorders $ drawPostsBox contents st tId

-- | Draw a PostListOverlay as a floating overlay on top of whatever
-- is rendered beneath it
drawPostsBox :: PostListContents -> ChatState -> TeamId -> Widget Name
drawPostsBox contents st tId =
  centerLayer $ hLimitWithPadding 10 $ borderWithLabel contentHeader $
    padRight (Pad 1) messageListContents
  where -- The 'window title' of the overlay
        hs = getHighlightSet st tId
        contentHeader = withAttr channelListHeaderAttr $ txt $ case contents of
          PostListFlagged -> "Flagged posts"
          PostListPinned cId ->
              let cName = case findChannelById cId (st^.csChannels) of
                      Nothing -> "<UNKNOWN>"
                      Just cc -> mkChannelName st (cc^.ccInfo)
              in "Posts pinned in " <> cName
          PostListSearch terms searching -> "Search results" <> if searching
            then ": " <> terms
            else " (" <> (T.pack . show . length) entries <> "): " <> terms

        entries = filterMessages knownChannel $ st^.csTeam(tId).tsPostListOverlay.postListPosts
        messages = insertDateMarkers
                     entries
                     (getDateFormat st)
                     (st^.timeZone)

        knownChannel msg =
            case msg^.mChannelId of
                Just cId | Nothing <- st^?csChannels.channelByIdL(cId) -> False
                _ -> True

        -- The overall contents, with a sensible default even if there
        -- are no messages
        messageListContents
          | null messages =
            padTopBottom 1 $
            hCenter $
            withDefAttr clientEmphAttr $
            str $ case contents of
              PostListFlagged -> "You have no flagged messages."
              PostListPinned _ -> "This channel has no pinned messages."
              PostListSearch _ searching ->
                if searching
                  then "Searching ..."
                  else "No search results found"
          | otherwise = vBox renderedMessageList

        -- The render-message function we're using
        renderMessageForOverlay msg tState tag =
          let renderedMsg = renderSingleMessage st hs True Nothing msg tState tag
          in case msg^.mOriginalPost of
            -- We should factor out some of the channel name logic at
            -- some point, but we can do that later
            Just post
              | Just chan <- st^?csChannels.channelByIdL(post^.postChannelIdL) ->
                 case chan^.ccInfo.cdType of
                  Direct
                    | Just u <- flip userById st =<< chan^.ccInfo.cdDMUserId ->
                        (forceAttr channelNameAttr (txt (addUserSigil $ u^.uiName)) <=>
                          (str "  " <+> renderedMsg))
                  _ -> (forceAttr channelNameAttr (txt (chan^.ccInfo.to (mkChannelName st))) <=>
                         (str "  " <+> renderedMsg))
            _ | CP _ <- msg^.mType -> str "[BUG: unknown channel]"
              | otherwise -> renderedMsg

        -- The full message list, rendered with the current selection
        renderedMessageList =
          let (s, (before, after)) = splitDirSeqOn matchesMessage messagesWithStates
              matchesMessage (m, _) = m^.mMessageId == (MessagePostId <$> st^.csTeam(tId).tsPostListOverlay.postListSelected)
              messagesWithStates = (, InThreadShowParent) <$> messages
              tag = PostList
          in case s of
            Nothing ->
                map (\(m, tst) -> renderMessageForOverlay m tst tag) (toList messagesWithStates)
            Just curMsg ->
              [unsafeRenderMessageSelection (curMsg, (before, after)) renderMessageForOverlay tag]
