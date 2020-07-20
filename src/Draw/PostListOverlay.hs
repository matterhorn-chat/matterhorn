{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Draw.PostListOverlay where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Control.Monad.Trans.Reader ( withReaderT )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (^?), (%~), to )

import           Network.Mattermost.Lenses
import           Network.Mattermost.Types

import           Constants ( userSigil )
import           Draw.Main
import           Draw.Messages
import           Draw.Util
import           Themes
import           Types


hLimitWithPadding :: Int -> Widget n -> Widget n
hLimitWithPadding pad contents = Widget
  { hSize  = Fixed
  , vSize  = (vSize contents)
  , render =
      withReaderT (& availWidthL  %~ (\ n -> n - (2 * pad))) $ render $ cropToContext contents
  }

drawPostListOverlay :: PostListContents -> ChatState -> [Widget Name]
drawPostListOverlay contents st =
  (joinBorders $ drawPostsBox contents st) : (drawMain False st)

-- | Draw a PostListOverlay as a floating overlay on top of whatever
-- is rendered beneath it
drawPostsBox :: PostListContents -> ChatState -> Widget Name
drawPostsBox contents st =
  centerLayer $ hLimitWithPadding 10 $ borderWithLabel contentHeader $
    padRight (Pad 1) messageListContents
  where -- The 'window title' of the overlay
        hs = getHighlightSet st
        contentHeader = withAttr channelListHeaderAttr $ txt $ case contents of
          PostListFlagged -> "Flagged posts"
          PostListPinned cId ->
              let cName = case findChannelById cId (st^.csChannels) of
                      Nothing -> "<UNKNOWN>"
                      Just cc ->
                          case cc^.ccInfo.cdType of
                              Direct ->
                                  case cc^.ccInfo.cdDMUserId >>= flip userById st of
                                      Nothing -> mkChannelName (cc^.ccInfo)
                                      Just u -> userSigil <> u^.uiName
                              _ -> mkChannelName (cc^.ccInfo)
              in "Posts pinned in " <> cName
          PostListSearch terms searching -> "Search results" <> if searching
            then ": " <> terms
            else " (" <> (T.pack . show . length) messages <> "): " <> terms

        messages = insertDateMarkers
                     (filterMessages knownChannel $ st^.csPostListOverlay.postListPosts)
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
        renderMessageForOverlay msg tState =
          let renderedMsg = renderSingleMessage st hs Nothing msg tState
          in case msg^.mOriginalPost of
            -- We should factor out some of the channel name logic at
            -- some point, but we can do that later
            Just post
              | Just chan <- st^?csChannels.channelByIdL(post^.postChannelIdL) ->
                 case chan^.ccInfo.cdType of
                  Direct
                    | Just u <- flip userById st =<< chan^.ccInfo.cdDMUserId ->
                        (forceAttr channelNameAttr (txt (userSigil <> u^.uiName)) <=>
                          (str "  " <+> renderedMsg))
                  _ -> (forceAttr channelNameAttr (txt (chan^.ccInfo.to mkChannelName)) <=>
                         (str "  " <+> renderedMsg))
            _ | CP _ <- msg^.mType -> str "[BUG: unknown channel]"
              | otherwise -> renderedMsg

        -- The full message list, rendered with the current selection
        renderedMessageList =
          let (s, (before, after)) = splitDirSeqOn matchesMessage messagesWithStates
              matchesMessage (m, _) = m^.mMessageId == (MessagePostId <$> st^.csPostListOverlay.postListSelected)
              messagesWithStates = (, InThreadShowParent) <$> messages
          in case s of
            Nothing ->
                map (uncurry renderMessageForOverlay) (reverse (toList messagesWithStates))
            Just curMsg ->
              [unsafeRenderMessageSelection (curMsg, (after, before)) renderMessageForOverlay]
