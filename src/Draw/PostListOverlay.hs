{-# LANGUAGE OverloadedStrings #-}

module Draw.PostListOverlay where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Trans.Reader (withReaderT)
import qualified Data.Foldable as F
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Lens.Micro.Platform
import           Network.Mattermost.Types
import           Network.Mattermost.Lenses

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import Themes
import Types
import Types.Channels
import Types.Messages
import Types.Users
import Draw.Main
import Draw.Messages
import Draw.Util

hLimitWithPadding :: Int -> Widget n -> Widget n
hLimitWithPadding pad contents = Widget
  { hSize  = Fixed
  , vSize  = (vSize contents)
  , render =
      withReaderT (& availWidthL  %~ (\ n -> n - (2 * pad))) $ render $ cropToContext contents
  }

drawPostListOverlay :: PostListContents -> ChatState -> [Widget Name]
drawPostListOverlay contents st =
  (joinBorders $ drawPostsBox contents st) : (forceAttr "invalid" <$> drawMain st)

-- | Draw a PostListOverlay as a floating overlay on top of whatever
-- is rendered beneath it
drawPostsBox :: PostListContents -> ChatState -> Widget Name
drawPostsBox contents st =
  centerLayer $ hLimitWithPadding 10 $ borderWithLabel contentHeader $
    padRight (Pad 1) messageListContents
  where -- The 'window title' of the overlay
        hs = getHighlightSet st
        contentHeader = withAttr channelListHeaderAttr $ txt $ case contents of
          PostListFlagged                -> "Flagged posts"
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
              PostListFlagged            -> "You have no flagged messages."
              PostListSearch _ searching ->
                if searching
                  then "Searching ..."
                  else "No search results found"
          | otherwise = vBox renderedMessageList

        -- The render-message function we're using
        renderMessageForOverlay msg =
          let renderedMsg = renderSingleMessage st hs Nothing msg
          in case msg^.mOriginalPost of
            -- We should factor out some of the channel name logic at
            -- some point, but we can do that later
            Just post
              | Just chan <- st^?csChannels.channelByIdL(post^.postChannelIdL) ->
                 case chan^.ccInfo.cdType of
                  Direct
                    | Just u <- userByDMChannelName (chan^.ccInfo.cdName)
                                                    (myUserId st)
                                                    st ->
                        (forceAttr channelNameAttr (txt (T.singleton '@' <> u^.uiName)) <=>
                          (str "  " <+> renderedMsg))
                  _ -> (forceAttr channelNameAttr (txt (chan^.ccInfo.to mkChannelName)) <=>
                         (str "  " <+> renderedMsg))
            _ | CP _ <- msg^.mType -> str "[BUG: unknown channel]"
              | otherwise -> renderedMsg

        -- The full message list, rendered with the current selection
        renderedMessageList =
          let (s, (before, after)) = splitMessages (st^.csPostListOverlay.postListSelected) messages
          in case s of
            Nothing -> map renderMessageForOverlay (reverse (F.toList messages))
            Just curMsg ->
              [unsafeRenderMessageSelection (curMsg, (after, before)) renderMessageForOverlay]
