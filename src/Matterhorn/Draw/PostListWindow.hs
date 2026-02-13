{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Matterhorn.Draw.PostListWindow
  ( drawPostListWindow
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Control.Monad.Trans.Reader ( withReaderT )
import qualified Data.Text as T
import           Lens.Micro.Platform ( (%~) )

import           Network.Mattermost.Types

import           Matterhorn.Draw.MessageInterface ( renderMessageListing
                                                  , messageListingBottomBar
                                                  )
import           Matterhorn.Draw.Util
import           Matterhorn.Events.PostListWindow ( postListWindowKeybindings
                                                  , postListWindowKeyOptions
                                                  )
import           Matterhorn.Themes
import           Matterhorn.Types


drawPostListWindow :: PostListContents -> ChatState -> TeamId -> Widget Name
drawPostListWindow contents st tId = joinBorders $ drawPostsBox contents st tId

hLimitWithPadding :: Int -> Widget n -> Widget n
hLimitWithPadding pad contents = Widget
  { hSize  = Fixed
  , vSize  = (vSize contents)
  , render =
      withReaderT (& availWidthL  %~ (\ n -> n - (2 * pad))) $ render $ cropToContext contents
  }

-- | Draw a PostListWindow as a floating window on top of whatever
-- is rendered beneath it
drawPostsBox :: PostListContents -> ChatState -> TeamId -> Widget Name
drawPostsBox contents st tId =
  centerLayer $
  hLimitWithPadding 10 $
  borderWithLabel contentHeader $
  (padRight (Pad 1) $
   renderMessageListing st True Nothing hs (csTeam(tId).tsPostListWindow)
     False PostList id) <=>
  (messageListingBottomBar st tId (csTeam(tId).tsPostListWindow) (const extraBindings))

  where
        ev = keyEventBindings st (postListWindowKeybindings tId)
        extraBindings = [ (ev evVal, name) | (evVal, name, _, _) <- postListWindowKeyOptions tId ]

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

        entries = filterMessages knownChannel $ st^.csTeam(tId).tsPostListWindow.mlMessages

        knownChannel msg =
            case msg^.mChannelId of
                Just cId | Nothing <- st^?csChannels.channelByIdL(cId) -> False
                _ -> True
