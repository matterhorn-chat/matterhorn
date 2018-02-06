{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draw.UserListOverlay where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Trans.Reader (withReaderT)
import qualified Data.Foldable as F
import qualified Data.Text as T
import           Data.Monoid ((<>))
import qualified Graphics.Vty as V
import           Lens.Micro.Platform

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import qualified Brick.Widgets.List as L
import Brick.Widgets.Center

import Themes
import Types
import Types.Users
import Draw.Main
import Draw.Util (userSigilFromInfo)

hLimitWithPadding :: Int -> Widget n -> Widget n
hLimitWithPadding pad contents = Widget
  { hSize  = Fixed
  , vSize  = (vSize contents)
  , render =
      withReaderT (& availWidthL  %~ (\ n -> n - (2 * pad))) $ render $ cropToContext contents
  }

drawUserListOverlay :: ChatState -> [Widget Name]
drawUserListOverlay st =
  drawUsersBox (st^.csUserListOverlay) :
  (forceAttr "invalid" <$> drawMain st)

-- | Draw a PostListOverlay as a floating overlay on top of whatever
-- is rendered beneath it
drawUsersBox :: UserListOverlayState -> Widget Name
drawUsersBox st =
  centerLayer $ hLimitWithPadding 10 $ vLimit 25 $
  borderWithLabel contentHeader body
  where
      body = vBox [ (padRight (Pad 1) $ str promptMsg) <+>
                    renderEditor (txt . T.unlines) True (st^.userListSearchInput)
                  , cursorPositionBorder
                  , userResultList
                  ]
      plural 1 = ""
      plural _ = "s"
      cursorPositionBorder = case st^.userListSearchResults.L.listSelectedL of
          Nothing -> hBorder
          Just _ ->
              let msg = case st^.userListRequestingMore of
                          True -> "Fetching more results..."
                          False -> case st^.userListHasAllResults of
                              True -> "Showing all " <>
                                       show numSearchResults <>
                                       " result" <> plural numSearchResults
                              False -> "Showing first " <>
                                       show numSearchResults <>
                                       " result" <> plural numSearchResults
              in hBorderWithLabel $ str $ "[" <> msg <> "]"

      scope = st^.userListSearchScope
      promptMsg = case scope of
          ChannelMembers _    -> "Search channel members:"
          ChannelNonMembers _ -> "Search users:"
          AllUsers            -> "Search all users:"

      userResultList =
          if st^.userListSearching
          then showMessage "Searching..."
          else showResults

      showMessage = center . withDefAttr clientEmphAttr . str

      showResults
        | numSearchResults == 0 =
            showMessage $ case scope of
              ChannelMembers _    -> "No users in channel."
              ChannelNonMembers _ -> "All users in your team are already in this channel."
              AllUsers            -> "No users found."
        | otherwise = renderedUserList

      contentHeader = str $ case scope of
          ChannelMembers _    -> "Channel Members"
          ChannelNonMembers _ -> "Invite Users to Channel"
          AllUsers            -> "Users On This Server"

      renderedUserList = L.renderList renderUser True (st^.userListSearchResults)
      numSearchResults = F.length $ st^.userListSearchResults.L.listElementsL

      renderUser foc ui =
          (if foc then forceAttr L.listSelectedFocusedAttr else id) $
          vLimit 2 $
          vBox [ hBox ( colorUsername (ui^.uiName) (T.singleton $ userSigilFromInfo ui)
                      : str " "
                      : colorUsername (ui^.uiName) (ui^.uiName)
                      : case ui^.uiNickName of
                          Just n | n /= (ui^.uiName) ->
                                   [txt (" (" <> n <> ")")]
                          _ -> []
                      )
               , hBox [ str "  "
                      , if (not (T.null (ui^.uiFirstName)) || not (T.null (ui^.uiLastName)))
                        then txt (ui^.uiFirstName <> " " <> ui^.uiLastName <> " ")
                        else emptyWidget
                      , if (T.null $ ui^.uiEmail)
                        then emptyWidget
                        else modifyDefAttr (`V.withURL` ("mailto:" <> ui^.uiEmail)) $
                             withDefAttr urlAttr (txt ("<" <> ui^.uiEmail <> ">"))
                      ]
               ] <+>
          fill ' '
