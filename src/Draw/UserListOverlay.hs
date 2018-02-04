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
import Brick.Widgets.Center

import Themes
import Types
import Types.Users
import Draw.Main

hLimitWithPadding :: Int -> Widget n -> Widget n
hLimitWithPadding pad contents = Widget
  { hSize  = Fixed
  , vSize  = (vSize contents)
  , render =
      withReaderT (& availWidthL  %~ (\ n -> n - (2 * pad))) $ render $ cropToContext contents
  }

drawUserListOverlay :: UserListContents -> ChatState -> [Widget Name]
drawUserListOverlay contents st =
  drawUsersBox contents (st^.csUserListOverlay) :
  (forceAttr "invalid" <$> drawMain st)

-- | Draw a PostListOverlay as a floating overlay on top of whatever
-- is rendered beneath it
drawUsersBox :: UserListContents -> UserListOverlayState -> Widget Name
drawUsersBox contents st =
  centerLayer $ hLimitWithPadding 10 $ borderWithLabel contentHeader body
  where -- The 'window title' of the overlay
        body = vBox [ (padRight (Pad 1) $ str promptMsg) <+>
                      renderEditor (txt . T.unlines) True (st^.userListSearchInput)
                    , hBorder
                    , padRight (Pad 1) userResultList
                    ]
        promptMsg = case contents of
            UserListChannelMembers -> "Search channel members:"
        userResultList
          | null (st^.userListUsers) =
            padTopBottom 1 $ hCenter $ withDefAttr clientEmphAttr $
            str $ case contents of
              UserListChannelMembers -> "No users in channel."
          | otherwise = vBox renderedUserList

        contentHeader = str "Users In Channel"

        users = F.toList (st^.userListUsers)
        renderedUserList = map renderUser users

        renderUser ui =
          vBox [ hBox ( colorUsername (ui^.uiName) (ui^.uiName)
                      : case ui^.uiNickName of
                          Just n | n /= (ui^.uiName) ->
                                   [txt (" (" <> n <> ")")]
                          _ -> []
                      )
               , hBox [ str "  "
                      , if (not (T.null (ui^.uiFirstName)) || not (T.null (ui^.uiLastName)))
                        then txt (ui^.uiFirstName <> " " <> ui^.uiLastName <> " ")
                        else emptyWidget
                      , modifyDefAttr (`V.withURL` ("mailto:" <> ui^.uiEmail)) $
                        withDefAttr urlAttr (txt ("<" <> ui^.uiEmail <> ">"))
                      ]
               ]
