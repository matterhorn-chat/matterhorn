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

drawUserListOverlay :: ChatState -> [Widget Name]
drawUserListOverlay st =
  drawUsersBox (st^.csUserListOverlay) :
  (forceAttr "invalid" <$> drawMain st)

-- | Draw a PostListOverlay as a floating overlay on top of whatever
-- is rendered beneath it
drawUsersBox :: UserListOverlayState -> Widget Name
drawUsersBox st =
  centerLayer $ hLimitWithPadding 10 $ borderWithLabel contentHeader body
  where -- The 'window title' of the overlay
        body = vBox [ (padRight (Pad 1) $ str promptMsg) <+>
                      renderEditor (txt . T.unlines) True (st^.userListSearchInput)
                    , hBorder
                    , padRight (Pad 1) userResultList
                    ]
        scope = st^.userListSearchScope
        promptMsg = case scope of
            ChannelMembers _ -> "Search channel members:"
            AllUsers         -> "Search all users:"
        userResultList
          | null users =
            padTopBottom 1 $ hCenter $ withDefAttr clientEmphAttr $
            str $ case scope of
              ChannelMembers _ -> "No users in channel."
              AllUsers         -> "No users found."
          | otherwise = vBox renderedUserList

        contentHeader = str $ case scope of
            ChannelMembers _ -> "Channel Members"
            AllUsers         -> "Users On This Server"

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
