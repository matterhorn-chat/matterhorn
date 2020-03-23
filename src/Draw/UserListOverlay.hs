{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Draw.UserListOverlay
  ( drawUserListOverlay
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Graphics.Vty as V

import           Draw.Main
import           Draw.Util ( userSigilFromInfo )
import           Draw.ListOverlay ( drawListOverlay )
import           Themes
import           Types


drawUserListOverlay :: ChatState -> [Widget Name]
drawUserListOverlay st =
    let overlay = drawListOverlay (st^.csUserListOverlay) userSearchScopeHeader
                                  userSearchScopeNoResults userSearchScopePrompt
                                  (renderUser (myUsername st))
    in joinBorders overlay : drawMain False st

userSearchScopePrompt :: UserSearchScope -> Widget Name
userSearchScopePrompt scope =
    txt $ case scope of
        ChannelMembers _ _    -> "Search channel members:"
        ChannelNonMembers _ _ -> "Search users:"
        AllUsers Nothing      -> "Search users:"
        AllUsers (Just _)     -> "Search team members:"

userSearchScopeNoResults :: UserSearchScope -> Widget Name
userSearchScopeNoResults scope =
    txt $ case scope of
        ChannelMembers _ _    -> "No users in channel."
        ChannelNonMembers _ _ -> "All users in your team are already in this channel."
        AllUsers _            -> "No users found."

userSearchScopeHeader :: UserSearchScope -> Widget Name
userSearchScopeHeader scope =
    txt $ case scope of
        ChannelMembers {}    -> "Channel Members"
        ChannelNonMembers {} -> "Invite Users to Channel"
        AllUsers Nothing     -> "Users On This Server"
        AllUsers (Just _)    -> "Users In My Team"

renderUser :: Text -> Bool -> UserInfo -> Widget Name
renderUser myUName foc ui =
    (if foc then forceAttr L.listSelectedFocusedAttr else id) $
    vLimit 2 $
    padRight Max $
    hBox $ (padRight (Pad 1) $ colorUsername myUName (ui^.uiName) (T.singleton $ userSigilFromInfo ui))
           : (hLimit usernameWidth $ padRight Max $ colorUsername myUName (ui^.uiName) (ui^.uiName))
           : extras
    where
        sanitize = T.strip . T.replace "\t" " "
        usernameWidth = 20
        extras = padRight (Pad 1) <$> catMaybes [mFullname, mNickname, mEmail]
        mFullname = if (not (T.null (ui^.uiFirstName)) || not (T.null (ui^.uiLastName)))
                    then Just $ txt $ (sanitize $ ui^.uiFirstName) <> " " <> (sanitize $ ui^.uiLastName)
                    else Nothing
        mNickname = case ui^.uiNickName of
                      Just n | n /= (ui^.uiName) -> Just $ txt $ "(" <> n <> ")"
                      _ -> Nothing
        mEmail = if (T.null $ ui^.uiEmail)
                 then Nothing
                 else Just $ modifyDefAttr (`V.withURL` ("mailto:" <> ui^.uiEmail)) $
                             withDefAttr urlAttr (txt ("<" <> ui^.uiEmail <> ">"))
