{-# LANGUAGE OverloadedStrings #-}
module Matterhorn.State.Users
  ( handleNewUsers
  , handleTypingUser
  , handleUserUpdated
  , withFetchedUser
  , withFetchedUserMaybe
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Text as T
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import           Data.Time ( getCurrentTime )
import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM
import           Network.Mattermost.Types

import           Matterhorn.Config
import           Matterhorn.Types
import           Matterhorn.State.Common


handleNewUsers :: Seq UserId -> MH () -> MH ()
handleNewUsers newUserIds after = do
    doAsyncMM Preempt getUserInfo addNewUsers
    where getUserInfo session =
              do nUsers <- MM.mmGetUsersByIds newUserIds session
                 let usrInfo u = userInfoFromUser u True
                     usrList = toList nUsers
                 return $ usrInfo <$> usrList

          addNewUsers :: [UserInfo] -> Maybe Work
          addNewUsers is = Just $ Work "handleNewUsers" $ mapM_ addNewUser is >> after

-- | Handle the typing events from the websocket to show the currently
-- typing users on UI
handleTypingUser :: UserId -> ChannelId -> Maybe PostId -> MH ()
handleTypingUser uId cId threadRootPostId = do
    config <- use (csResources.crConfiguration)
    when (configShowTypingIndicator config) $ do
        withFetchedUser (UserFetchById uId) $ const $ do
            ts <- liftIO getCurrentTime

            -- Indicate that the user is typing in the specified
            -- channel.
            csChannel(cId).ccMessageInterface.miEditor.esEphemeral %= addEphemeralStateTypingUser uId ts

            -- If the typing is occurring in a thread and that thread is
            -- open in some team, also indicate that the user is typing
            -- in that thread's window.
            teams <- use csTeams
            forM_ (HM.keys teams) $ \tId -> do
                pId <- preuse (threadInterface(tId).miRootPostId)
                when (pId == threadRootPostId) $ do
                    threadInterface(tId).miEditor.esEphemeral %= addEphemeralStateTypingUser uId ts

-- | Handle the websocket event for when a user is updated, e.g. has changed
-- their nickname
handleUserUpdated :: User -> MH ()
handleUserUpdated user = do
    csUsers %= modifyUserById (userId user)
        (\ui -> userInfoFromUser user (ui ^. uiInTeam))


-- | Given a user fetching strategy, locate the user in the state or
-- fetch it from the server, and pass the result to the specified
-- action. Assumes a single match is the only expected/valid result.
withFetchedUser :: UserFetch -> (UserInfo -> MH ()) -> MH ()
withFetchedUser fetch handle =
    withFetchedUserMaybe fetch $ \u -> do
        case u of
            Nothing -> postErrorMessage' "No such user"
            Just user -> handle user

withFetchedUserMaybe :: UserFetch -> (Maybe UserInfo -> MH ()) -> MH ()
withFetchedUserMaybe fetch handle = do
    st <- use id
    session <- getSession

    let localMatch = case fetch of
            UserFetchById uId -> knownUserById uId st
            UserFetchByUsername uname -> userByUsername uname st
            UserFetchByNickname nick -> userByNickname nick st

    case localMatch of
        Just user -> handle $ Just user
        Nothing -> do
            mhLog LogGeneral $ T.pack $ "withFetchedUserMaybe: getting " <> show fetch
            doAsyncWith Normal $ do
                results <- case fetch of
                    UserFetchById uId ->
                        MM.mmGetUsersByIds (Seq.singleton uId) session
                    UserFetchByUsername uname ->
                        MM.mmGetUsersByUsernames (Seq.singleton $ trimUserSigil uname) session
                    UserFetchByNickname nick -> do
                        let req = UserSearch { userSearchTerm = trimUserSigil nick
                                             , userSearchAllowInactive = True
                                             , userSearchWithoutTeam = True
                                             , userSearchInChannelId = Nothing
                                             , userSearchNotInTeamId = Nothing
                                             , userSearchNotInChannelId = Nothing
                                             , userSearchTeamId = Nothing
                                             }
                        MM.mmSearchUsers req session

                return $ Just $ Work "withFetchedUserMaybe" $ do
                    infos <- forM (F.toList results) $ \u -> do
                        let info = userInfoFromUser u True
                        addNewUser info
                        return info

                    case infos of
                        [match] -> handle $ Just match
                        [] -> handle Nothing
                        _ -> postErrorMessage' "Error: ambiguous user information"
