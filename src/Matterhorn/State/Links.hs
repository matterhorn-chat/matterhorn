module Matterhorn.State.Links
  ( openLinkTarget
  , openLink
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Control.Exception ( SomeException, catch )
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import           Network.Mattermost.Exceptions
import           Network.Mattermost.Types

import           Matterhorn.State.Common
import {-# SOURCE #-} Matterhorn.State.Messages ( jumpToPost )
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( URL, getPermalink, unURL )


openLink :: URL -> MH ()
openLink url = openLinkTarget =<< linkTargetForURL url

linkTargetForURL :: URL -> MH LinkTarget
linkTargetForURL url = do
    teamIds <- HM.keys <$> use csTeams
    st <- use id
    let pairs = catMaybes [getPermalink (serverBaseUrl st tId) url | tId <- teamIds]
    return $ case pairs of
        [(tName, pId)] -> LinkPermalink tName pId
        _              -> LinkURL url

openLinkTarget :: LinkTarget -> MH ()
openLinkTarget target = do
    session <- getSession
    case target of
        LinkURL url -> openWithOpener (return $ Right $ T.unpack $ unURL url)
        LinkFileId fId -> openWithOpener (fetchAttachment fId session)
        LinkPermalink _ pId -> jumpToPost pId

fetchAttachment :: FileId -> Session -> IO (Either MHError String)
fetchAttachment fId session = do
    (Right <$> fetchFile fId session)
        `catch` (\(e::MattermostError) -> return $ Left $ ServerError e)
        `catch` (\(e::SomeException) -> return $ Left $ GenericError $ T.pack $ show e)
