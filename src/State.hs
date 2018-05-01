module State
  ( refreshClientConfig
  , showHelpScreen
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick.Main ( viewportScroll, vScrollToBeginning )
import           Lens.Micro.Platform

import qualified Network.Mattermost.Endpoints as MM

import           Types

import           State.Common


-- | Refresh client-accessible server configuration information. This
-- is usually triggered when a reconnect event for the WebSocket to the
-- server occurs.
refreshClientConfig :: MH ()
refreshClientConfig = do
    session <- getSession
    doAsyncWith Preempt $ do
        cfg <- MM.mmGetClientConfiguration (Just "old") session
        return (csClientConfig .= Just cfg)

showHelpScreen :: HelpTopic -> MH ()
showHelpScreen topic = do
    mh $ vScrollToBeginning (viewportScroll HelpViewport)
    setMode $ ShowHelp topic
