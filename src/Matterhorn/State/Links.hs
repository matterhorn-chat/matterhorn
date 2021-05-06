module Matterhorn.State.Links
  ( openLinkTarget
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import qualified Data.Text as T

import           Matterhorn.State.Common
import           Matterhorn.State.Messages ( jumpToPost )
import           Matterhorn.Types
import           Matterhorn.Types.RichText ( unURL )


openLinkTarget :: LinkTarget -> MH Bool
openLinkTarget target = do
    session <- getSession
    case target of
        LinkURL url -> openWithOpener (return $ T.unpack $ unURL url)
        LinkFileId fId -> openWithOpener (liftIO $ fetchFile fId session)
        LinkPermalink _ pId -> jumpToPost pId >> return True
