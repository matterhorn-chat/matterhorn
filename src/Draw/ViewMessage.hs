module Draw.ViewMessage
  ( drawViewMessage
  )
where

import           Prelude ()
import           Prelude.MH

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Center
import           Lens.Micro.Platform ( to )

import           Draw.Main
import           Draw.Messages ( nameForUserRef )
import           Markdown
import           Themes
import           Types


drawViewMessage :: ChatState -> [Widget Name]
drawViewMessage st = (viewMessageBox st) : (forceAttr "invalid" <$> drawMain st)

viewMessageBox :: ChatState -> Widget Name
viewMessageBox st =
    let body = case st^.csViewedMessage of
          Nothing -> str "BUG: no message to show, please report!"
          Just msg ->
              let hs = getHighlightSet st
                  parent = case msg^.mInReplyToMsg of
                       NotAReply -> Nothing
                       InReplyTo pId -> getMessageForPostId st pId
              in cached ViewMessageArea $
                 renderMessage $ MessageData { mdEditThreshold     = Nothing
                                             , mdShowOlderEdits    = False
                                             , mdMessage           = msg
                                             , mdUserName          = msg^.mUser.to (nameForUserRef st)
                                             , mdParentMessage     = parent
                                             , mdParentUserName    = parent >>= (^.mUser.to (nameForUserRef st))
                                             , mdRenderReplyParent = True
                                             , mdHighlightSet      = hs
                                             , mdIndentBlocks      = True
                                             }

    in centerLayer $
       Widget Fixed Fixed $ do
           ctx <- getContext
           let maxWidth = 80
               maxHeight = 25
               width = min maxWidth (ctx^.availWidthL)
               height = min maxHeight (ctx^.availHeightL)
           render $ vLimit height $
                    hLimit width $
                    borderWithLabel (withDefAttr clientEmphAttr $ str "View Message") $
                    viewport ViewMessageArea Both $
                    body
