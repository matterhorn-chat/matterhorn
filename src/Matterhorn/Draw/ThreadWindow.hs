module Matterhorn.Draw.ThreadWindow
  ( drawThreadWindowLayers
  )
where

import Prelude ()
import Matterhorn.Prelude

import Brick
import Brick.Widgets.Border
import Lens.Micro.Platform (Lens', _Just, singular, SimpleGetter)
import Network.Mattermost.Types (TeamId, Type(..))

import Matterhorn.Types
import Matterhorn.Draw.Main
import Matterhorn.Draw.Autocomplete
import Matterhorn.Draw.RichText
import Matterhorn.Draw.Util

drawThreadWindowLayers :: ChatState -> TeamId -> [Widget Name]
drawThreadWindowLayers st tId =
    let ti :: Lens' ChatState ThreadInterface
        ti = csTeam(tId).tsThreadInterface.singular _Just
        ed :: SimpleGetter ChatState EditState
        ed = ti.threadEditor
    in [ autocompleteLayer st ed
       , drawThreadWindow st tId
       ]

drawThreadWindow :: ChatState -> TeamId -> Widget Name
drawThreadWindow st tId =
    joinBorders body
    where
        ti :: Lens' ChatState ThreadInterface
        ti = csTeam(tId).tsThreadInterface.singular _Just

        hs = getHighlightSet st tId
        inMsgSelect = mode == ThreadWindowMessageSelect
        mode = st^.csTeam(tId).tsMode
        cId = st^.ti.threadParentChannelId

        title = case st^?csChannel(cId) of
            Nothing -> "Thread"
            Just chan ->
                let prefix = case chan^.ccInfo.cdType of
                        Group -> "Thread with "
                        Direct -> "Thread with "
                        _ -> "Thread in "
                in prefix <> mkChannelName st (chan^.ccInfo)

        -- TODO: "Thread from ~<channel>" or "Thread with @<user>[, @<user>[, ...]]"
        -- depending on whether it's a DM/group/public thread or not
        header = vLimit 1 $ renderText' Nothing "" hs Nothing title <+> fill ' '

        body = header <=> hBorder <=> messageUI
        messageUI = drawMessageInterface st hs
                            (ThreadWindowMessages tId)
                            tId inMsgSelect
                            False
                            (ti.threadMessageSelect)
                            (ti.threadEditor)
                            (ti.threadMessages)
                            False
                            (ThreadWindowEditorPreview tId)
