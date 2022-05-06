module Matterhorn.Draw.ThreadWindow
  ( drawThreadWindow
  )
where

import Prelude ()
import Matterhorn.Prelude

import Brick
import Brick.Widgets.Border
import Lens.Micro.Platform (Lens', _Just, singular)
import Network.Mattermost.Types (TeamId)

import Matterhorn.Types
import Matterhorn.Draw.Main

drawThreadWindow :: ChatState -> TeamId -> Widget Name
drawThreadWindow st tId =
    joinBorders body
    where
        ti :: Lens' ChatState ThreadInterface
        ti = csTeam(tId).tsThreadInterface.singular _Just

        hs = getHighlightSet st tId
        inMsgSelect = mode == ThreadWindowMessageSelect
        mode = st^.csTeam(tId).tsMode

        -- TODO: "Thread from ~<channel>" or "Thread with @<user>[, @<user>[, ...]]"
        -- depending on whether it's a DM/group/public thread or not
        header = txt "Conversation with <TODO FIXME>"
        body = header <=> hBorder <=> messageUI
        messageUI = drawMessageInterface st hs
                            (ThreadWindowMessages tId)
                            tId inMsgSelect
                            (ti.threadMessageSelect)
                            (ti.threadEditor)
                            (ti.threadMessages)
                            (ThreadWindowEditorPreview tId)
