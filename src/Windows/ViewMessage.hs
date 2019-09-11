module Windows.ViewMessage
  ( viewMessageWindowTemplate
  )
where

import           Prelude ()
import           Prelude.MH

import qualified Graphics.Vty as Vty
import           Brick

import           Types

viewMessageWindowTemplate :: TabbedWindowTemplate ViewMessageWindowTab
viewMessageWindowTemplate =
    TabbedWindowTemplate { twtEntries = [ messageEntry
                                        , reactionsEntry
                                        ]
                         , twtTitle = const $ txt "View Message"
                         }

messageEntry :: TabbedWindowEntry ViewMessageWindowTab
messageEntry =
    TabbedWindowEntry { tweValue = VMTabMessage
                      , tweRender = renderMessageTab
                      , tweHandleEvent = handleEventMessageTab
                      , tweTitle = const $ const $ txt "Message"
                      }

reactionsEntry :: TabbedWindowEntry ViewMessageWindowTab
reactionsEntry =
    TabbedWindowEntry { tweValue = VMTabReactions
                      , tweRender = renderReactionsTab
                      , tweHandleEvent = handleEventReactionsTab
                      , tweTitle = const $ const $ txt "Reactions"
                      }

renderMessageTab :: ViewMessageWindowTab -> ChatState -> Widget Name
renderMessageTab _ _ = txt "message"

renderReactionsTab :: ViewMessageWindowTab -> ChatState -> Widget Name
renderReactionsTab _ _ = txt "reactions"

handleEventMessageTab :: ViewMessageWindowTab -> Vty.Event -> MH ()
handleEventMessageTab _ _ = return ()

handleEventReactionsTab :: ViewMessageWindowTab -> Vty.Event -> MH ()
handleEventReactionsTab _ _ = return ()
