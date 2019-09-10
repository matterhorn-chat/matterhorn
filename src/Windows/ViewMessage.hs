module Windows.ViewMessage
  ( viewMessageWindowTemplate
  )
where

import           Prelude ()
import           Prelude.MH

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
                      }

reactionsEntry :: TabbedWindowEntry ViewMessageWindowTab
reactionsEntry =
    TabbedWindowEntry { tweValue = VMTabReactions
                      , tweRender = renderReactionsTab
                      , tweHandleEvent = handleEventReactionsTab
                      }

renderMessageTab :: ViewMessageWindowTab -> ChatState -> Widget Name
renderMessageTab _ _ = txt ""

renderReactionsTab :: ViewMessageWindowTab -> ChatState -> Widget Name
renderReactionsTab _ _ = txt ""

handleEventMessageTab :: ViewMessageWindowTab -> BrickEvent Name MHEvent -> MH ()
handleEventMessageTab _ _ = return ()

handleEventReactionsTab :: ViewMessageWindowTab -> BrickEvent Name MHEvent -> MH ()
handleEventReactionsTab _ _ = return ()
