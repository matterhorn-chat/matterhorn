module Matterhorn.Events.Mouse
  ( mouseHandlerByMode
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick

import           Network.Mattermost.Types ( TeamId )

import           Matterhorn.State.Channels
import           Matterhorn.State.Teams ( setTeam )
import           Matterhorn.State.ListOverlay ( listOverlayActivate )
import           Matterhorn.Types

import           Matterhorn.Events.EditNotifyPrefs ( handleEditNotifyPrefsEvent )
import           Matterhorn.State.Reactions ( toggleReaction )
import           Matterhorn.State.Links ( openLinkTarget )

-- The top-level mouse click handler. This dispatches to specific
-- handlers for some modes, or the global mouse handler when the mode is
-- not important (or when it is important that we ignore the mode).
mouseHandlerByMode :: TeamId -> Mode -> BrickEvent Name MHEvent -> MH ()
mouseHandlerByMode tId mode =
    case mode of
        ChannelSelect            -> channelSelectMouseHandler tId
        EditNotifyPrefs          -> handleEditNotifyPrefsEvent tId
        ReactionEmojiListOverlay -> reactionEmojiListMouseHandler tId
        UrlSelect                -> urlListMouseHandler
        _                        -> globalMouseHandler tId

-- Handle global mouse click events (when mode is not important).
--
-- Note that the handler for each case may need to check the application
-- mode before handling the click. This is because some mouse events
-- only make sense when the UI is displaying certain contents. While
-- it's true that we probably wouldn't even get the click events in the
-- first place (because the UI element would only cause a click event
-- to be reported if it was actually rendered), there are cases when we
-- can get clicks on UI elements that *are* clickable even though those
-- clicks don't make sense for the application mode. A concrete example
-- of this is when we display the current channel's contents in one
-- layer, in monochrome, and then display a modal dialog box on top of
-- that. We probably *should* ignore clicks on the lower layer because
-- that's not the mode the application is in, but getting that right
-- could be hard because we'd have to figure out all possible modes
-- where those lower-layer clicks would be nonsensical. We don't bother
-- doing that in the harder cases; instead we just handle the clicks
-- and do what we would ordinarily do, assuming that there's no real
-- harm done. The worst that could happen is that a user could click
-- accidentally on a grayed-out URL (in a message, say) next to a modal
-- dialog box and then see the URL get opened. That would be weird, but
-- it isn't the end of the world.
globalMouseHandler :: TeamId -> BrickEvent Name MHEvent -> MH ()
globalMouseHandler tId (MouseDown n _ _ _) = do
    case n of
        ClickableChannelListEntry channelId -> do
            whenMode tId Main $ do
                resetReturnChannel tId
                setFocus tId channelId
                setMode tId Main
        ClickableTeamListEntry teamId ->
            -- We deliberately handle this event in all modes; this
            -- allows us to switch the UI to another team regardless of
            -- what state it is in, which is by design since all teams
            -- have their own UI states.
            setTeam teamId
        ClickableURLInMessage _ _ t ->
            void $ openLinkTarget t
        ClickableURL _ _ t ->
            void $ openLinkTarget t
        ClickableUsernameInMessage _ _ username ->
            changeChannelByName tId $ addUserSigil username
        ClickableUsername _ _ username ->
            changeChannelByName tId $ addUserSigil username
        ClickableAttachment fId ->
            void $ openLinkTarget $ LinkFileId fId
        ClickableReactionInMessage pId t uIds ->
            void $ toggleReaction pId t uIds
        ClickableReaction pId t uIds ->
            void $ toggleReaction pId t uIds
        _ ->
            return ()
globalMouseHandler _ _ =
    return ()

urlListMouseHandler :: BrickEvent Name MHEvent -> MH ()
urlListMouseHandler (MouseDown (ClickableURLListEntry _ t) _ _ _) =
    void $ openLinkTarget t
urlListMouseHandler _ =
    return ()

channelSelectMouseHandler :: TeamId -> BrickEvent Name MHEvent -> MH ()
channelSelectMouseHandler tId (MouseDown (ChannelSelectEntry match) _ _ _) = do
    setMode tId Main
    setFocus tId $ channelListEntryChannelId $ matchEntry match
channelSelectMouseHandler _ _ =
    return ()

reactionEmojiListMouseHandler :: TeamId -> BrickEvent Name MHEvent -> MH ()
reactionEmojiListMouseHandler tId (MouseDown (ReactionEmojiListOverlayEntry val) _ _ _) =
    listOverlayActivate tId (csTeam(tId).tsReactionEmojiListOverlay) val
reactionEmojiListMouseHandler _ _ =
    return ()
