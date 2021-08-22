module Matterhorn.Events.Mouse
  ( mouseHandlerByMode
  )
where

import           Prelude ()
import           Matterhorn.Prelude

import           Brick
import qualified Graphics.Vty as Vty

import           Matterhorn.Connection
import           Matterhorn.Constants ( userSigil, normalChannelSigil )
import           Matterhorn.HelpTopics
import           Matterhorn.State.ChannelList
import           Matterhorn.State.Channels
import           Matterhorn.State.Common
import           Matterhorn.State.Help
import           Matterhorn.State.Messages
import           Matterhorn.State.Teams ( setTeam )
import           Matterhorn.State.ListOverlay ( listOverlayActivate )
import           Matterhorn.Types

import           Matterhorn.Events.EditNotifyPrefs ( handleEditNotifyPrefsEvent )
import           Matterhorn.State.Reactions ( toggleReaction )
import           Matterhorn.State.Links ( openLinkTarget )

mouseHandlerByMode :: Mode -> Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
mouseHandlerByMode mode =
    case mode of
        ChannelSelect            -> channelSelectMouseHandler
        EditNotifyPrefs          -> editNotifyPrefsMouseHandler
        ReactionEmojiListOverlay -> reactionEmojiListMouseHandler
        UrlSelect                -> urlListMouseHandler
        _                        -> globalMouseHandler

-- Handle mouse click events.
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
globalMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
globalMouseHandler (ClickableChannelListEntry channelId) Vty.BLeft [] _ = do
    whenMode Main $ do
        resetReturnChannel
        setFocus channelId
        setMode Main
globalMouseHandler (ClickableTeamListEntry teamId) Vty.BLeft [] _ =
    -- We deliberately handle this event in all modes; this allows us to
    -- switch the UI to another team regardless of what state it is in,
    -- which is by design since all teams have their own UI states.
    setTeam teamId
globalMouseHandler (ClickableURLInMessage _ _ t) Vty.BLeft [] _ =
    void $ openLinkTarget t
globalMouseHandler (ClickableURL _ _ t) Vty.BLeft [] _ =
    void $ openLinkTarget t
globalMouseHandler (ClickableUsernameInMessage _ _ username) Vty.BLeft [] _ =
    changeChannelByName $ userSigil <> username
globalMouseHandler (ClickableUsername _ _ username) Vty.BLeft [] _ =
    changeChannelByName $ userSigil <> username
globalMouseHandler (ClickableAttachment fId) Vty.BLeft [] _ =
    void $ openLinkTarget $ LinkFileId fId
globalMouseHandler (ClickableReactionInMessage pId t uIds) Vty.BLeft [] _ =
    void $ toggleReaction pId t uIds
globalMouseHandler (ClickableReaction pId t uIds) Vty.BLeft [] _ =
    void $ toggleReaction pId t uIds
globalMouseHandler _ _ _ _ =
    return ()

editNotifyPrefsMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
editNotifyPrefsMouseHandler n b mods l =
    handleEditNotifyPrefsEvent (MouseDown n b mods l)

urlListMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
urlListMouseHandler (ClickableURLListEntry _ t) Vty.BLeft [] _ =
    void $ openLinkTarget t
urlListMouseHandler _ _ _ _ =
    return ()

channelSelectMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
channelSelectMouseHandler (ChannelSelectEntry match) Vty.BLeft [] _ = do
    setMode Main
    setFocus $ channelListEntryChannelId $ matchEntry match
channelSelectMouseHandler _ _ _ _ =
    return ()

reactionEmojiListMouseHandler :: Name -> Vty.Button -> [Vty.Modifier] -> Location -> MH ()
reactionEmojiListMouseHandler (ReactionEmojiListOverlayEntry val) Vty.BLeft [] _ =
    listOverlayActivate (csCurrentTeam.tsReactionEmojiListOverlay) val
reactionEmojiListMouseHandler _ _ _ _ =
    return ()
