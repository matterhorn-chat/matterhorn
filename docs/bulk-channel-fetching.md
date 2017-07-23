
By: jtdaugherty

Background
==========

At the time of this writing we have a behavior that I've been wanting to
improve for a long time, which is that on startup, we make O(n) requests
to get channel metadata. As a result, it can take quite a while, even on
fast connections, for all channels with unread messages to be evident
in the sidebar. On slow connections this is even worse. I looked into
the API options and found that there is now a bulk-fetching endpoint for
this data:

https://api.mattermost.com/#tag/channels%2Fpaths%2F~1users~1%7Buser_id%7D~1teams~1%7Bteam_id%7D~1channels~1members%2Fget

The upstream terminology for this is "channel members for a user"
-- in other words, the data structures that describe a user's view
of a channel, i.e., last viewed time, number of mentions, etc. The
mattermost-api type for this data is ChannelData, and typically when
we fetch channel information we use `mmGetChannel`, which gives us a
`ChannelWithData`, i.e., a pair of `Channel` and `ChannelData`.

In addition to just making startup less traffic-intensive and faster,
using this endpoint is also geared toward another objective of mine, and
something that kquick has also been working toward: avoid special cases
in setup and leverage other code paths to get the state configured. So,
for example, rather than fetching all the channels and users at startup
in special-purpose code and then having similar-but-different code paths
for the same operations at runtime, simplify setup so that the runtime
code paths are re-used.

Entanglements
=============

I started working on a change to add support for this endpoint and to
use it to improve our startup process. I ultimately threw away all of
the changes, but I'm capturing what I learned here. After this text I'll
propose a stepwise approach. Here's what happened:

 * I added mattermost-api endpoint support for the endpoint mentioned
   above.

 * I looked at the setup procedure and noticed that although we fetch
   the channel data there, it's really the websocket connection event
   handler that goes and grabs the channel metadata.

 * So I went to the websocket connection event handler
   (`refreshChannels`) and updated it to use this endpoint. This moved
   us from O(n) metadata requests to a single request.

 * When looking `refreshChannels`, it occurred to me that we were
   potentially missing channels that the user got added to (while
   disconnected), and that if `refreshChannels` were to also take care
   of adding new channels, that would mean that `refreshChannels` would
   entirely replace the work we were doing during setup!

 * Then I remove the channel-fetching code from setup and improved
   `refreshChannels` so that when it fetched channels and their
   metadata, it automatically registered any unknown channels with
   the state, or updated ones if they were already known. This meant
   `refreshChannels` worked for both initial connection and reconnection
   and dealt with missing channels unlike before.

 * Having done that, I found that now we had a problem: if we were
   waiting until the websocket was up to do a channel fetch, then there
   was a period of time where the application had no channels. But the
   application is in many ways designed to assume that this is never
   true! I think it's valid for the application to be in this state,
   though: what if the user leaves all the channels they're in? This
   is something I've wanted to tackle for a while, because I think
   it's a valid state and disallowing it makes for some ingrained
   inflexibility.

 * To tackle this assumption that is baked into everything, I first
   modified the Zipper data structure we use so that it can be focused
   on an element *optionally*, i.e., `focus :: Zipper a -> Maybe a`.
   This meant that `Nothing` indicated an empty zipper which had
   no focus, or `Just` otherwise. This meant that on startup, it'd
   initially be empty and so unfocused.

 * Now that the zipper could be unfocused, that meant everything that
   assumed there was always a "current channel" had to be adjusted.
   To deal with this, many functions that used csCurrentChannelId
   or csCurrentChannel had to be gated with a new function,
   `withCurrentChannelId :: (ChannelId -> MH ()) -> MH ()`, that would
   do what you expect and invoke the action, or nothing if there was
   no current channel ID. In most cases this meant that things would
   silently do nothing if invoked outside the context of a selected
   channel, which is reasonable because those functions depended on that
   assumption anyway.

 * This change also revealed the many spots where we rely on an implicit
   current channel, or where we fall back to the current channel in the
   absence of a specified channel.

 * The next issue was dealing with rendering, since without an implicit
   current channel, we needed a way to deal with that. The approach I
   took was to show nothing in the messages area whenever the user is
   not in any channels (i.e. when the zipper has nothing to focus).

   Note that this state would only persist during startup just before
   the bulk channel fetch. More on this below.

 * This also revealed another problem, which is that we were fetching
   users in setup, too. Just like with channels, we have other code
   paths for dealing with users new to the client, and we aren't using
   those (`handleNewUser`).

Proposal
========

I'd like to propose essentially the same approach as described above,
but bottom-up instead of top-down as I did the first time, with the
intention that each step is preparation for the ones that follow and
leaves things working as much as possible.

(For clarity, I'm expecting to do all of this work. I've already done
most of it once, messily, and the second time will be easier.)

To recap, the concerns I'm trying to address here are:

 * Make setup simpler (move intelligence from setup to other spots where
   it already has to live anyway)
 * Make setup more efficient (O(n) -> O(1) requests)
 * Reduce setup vs. runtime code duplication / inconsistency
 * Make the client operational in the absence of channels as an aid to
   this startup improvement and for interactive web browsing (see below)
 * Make channel dependencies explicit

It's worth noting that it's impossible for a user to be in zero
channels, since the server does not permit the user to leave the default
channel (town square). However, we will still encounter this state in
the client on startup. Supporting this state has an additional benefit
that it allows us to get rid of the special initial channel state that
we use for Town Square to control its behavior on startup. Instead we
can initially be in no channel, then switch to whichever channel is the
one we should view first, and use the same logic for switching channels
that we currently have.

This proposal also improves our ability to handle interactive terminal
browsers by making some of the issues easier to deal with; see more at

  https://github.com/matterhorn-chat/matterhorn/issues/266

These first steps should be possible without breaking anything:

 1. In cases where we currently accept an optional channel ID and fall
    back to the current one if it isn't provided, instead always require
    a channel ID (e.g. `doAsyncChannelMM`). This forces the caller to
    make the fallback policy decision, makes it impossible for the
    caller's current channel to differ from the callee's current channel
    (due e.g. to accessing the wrong state), and makes data flow easier
    to understand.

    Done:
    https://github.com/matterhorn-chat/matterhorn/commit/08e9c900167b8d32534c9ab44b669ce11c2ebfde

 2. Remove the `csCurrentChannelId` and `csCurrentChannel` lenses.
    Replace their uses with `withCurrentChannelId` and
    `withCurrentChannel` function calls, respectively, to force explicit
    dependencies on the assumption of a current channel. Initially these
    will just use the current channel as usual. We'll modify them in a
    later step.

    `withCurrentChannelId :: (ChannelId -> MH ()) -> MH ()`
    `withCurrentChannel :: (ClientChannel -> MH ()) -> MH ()`

    Done in
    https://github.com/matterhorn-chat/matterhorn/commit/d9b6ab56c7198071b3e93b26e2bc0bbed9d67006
    but reverted and postponed to the latter half of this plan.

 3. Modify the `WebsocketConnect` handler so that it also refreshes user
    data. This makes it suitable for use at startup and also to pick up
    users added during a disconnection.

    Done:
    https://github.com/matterhorn-chat/matterhorn/commit/3d2c9fa89f410e5680479fa5e974bb96212c317a

 4. Modify `refreshChannels` so that it can handle new channels in
    addition to known channels. This makes it suitable for use at
    startup and also to detect channels that the user was added to
    during a disconnection.

    Done:
    https://github.com/matterhorn-chat/matterhorn/commit/e5c7a8a68be3c08bbbe2a0043762f00648f38182

 5. Modify `refreshChannels` so that instead of iterating over the
    channels already in the state, it asks the server for a full
    list using the bulk endpoint mentioned above. This also provides the
    user-specific metadata, so make `refreshChannels` update the state
    with that, too.

    Done:
    https://github.com/matterhorn-chat/matterhorn/commit/558760cf906ef1770f492fe4b8676e8adfd4f507

As of this point we have the following improvement:

    cygnus@pickle:~/src/matterhorn$ wc -l log_before
         165 log_before
    cygnus@pickle:~/src/matterhorn$ wc -l log_after
          29 log_after

So here are the stats as of this step:

 * Before this refactor: startup time: 14 seconds, 165 log lines
 * After this refactor: startup time: 3.5 seconds, 29 log lines

Now come the steps to get rid of the implicit current channel
assumption:

 7. Modify the Zipper data structure to support optional focus:

    - focus :: Z.Zipper a -> Maybe a
    - focus (Z.fromList []) == Nothing
    - focus (Z.fromList [a]) == Just a
    - (find)left/(find)right/etc transition from Nothing to Just 0 or
      (N-1) as appropriate

 8. Modify `refreshChannels` so that it uses server state to determine
    which channel should be initially selected, rather than hard-coding
    Town Square. Doing this is easy: if the zipper is not focusd on a
    channel, then we're starting up and it's safe to set the focus.
    Otherwise don't do this.

 9. Modify `withCurrentChannel(Id)` to do nothing (skip calls to their
    input actions) when `focus (st^.csFocus) == Nothing`.

 10. Modify `preChangeChannelCommon` and `preChangeChannelCommon` so
     that they can each cope with the new zipper state transformation
     (Nothing -> Just).

 11. Modify the setup process (`initializeState`) so that it does not
     fetch any channels or users. Initialize the channel zipper to empty.

 12. Modify `Draw.Main` to show nothing (or perhaps a helpful message)
     in the channel message area when the channel zipper is unfocused.
     Modify it also to not display a cursor for the message editor.
     (We'll assume that this state is only ever transient and no user
     interaction with the editor will be meaningful in this state. If
     that turns out to be false -- if we want to permit the user to
     interact with the editor in periods of prolonged startup delay --
     we can always improve this later.)

 13. Remove the now-unused special channel state for the initially
     selected channel.
