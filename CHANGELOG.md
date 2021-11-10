
50200.14.1
==========

Bug fixes:
 * Fixed an issue where some builds would crash right after startup with
   a missing "mappend" exception.

50200.14.0
==========

New features and improvements:
 * The UI elements of the `/notify-prefs` window now support mouse
   interaction.
 * Message attachments can now be clicked with the mouse to open them
   with the configured URL/file opener command.
 * Long verbatim and code blocks can now be truncated when they're too
   large. By default, they are not truncated. (#725) This change adds:
   * A new theme attribute, `verbatimTruncateMessage`, that affects the
     truncation message
   * A new configuration setting, `truncateVerbatimBlockHeight` (default
     `0`) that governs how long code and verbatim blocks are truncated.
     A value of zero disables truncation, while values greater than zero
     cause truncation at the specified line limit.
   * A new command, `/toggle-truncate-verbatim-blocks`, that toggles the
     configuration setting at runtime
 * The channel list now floats muted channels to the end of their
   respective channel groups to mirror the UI behavior of the official
   web client.
 * Reactions posted to messages now visually indicate which reactions
   are posted by the current user (#729). This change adds two new theme
   attributes:
   * "reaction" - used for emoji reactions posted by other users, and
   * "reaction.mine" - used for emoji reactions posted by the current
     user
   This change uses the new attributes for emoji reations rather than
   emojiAttr ("emoji"). We do this to visually indicate which reactions
   the current user has contributed to, so that they do not accidentally
   remove their own reactions due to trying to increment them and
   removing them by accident.
 * The `/join` window lists channels with the display name of each
   channel before that entry's URL name and sorts the entries by display
   name.
 * Matterhorn now searches for XML syntax specifications and
   `emoji.json` using the Cabal package data-files path. This
   makes Matterhorn behave better when installed with `cabal`, but
   Matterhorn's search behavior is otherwise unchanged for users who
   are not running `cabal`-installed builds of Matterhorn. Thanks to
   @sternenseemann for this work!

Bug fixes:
 * Hyperlinks and username references inside of Markdown tables are now
   supported (#739).
 * Post listing windows now display messages chronologically (#731).
 * Post listing windows now show the number of messages, not list
   entries, in the window title bar (#730).
 * The `/join` window now uses the correct server API endpoint to fetch
   the initial dataset, causing more channels to be displayed (#727).

50200.13.0
==========

New features:
 * Matterhorn now has optional mouse support! Thanks to Hari Menon
   (@phsmenon) for this work. From the User Guide: Matterhorn supports
   mouse interaction with some UI elements. To enable mouse support, set
   `enableMouseMode` to `True` in your Matterhorn configuration. Mouse
   interaction is supported on the following user interface elements:
   * Channel list entries can be clicked to switch channels.
   * Channel list entries in channel selection mode (`C-g`) can be
     clicked to switch to the selected channel match.
   * Team names in the team list can be clicked to switch teams.
   * URLs and post links in the URL list (`C-o`) can be clicked to open
     them.
   * URLs in messages can be clicked to open them using the configured
     URL opener.
   * Post links in messages can be clicked to switch to the channel
     containing the post.
   * Usernames in messages can be clicked to switch to the direct
     message channel for the clicked user.
   * Usernames in the "Reactions" tab of the message view window can be
     clicked to switch to the direct message channel for the clicked
     user.
   * Reactions to messages can be toggled by clicking on them.
     Click-toggling also works in the "Search Emoji" window as well as
     the "Reactions" tab of the message view window.
 * Notification scripts now have a richer versioned interface. Thanks to
   Dave Lamkins (@TieDyedDevil) for this work. Details can be found in
   `docs/notification-scripts.md`.
 * Favorite channel support was added. Thanks to Ajay Eeralla
   (@ajayeeralla) for this work.
   * Matterhorn now honors the "favorite" status of channels, displaying
     favorite channels together in a new channel list category.
   * Matterhorn got a new command, `/toggle-favorite`, to toggle the
     favorite status of the current channel.
 * A new `/attach` command can be used to attach a file to the message
   being composed.
 * The URL list got a new keybinding to prompt for attachment save path
   for the selected entry. This added a new key event, `save-attachment`
   (default binding: `s`), for the URL list. When triggered, if the
   selected URL entry is for an attachment, a dialog box is shown
   prompting the user for a path to which to save the attachment. (If
   the entry is not an attachment, the event is ignored.)
 * The attachment file browser's key bindings can now be customized as
   Matterhorn key events. Thanks to Dave Lamkins (@TieDyedDevil) for
   this work. (#628)
 * Matterhorn now supports pipe table syntax in Mattermost messages.
 * The URL selection mode now displays available actions in its UI.
   The URL list's bottom bar now indicates any actions that are
   available for the selected link, similarly to how Matterhorn displays
   such actions for messages in message selection mode. This change also
   adds a new theme attribute, `urlSelectStatus`, that is used to render
   the keybindings shown in the options list.

Bug fixes:
 * Message reactions are now wrapped rather than being truncated by the
   window width (#715)
 * When switching away from one team to another, the previous team now
   has its current channel marked as viewed to fix a bug where unread
   messages in that channel were still seen as unread.

Other improvements:
 * Channel autocompletion alternatives are now trimmed to one row in
   height (thanks Karl Smeltzer (@karljs)).
 * User metadata is now updated correctly in reponse to websocket events
   (#697; thanks Karl Smeltzer (@karljs)).
 * "No route to host" exceptions no longer trigger "unexpected error"
   messages (#714)
 * Matterhorn no longer exposes the server's `/msg` command (#657)
 * Matterhorn no longer exposes the server's `/search` command since
   Matterhorn provides its own UI for that command.
 * The `/group-msg` command was renamed to `/group-create` to make it
   more distinct from the server command which has a different behavior
   (#657).
 * The `/remove-user` command was renamed to `/remove` and the server's
   `/remove` command is no longer exposed (#657).
 * Matterhorn now shows the team position and count in team list (#692).
 * Matterhorn now falls back to UTC when it is unable to load local time
   zone data (#695).

50200.12.0
==========

New features:
 * Matterhorn now supports multiple teams! This includes a number of UI
   changes:
   * Matterhorn no longer prompts for a team on startup.
   * If the configuration file sets `team`, Matterhorn will attempt to
     honor that by selecting that as the active team on startup.
     If the configuration does not specify the `team` setting, the
     initially-selected team is chosen as the first team after sorting
     the team list alphabetically.
   * The window title now reflects the sum of all unread channels in all
     teams, rather than the number of unread channels in the current
     team.
   * If the user is a member of more than one team, the list of teams
     will be displayed at the top of the window with the active team
     highlighted. Each team with unread channels will display the count
     of unread channels next to the team name and teams with unread
     channels will be highlighted. This change comes with a new theme
     attribute, `currentTeam`, to style the selected team in the list.
   * The current team selection can be changed with new keybindings:
     * `next-team` (default binding: `C-Right`)
     * `prev-team` (default binding: `C-Left`)
   * The team list can be reordered manually. In the official web
     client, the team list can be reordered by dragging and dropping
     the team icons; in Matterhorn, this can be done by using two new
     commands, `/move-team-left` and `/move-team-right`, respectively.
     New keybindings to do the same thing were also added:
     * `move-current-team-left` (unbound by default)
     * `move-current-team-right` (unbound by default)
 * The new `/rename-channel-url` command renames the current channel's
   URL name. This differs from the server command `/rename`, which
   renames the current channel's display name. Renaming the URL name
   with this command is equivalent to changing the value of the "URL"
   field in the "Rename Channel" dialog box in the official web client.
   (thanks to Ajay Eeralla for this work)

Other improvements:
 * The channel list now takes up the full window height. Previously
   the message editor and input preview were displayed across the entire
   screen; now those areas stop at the channel list, so the channel list
   gets a few more rows of space to display channels.
 * Matterhorn now parses all incoming Markdown text with the Haskell
   `commonmark` library. Migrating to `commonmark` improved our support
   for many Markdown extensions and improved how Matterhorn handles some
   Markdown syntax.
 * The syntax highlighting XML files in `syntax/` were updated from
   version 0.10.0.3 of the `skylighting` package.

50200.11.0
==========

New features:
 * Matterhorn now opens post links itself rather than invoking the
   configured URL open command. This means that if a user obtains a link
   to a post in the web client and pastes it into a message, Matterhorn
   will detect it, and if the user chooses to open that link, Matterhorn
   will switch to the post's channel and show the post. This change
   comes a new theme attribute, `permalink`, that is used to style post
   permalinks. Post links with no labels appear as `<post link>`; post
   links with labels are rendered with their labels. Post links also
   appear in the URL list accessed with `C-o`, and they are opened by
   the message selection mode `o` binding.
 * Message timestamp visibility can now be controlled with the
   configuration setting `showMessageTimestamps` (default: `True`) and
   can be toggled at runtime with the new `/toggle-message-timestamps`
   command.
 * The channel list can now be displayed on either the left side of
   the screen or the right. This is controlled with a new configuration
   setting, `channelListOrientation`, set to either `left` (the default
   and previous behavior) or `right`.
 * Matterhorn now supports strikethrough syntax in messages. This change
   adds a new theme attribute, `markdownStrikethrough`, that
   is used to style strikethrough text. This change also adds
   support for `strikethrough` as a valid style specifier in theme
   customization files. Bear in mind that your terminal might
   not support strikethrough; for details, see the FAQ entry on
   strikethrough support.
 * Matterhorn now has improved handling for channel topics (headers)
   with multiple lines of text. Previously Matterhorn displayed all
   lines in a multi-line channel topic but replaced newlines with
   spaces. That resulted in broken layouts for multi-line topics where
   layout was important. Now it renders the topics as originally
   intended with user control over how multi-line topics are displayed
   in order to let the user control how the topic affects screen real
   estate use.
   * Added a new key event, `toggle-expanded-channel-topics`, to toggle
     whether channel topics show only their first line or all lines.
   * Added a new configuration setting, `showExpandedChannelTopics`, to
     set this behavior on startup (default: `True`).
   * Added a new command, `/toggle-expanded-topics`, and a new default
     keybinding, `F3`, to control this behavior at runtime.
 * Added a `/topic` command that opens an interactive topic editor when
   run with no arguments. The editor provides a live Markdown preview
   of the channel topic. As part of the UI for this feature, new theme
   attributes were added: `button` and `button.focused` for rendering
   unfocused and focused buttons, respectively.
 * Command autocompletion now queries the server for available commands
   and includes those in the command list. Server commands are also
   indicated as being provided by the server to help distinguish them
   from Matterhorn-specific commands.

Other changes:
 * Channel selection mode (`C-g`) now displays entries more consistently
   with the normal channel sidebar, including:
   * Channel mention counts and visual styles
   * The previous channel sigil (`<`) and first-unread channel sigil
     (`~`)
   * Channel mute status (`(m)`)
   * Unread status and visual style
 * When composing replies, Mattehorn now displays the thread root
   message of the thread being replied to, rather than the message
   the user selected for reply (#670). This is intended to be less
   surprising than the old behavior since showing the root post in the
   preview is consistent with the display of the post in the message
   list once it is posted, where the root post is shown above the reply.
 * Matterhorn no longer considers external program output to constitute
   an error, instead only reporting external program errors on non-zero
   exit status (#665).
 * Matterhorn now decorates all hyperlinks with angle brackets. This
   change is intended to get around issues caused when a hyperlink label
   has a style or color that prevents the normal hyperlink color from
   being used, thus concealing the fact that it is a link. This makes
   all links visible regardless of link label content. In particular,
   links labeled `foo` now get rendered as `<foo>`. Links with no labels
   also get decorated this way.
 * The total unread channel count is now included in the window title
   bar updates.
 * The total unread channel count is now included in the channel list
   header.
 * Long lines in code blocks with no syntax highlighting are now wrapped
   to improve readability.
 * Command autocompletion now sorts command completions to prefer prefix
   matches first.
 * Command autocomplete now makes only one request for the server
   command list per completion attempt.
 * There is now a new `/shortcuts` command to stand in for
   server-provided version which did nothing in Matterhorn.
 * Message selection mode now remains active when:
   * Opening message URLs (`C-s`/`o`),
   * Exiting the message view window (`C-s`/`v`), and
   * Exiting the emoji reaction list window (`C-s`/`a`).
 * The channel list now includes an unread indicator in the DM channel
   section header when any DM channels are unread.
 * The visual style for channel selection match text now includes the
   underline style.
 * All unknown websocket events are now logged rather than triggering
   messages to ask the user to report issues.

Bug fixes:
 * The URL list (`C-o`) now has a fixed header when the channel in
   question is a DM channel.
 * The URL list no longer treats URLs with the same target but different
   labels as duplicates.
 * The message editor's multi-line toggle UI hint now looks up the
   active keybinding rather than hard-coding it.
 * The channel sidebar's unread logic was improved (#655).

50200.10.3
==========

Bug fixes:
 * Fixed an issue where hyperlinks whose labels contained more
   hyperlinks would cause Matterhorn to generate rendering bug report
   messages (#674)

50200.10.2
==========

Bug fixes:
 * New websocket event types will no longer trigger JSON parsing
   exceptions. This issue was causing websocket disconnections which led
   to the client not getting server events.

50200.10.1
==========

Bug fixes:
 * Muted channels no longer appear to have unread messages when a
   message in such a channel is edited (#655)

50200.10.0
==========

New features:
 * Per-channel notification settings for non-DM channels can now be
   managed with the `/notify-prefs` command. As part of that change,
   themes got a new attribute, `brickForm.focusedInput`, that governs
   how focused form inputs are styled. (Thanks to Isaiah Mindich for
   this feature)
 * A new configuration file setting, `messageSelectAfterURLOpen`, was
   added to govern how URL opening works in message selection mode. When
   `False` (the default), opening URLs from a selected message returns
   the application to the main UI (the behavior prior to this release).
   When `True`, opening URLs leaves the application in message selection
   mode, allowing the user to continue opening URLs from other messages
   before returning to main mode manually.
 * The message viewing window now supports paging horizontally. This
   change adds two new key events:
    * `page-left` with default binding `Shift-Left`
    * `page-right` with default binding: `Shift-Right`
 * Matterhorn now sets the terminal window title based on unread
   activity (`matterhorn*` when there are unread messages; `matterhorn`
   otherwise.)
 * The `/theme` command now opens an interactive theme list window to
   choose between available built-in themes.
 * Added support for handling muted channels and respecting the `/mute`
   server command (thanks to Isaiah Mindich for this feature)
 * Make `Shift-Home`, not `Home`, selected the oldest message when in
   normal mode (#258). This change:
    * Changes the default keybinding for entering message selection mode
      and selecting the oldest message from `Home` to `Shift-Home`.
    * Adds a new key event, `select-oldest-message`, that `Shift-Home`
      triggers by default.
    * `Home` now triggers the `editor-home` event. Users wanting the
      old behavior can simply unbind the `editor-home` key event and
      bind `select-oldest-message` to `Home`.
 * New 256-color variants of the built-in light and dark themes were
   added. Those themes are identical to the 16-color variants except
   that larger pools of 256-color values are used to assign colors
   to usernames. If a username color chosen by the new themes is
   objectionable, it can be customized by first using the new
   `/username-attribute` command to find out which attribute is used
   to render a username and then setting it in the theme customization
   config. As part of this change the number of theme attributes for
   usernames was extended to 50, meaning attributes from `username.0` to
   `username.49` now govern the assignment of colors.

Other improvements:
 * The `/members` window now displays the total server user count in
   the title bar to avoid confusion about the number of displayed search
   results, which is capped by the server's API response.
 * Autocompletion now completes `@all` and `@channel` with prefix
   matches only and puts them after all other alternatives in the
   alternative list.
 * The help documentation on themes now documents the `#RRGGBB` theme
   color syntax that was already supported but not documented.
 * Key events now supported in the attachment management UI are now
   included in the keybinding help list.
 * The help documentation on themes now shows an attribute demo for each
   theme attribute using the current theme's colors.
 * Some documentation in the `README` was factored out into other
   documents in `docs/`.
 * The UI for showing typing notifications was improved.
 * The `/leave` command now hide DM and group channels instead of
   reporting an error (#626)

Bug fixes:
 * Matterhorn now properly restores the terminal state before crashing
   with unhandled exceptions (#622)
 * The URL list accessible with `C-o` now shows URLs from messages
   regardless of what kind of user reference they contain (#638)

50200.9.0
=========

New features:
 * More text-editing keybindings are now rebindable rather than being
   hard-coded. In addition, the keybinding events for text editing are
   now respected by editors for the `/join` window, the `/msg` window,
   the `/members` window, as well as channel selection mode (`C-g`). For
   the complete list of supported configurable keybindings, please see
   the `Text Editing` section of `/help` and `/help keybindings` as well
   as the keybinding tables provided by `matterhorn -k`.
 * Matterhorn now has support for dealing with server API request rate
   limiting. When Matterhorn is denied an API request due to rate
   limiting it will now attempt to reissue the request once the server's
   rate limit has reset. Matterhorn will indicate when it does this by
   displaying an informative message in the current channel. If for
   some reason the second attempt fails, the user will be notified. We
   recommend that users encountering rate limiting issues (e.g. on fast
   connections to their servers) contact their server administrators.
 * Channel selection mode (triggered by event `enter-fast-select` /
   default key: `C-g`) now provides two alternative key events for
   changing the selected entry (#609). These new events are alternatives
   for the existing events:
   * `focus-prev-channel-alternate`, default key: `Up`, alternative to
     `focus-prev-channel`
   * `focus-next-channel-alternate`, default key: `Down`, alternative to
     `focus-next-channel`
 * The message editor's tab completion now supports more intelligent
   completion by detecting punctuation following completions.
   Previously, tab completion would insert the completion followed by
   a trailing space; for example, `@u<TAB>` might complete to `@user`
   followed by a space. While this is often desirable behavior, it
   also meant that if the very next input character was a punctuation
   character such as a period, the final text would be `@user .` Based
   on the observation that usually the intention is to end up with
   `@user.`, the editor now automatically replaces the trailing space
   when such a punctuation character is entered. This behavior is
   configurable with the `smartediting` configuration setting which
   defaults to On. The list of characters that trigger this behavior is:
   `.,'";:)]!?`.

Other improvements:
 * The current user's status is now displayed next to the current user's
   username at the top of the channel sidebar (#594).
 * Help for keybindings was improved to list all available bindings for
   each event. The help text also now makes it clear when an event has a
   non-bindable (fixed) key or has no binding at all. These improvements
   affected the output of `/help`, `/help keybindings`, and the output
   of the command-line options to print keybinding tables.
 * The sample configuration documentation clarified how to use the
   `host` field and also how to use Matterhorn without TLS.
 * The message editor's spell checking no longer checks the spelling of
   tokens that start with slashes or user/channel sigils (#611).
 * Matterhorn now sets the Markdown file extension when editing messages
   in external editors in order to help the external editors detect the
   file type and do syntax highlighting (#590).
 * The help text for the logging commands now makes it clear that the
   logging commands log application debug information, not chat
   messages.
 * The autocompletion list no longer appears if only a single completion
   is available when the user autocompletes input text.

Bug fixes:
 * Parsing for the configuration file's `host` field has been improved
   to accept only valid hostnames and IPv4/IPv6 addresses.
 * Editing a message now properly clears the message editor of any
   preexisting input (#614).
 * The example notification scripts for Linux and macOS have been
   improved with backslash and quote escaping (#613, thanks Bernhard
   Walle).
 * Incoming messages are now sanitized of unprintable characters (#604).
 * Matterhorn is now better behaved when the server has disabled custom
   emoji (#600).
 * Matterhorn's parsing of timezone data is now improved platforms where
   it previously could not parse the timezone files (#542).
 * The `/join` channel list no longer includes channels of which the
   user is already a member (#602).
 * The channel messages now render properly whenever the editor's
   multiline preview changes size (#587).

50200.8.0
=========

New features:
 * Support for Personal Access Tokens and browser session tokens was
   added. To take advantage of this feature, paste either type of
   token in the new "Access Token" field of the login user interface
   or set up the new `tokencmd` configuration file setting. See
   the "Authentication" section of the README for help and see
   `docs/sample-config.ini` for documentation on `tokencmd`. Thanks to
   @ftilde for early work on this feature!
 * The attribute used to render the current user's username can now be
   customized. By default the attribute still uses the same color as
   before (chosen based on the username) but is bold. The new theme
   attribute `currentUser` can be customized just like other attributes;
   for details, see `/help themes`.
 * The `focus-next-unread` (default: `M-a`) and
   `focus-next-unread-user-or-channel` (default: no binding) events can
   now be repeatedly triggered to cycle through channels with unread
   activity and then return to the original channel where you started,
   which is now marked with `~` in the channel sidebar.
 * Matterhorn got a new `--check-config` argument that triggers a
   configuration file validation check (thanks @glguy).
 * The Matterhorn login user interface was greatly improved and
   simplified. Now, instead of providing individual fields for server
   hostname and port, the user interface provides a single "Server URL"
   field that is capable of accepting server hostnames with or without
   ports as well as full Mattermost web client browser URLs, complete
   with team names and URL paths. This adds support for servers that run
   at URL paths other than "/" and auto-detects teams in URLs to improve
   the web client user's migration experience. As part of this change,
   the configuration got a new setting, `urlPath`, that corresponds to
   the URL path of the server URL in the login interface. Thanks, @glguy
   and @immindich!

Other improvements:
 * The XML syntax highlighting descriptions were updated.
 * Channels with unread join or leave messages now appear as unread in
   the sidebar as intended.
 * The format-specific command-line options for markdown and plain text
   for generated command and keybinding tables were merged. The old
   interface was:

   * `-k`/`-K` to output configured keybindings in plain and markdown
     formats, respectively
   * `-m`/`-M` to output commands in plain and markdown formats,
     respectively

   And the new interface is:

   * `-f` to set the output format (default: plain)
   * `-k` to output configured keybindings in the active format
   * `-m` to output commands in the active format
 * The macOS example notification script was improved (thanks Geir
   Skjotskift)

50200.7.0
=========

New features:
 * The channel sidebar now displays channels by display name, not URL name (#522).
   * The channel autocompletion list now includes channel display names.
 * Matterhorn now adds the character set with "stty erase" to the Vty
   input map so that the erase character translates to a backspace key
   event.
 * Matterhorn now supports viewing pinned posts and changing pinning status (#213)
   * This change adds a new command, `/pinned-posts`, that will show a
     post list of pinned posts in the current channel.
   * This change adds a new message select key event, `pin-message`
     (default: `p`) to toggle the pinning state of the selected post.
   * A new `[PIN]` indicator is now displayed to the left of pinned posts.
   * A new theme attribute, `pinnedMessageIndicator`, was added to
     render the pinning indicator.
 * A new configuration option now controls certificate validation (#263).
   This change adds a new config setting, `validateServerCertificate`.
   Its default is `True`. The sample config also includes documentation
   about the setting.
 * A new `/purpose` command was added to set channel purpose strings
   (#524).
 * A new example notification script for Mac OS systems was added at
   `notification-scripts/notify-macos`.
 * The help screen (default binding: `F1`) is now reachable from any
   Matterhorn screen (#525).
 * Private channels are now displayed as their own sidebar group (#510).

Other changes:
 * The `markdownEmph` attribute is now being used to render user list
   window and attachment window titles.
 * The output of the `/theme` command was improved.
 * The help UI's default colors were changed to improve contrast (#564)
 * Matterhorn now shows channel purpose strings, not headers, in channel
   autocompletion matches to match the official client's behavior (#524).
 * The notification script API is now documented in
   `docs/notification-scripts.md`.
 * Configuration files without trailing newlines are now supported
   (#540).
 * Websocket action responses are now supported, which fixed a bug where
   typing notifications would cause websockets to get disconnected (#529).
 * Emote messages that get flagged now properly display a flagging
   indicator (#530).
 * Matterhorn now supports generation of a command table similar to its
    support for generating keybinding tables. This is done with the new
   `-m`/`-M` command-line options.
 * Date transition lines at the beginning of channels are now preserved
   (#534).

50200.6.0
=========

Bug fixes:
 * Initialization of the attachment file browser is delayed until it is
   needed.
 * Reply message coalescing now occurs if a non-reply intervening
   message is deleted.
 * When showing the attachment management window, the main UI is dimmed.
 * Fixed a race condition where a script completes after the user
   changes channels.

Other improvements:
 * README: improve mention of display corruption (relevant to #544)
 * README: clarify source build instructions (relevant to #536)

50200.5.0
=========

New features:
 * The configuration file now supports tilde expansion in the following
   fields, where a tilde ("~") will be expanded to the path to the
   user's home directory:
   * `urlOpenCommand`
   * `syntaxDirectories`
   * `activityNotifyCommand`
   * `themeCustomizationFile`
 * The "message view" window (accessible in message selection mode by
   default with the `v` binding) has been redesigned.
   * The window now shows the message and the message's user reactions
     in separate tabs in a tabbed interface.
   * New key events, `select-next-tab` (default: `Tab`) and
     `select-previous-tab` (default: `Shift-Tab`), were added to control
     tab-switching behavior.
   * The window got some keybinding help text.
   * Message text is now wrapped in this view, whereas before long lines
     were not wrapped and had to be scrolled horizontally to be read.
     In addition, code blocks are not wrapped, making wide code blocks
     readable by scrolling as originally intended.
   * New theme attribute names, `tabSelected` and `tabUnselected`, were
     added to style the currently-selected and other tabs, respectively.
   * Sections on the available keybindings in the various tabs for this
     interface were added to the interactive help section on keybindings
     as well as the generated keybindings file provided with Matterhorn.
 * The application's default attachment browser path can now be
   configured, thanks to Victor Shamanov (@wiruzx). See the
   `defaultAttachmentPath` setting in `sample-config.ini` for details.
 * Added the `/log-mark` command to add user-specified marker messages
   to the log to aid in debugging.
 * Logging no longer includes duplicate overlapping messages when paused
   and resumed.
 * Added the `/create-private-channel` command to support creating
   private channels. (#512)
 * The attachment list now supports the `select-up` and `select-down`
   key events.
 * All editors in Matterhorn now support bracketed pastes if your
   terminal emulator supports bracketed paste mode.

Other improvements:
 * Various files (e.g. `keybindings.md`) were moved to `docs/` in the
   repository as well as in releases.
 * The team selection interface at startup now shows team display names.
 * The main user interface now shows the current team and user at the
   top of the channel list sidebar.

Bug fixes:
 * The attachment file browser will now no longer disappear if the user
   attempts to navigate a symlinked directory. Instead, the symlinked
   directory will be navigated as expected. (#507)
 * The "Fetching messages" message that appears in channels with pending
   server responses now gets cleaned up properly. (#498)
 * Creating new `/group-msg` channels now properly shows the new group
   immediately; previously the `/group-msg` command had to be issued a
   second time or Matterhorn had to be restarted. (#477)

50200.4.0
=========

New features:
 * This release includes support for viewing, adding, and removing post
   emoji reactions:
    * A new message select keybinding (default `a`, event
      'react-to-message') opens an emoji search window.
    * Pressing 'Enter' in the emoji search window posts the selected
      reaction to the selected message if you have not already posted
      that reaction, or removes it from the message otherwise.
    * The emoji search window automatically lists reactions that other
      users have posted before other emoji, along with your own posted
      reactions. This allows you to easily add your vote to (or remove)
      reactions.
    * Emoji reactions posted to a message are now displayed in the
      message view window (message select binding `v`).
 * Emoji are now supported in autocompletion by pressing `TAB` after a
   colon (`:`). The autocompletion list also includes custom emoji
   obtained from the server. The standard emoji are sourced from the
   file `emoji.json` located in the same directory as the Matterhorn
   configuration file `config.ini`, or at the path `emoji/emoji.json`
   relative to the location of the `matterhorn` binary (as in releases).
 * The channel list shown by the `/join` command has been redesigned to
   support incremental search similar to the UI for `/msg`.
 * Special `@all` and `@channel` mentions are now highlighted and
   included in autocompletion. (#491)
 * If a channel has new messages that go beyond the top of the displayed
   message area, a `New Messages` line appears at the top of the window
   to indicate that more unread messages precede it. (#131)

Other improvements:
 * The subprocess logger indicates when unexpected stdout output was
   received. (#503)
 * The log manager thread is now started before authentication begins so
   that the authentication code can do logging.
 * Keybinding event names are now included in auto-generated text and
   Markdown keybinding tables.
 * User status updates are automatically batched, resulting in `O(1)`
   user status requests on startup rather than `O(n)`.
 * Command help text now includes `@` and `~` in arguments to more
   clearly indicate user and channel arguments respectively.

Bug fixes:
 * Theme changes now invalidate the rendering cache, fixing color
   artifacts left over in the channel message area.
 * When searching for users, username sigils are always trimed first.
   (#497)

50200.3.1
=========

Bug fixes:
 * Matterhorn now correctly handles server API responses with
   Content-type parameters (#496).

50200.3.0
=========

New features:
 * Message reply rendering has been improved. Matterhorn now uses a
   rendering almost identical to that of the web client: rather than
   rendering an arrow from the reply to its parent, Matterhorn now
   indents the reply with a vertical border to its left. Matterhorn now
   also only renders the reply's parent message if it is not already
   adjacent or if it is not in the same thread as the previous message.
   This greatly reduces clutter in channels with a lot of threaded
   message activity. (#490)
 * At startup, Matterhorn no longer restores the terminal state to the
   pre-startup state in between authentication attempts.
 * At startup, if Matterhorn immediately attempts a connection (due to
   credentials being available in the configuration), a "Connecting to
   ..." message is displayed while the connection attempt is underway.
 * The syntax highlighting XML specification files included with
   Matterhorn were updated to the versions in the upstream Skylighting
   package as of commit `bf3e5d163c3e7b9f98d1cd3b9aeb6780a6f76572`. For
   more details, see

   * https://github.com/matterhorn-chat/matterhorn/commit/70929a5bb029b9c37bf0cdcf4365328b3e8e43d6
   * https://github.com/jgm/skylighting

 * The help and usage messages for the `/msg` command were improved.
 * The `run.sh` script got various bug fixes and portability
   improvements.

Bug fixes:
 * Messages from bots now properly display a "[BOT]" indicator next to
   the post author username (#494)
 * Matterhorn no longer reports timeout-related network socket
   exceptions to the user (#492)

50200.2.0
=========

New features:
 * Channel scrolling mode has been merged with message select mode.
   There are no longer two separate modes that the user must switch
   between for fetching additional messages and operating on displayed
   messages.
   * As part of this change, message gaps in the local client history
     are displayed as special entries in the message history, along with
     specific key commands that can be used to fetch additional messages
     at those points.
 * The `/msg` command now takes optional user and message arguments to
   make it faster and easier to initiate discussions with specific
   users: `/msg [@user [message]]`.  This resolves enhancement issue
   #471.
 * The `/search` and `/flags` post listings now support navigating to a
   post by pressing `Enter` when it is selected. This causes the client
   to switch to the post's channel and select the post in message
   selection mode.
 * If smart editing is enabled and a triple backtick is typed at the
   start of a line, input automatically switches to multi-line mode.
   Fixes issue #467.
 * Editor status is now tracked on a per-channel basis. The following
   editor state is tracked on a per-channel basis and is restored
   between channel changes:
   * Incomplete message (editor) entries.
   * Multi-line editor mode.
   * Input history scrollback position.
   * Incomplete message reply-to or editing-previous-post status.
   This resolves enhancement issue #445.
 * Adds the `/user` command as an alias for the `/msg` command.
 * Channel auto-completion mode indication of the user's membership in
   the channel is more clearly indicated.
 * A new configuration item (`directChannelExpirationDays`) is added.
   In release 50200.1.0, the channel sidebar management was updated to
   automatically remove direct channels if there had been no activity
   for 7 days.  This configuration item allows overriding that time
   period, (thanks, Brent Carmer).
 * A new configuration item (`cpuUsagePolicy`) is added.  Valid values
   are `single` and `multiple` which determines how many of the
   available processors Matterhorn will make use of.  The default is
   `multiple`.  This resolves enhancement issue #484.
 * Additional information on managing attachments (relevant to issue
   #485).

New command-line options:
 * `-k`: print the active keybindings in plain text format. The output
   includes customized bindings in the current configuration, or the
   defaults if `-i` is used.
 * `-K`: print the active keybindings in Markdown format. The output
   includes customized bindings in the current configuration, or the
   defaults if `-i` is used.

Documentation updates:
 * The FAQ is updated to help users with terminals that don't support
   hyperlinks (thanks, Bob Arezina).
 * Updated the Design notes and development Practices documentation
   for Matterhorn.
 * Matterhorn releases now include the default keybindings in a Markdown
   document, `keybindings.md`.

Bug fixes:
 * Updated to mattermost-api 50200.1.2 which includes a bugfix for a
   server "UploadResponse" with a null `client_ids` field.  This
   manifested as an error report when attempting to post a message
   with an attachment to a general channel.
 * Misc process and maintenance changes to re-enable and update the
   Travis builds (fully passing now).

Dependency updates:
 * Added support for building with GHC 8.6 (thanks, Adam Wick).

50200.1.2
=========

Bug fixes:
 * Permit user creation timestamp to be optional in server responses
   (#483)

50200.1.1
=========

Bug fixes:
 * The file browser now deals gracefully with directories containing
   broken symlinks.

50200.1.0
=========

This release contains many new features and changes. Please read
carefully!

New features:

 * Matterhorn is now supported on CentOS 7. See the GitHub release tag
   for a prebuilt CentOS 7 binary.
 * Matterhorn's channel sidebar has changed significantly:
   * Matterhorn now only shows recently-active DM channels in the
     sidebar rather than showing all users. This change brings
     Matterhorn closer to the behavior of the official web client and
     reduces clutter. However, this change may be disorienting since
     previously all users were shown in the sidebar. To begin a chat
     with a user absent from your sidebar, use the `/msg` command.
   * The sidebar channel group headings have been renamed to reflect the
     new changes and to match those of the official web client.
   * Group DM channels are now displayed in the new Direct Messages
     section along with one-on-one DM channels.
   * Each channel group heading now displays an asterisk if any channels
     in that group have unread activity.
 * The autocompletion user interface has been dramatically improved.
   * The command completion UI now appears automatically if the user
     types a slash at the beginning of the prompt. Any text typed after
     the slash is matched case-insensitively on all client-side command
     names and descriptions, with a preference for matching command
     names. Tab then cycles through the displayed alternatives.
   * The user completion UI now appears automatically if the user enters
     a user sigil ("@"). Text typed after the sigil searches usernames,
     nicknames, and full names. Users in the current channel appear
     first and users not in the current channel appear next, marked with
     an asterisk and a help hint. Tab cycles through completion of the
     alternatives.
   * The channel completion UI now appears automatically if the user
     enters a channel sigil ("~"). Text typed after the sigil searches
     channel names, headers, and purposes. Channels of which the user
     is a member are listed first, with other channels following. Tab
     cycles through completion of the alternatives.
   * The fenced code block language name completion UI now appears
     automatically if the user types three backticks to begin a code
     block. Text following the backticks searches known language names
     case-insensitively with a preference for prefix match. Tab cycles
     through the alternatives.
 * Matterhorn now supports adding attachments to posts:
   * The default binding of `C-x` shows a file browser.
   * `Enter` in the file browser selects a regular file for attachment
     (or, if used on a directory, navigates to that directory in the
     browser).
   * The default cancellation binding of `Esc` closes the browser.
   * The default binding of `o` opens the selected browser entry using
     the URL opening command.
   * Once a file is attached, an attachment count is displayed above the
     message editor.
   * Pressing `C-x` again will open the attachment list for the current
     post, where existing attachments can be removed (`d`) or new ones
     can be added (`a`).
 * The channel selection mode (`C-g`) user input is now performed with a
   line editor, meaning that all of the text-editing keybindings that
   are supported in the message editor are now supported in channel
   selection mode.
 * Channel sidebar cycling keys (`C-n`, `C-p`) now cycle through all
   channels including DM channels, not just non-DM channels as before.

New commands:

 * `/reconnect`: forces a disconnection and reconnection of Matterhorn's
   websocket connection in case of network connection trouble.
   Ordinarily Matterhorn will time out and determine that its connection
   needs to be reestablished, but this command may be useful for other
   situations.
 * `/hide`: hides the current single-user or group direct message
   channel from the sidebar. The channel will reappear later if new
   messages arrive in the channel.

New key events:

  * `show-attachment-list` (default `C-x`): show the attachment list or
    file browser to attach a file to the current post.
  * `add-to-attachment-list` (default `a`): in the attachment list, open
    the file browser to select an additional file to attach to the
    current post.
  * `delete-from-attachment-list` (default `d`): in the attachment list,
    delete the selected pending attachment. (This does not affect the
    original disk file.)
  * `open-attachment` (default `o`): in the attachment list or file
    browser, open the selected file or attachment with the URL opening
    command, if one is configured.

New theme attributes:

 * `fileBrowser`: the base attribute for the attachment file browser.
 * `fileBrowser.currentDirectory`: the attribute for the file browser's
    heading displaying its working directory.
 * `fileBrowser.selectionInfo`: the attribute for the file browser's
    footer displaying information about the selected file or directory.
 * `fileBrowser.directory`: the attribute used for directories in the
    file browser.
 * `fileBrowser.block`: the attribute used for block devices in the file
    browser.
 * `fileBrowser.regular`: the attribute used for regular files in the
    file browser.
 * `fileBrowser.char`: the attribute used for character devices in the
    file browser.
 * `fileBrowser.pipe`: the attribute used for named pipes in the file
    browser.
 * `fileBrowser.symlink`: the attribute used for symbolic links in the
    file browser.
 * `fileBrowser.unixSocket`: the attribute used for Unix sockets in the
    file browser.

New command-line options:

 * `-i`/`--ignore-config`: start Matterhorn with no configuration, i.e.,
   ignore the default configuration.

Performance improvements:

 * Matterhorn now renders its user interface more efficiently, resulting
   in improved input latency, especially on larger window sizes.
 * Matterhorn now makes fewer requests on startup, resulting in lower
   server API burden and improved startup performance.
 * Message username and channel name highlighting is now more efficient.

Bug fixes:

 * Username highlighting in messages now supports usernames containing
   periods and underscores (#373)
 * Matterhorn will now ask for a keypress if an interactive URL opener
   exits with a non-zero exit status, to allow the user to observe any
   error output before returning to Matterhorn (#462)
 * `/group-msg` now supports username arguments with sigils.
 * Matterhorn now properly updates channel view timestamps under the
   right conditions (#449)
 * The bundled `notify` script now uses `bash`, not `sh`, to get
   extended `[` functionality.

Miscellaneous:

 * The help interface now takes up the entire screen when it is active.
 * The user list overlay for `/msg` now respects the direct message
   restriction server setting (#378).

50200.0.0
=========

This release is compatible with server version 5.2.

New features:

 * The channel list sidebar can now be shown and hidden:
   * With the `/toggle-channel-list` command
   * With the default binding of `F2`
   * With the customized keybinding name of
     `toggle-channel-list-visibility`
 * The account preference for teammate name display is now honored and
   takes precedence over the server default; at present, only the
   "nickname" and "username" settings are supported.
 * Channel selection mode now matches case-insensitively if the input is
   entirely lowercase and matches case-sensitively otherwise.
 * Non-public channels now include their privacy level in the channel
   header.
 * A bundled "talky" script is now provided for working with the Talky
   video chat service: https://talky.io/ (Thanks @tommd)
 * Theme styles now support italics in terminals that support it. See
   the Theme help info for details.
 * A new key event, "focus-next-unread-user-or-channel", was added.
   This event changes to the next channel with unread messages,
   preferring direct message channels. This event has no default
   binding. (Thanks Brent Carmer)

Bug fixes:
 * Multiline mode in the built-in message editor is now preserved upon
   returning from editing with an external editor (#419)
 * Mixed formatting in hyperlink labels is now supported (#418)
 * When dealing with posts internally, we now use the post's root ID,
   not its parent ID, everywhere since parent IDs are unused (#404)
 * Private channels are no longer included in the tab-completion
   alternative list
 * Theme customization files can now use empty style lists.
 * Message selection mode now disallows selection of deleted messages.
 * The disconnection message is now displayed in a layer to avoid layout
   reflowing in the channel header.
 * Message view mode now properly displays reply parent (#397)
 * The startup team listing is now sorted by team name.
 * We now log websocket timeout and generic exceptions (#385)

40901.0.0
=========

New features:
 * Improved logging infrastructure. Matterhorn now keeps a running
   bounded internal buffer of log messages that can be written to disk
   at any time, in addition to supporting the usual mode where all
   messages are logged to a file. This feature includes:
   * Four new commands:
     * `/log-status`: check on whether Matterhorn is currently logging
       at all.
     * `/log-start <path>`: start logging to the specified file.
     * `/log-stop`: stop any active logging.
     * `/log-snapshot <path>`: dump the internal log message buffer to
       the specified file.
   * A new optional configuration setting, `logMaxBufferSize`, which is
     the number of entries in the internal log buffer.
 * Added a new message selection mode keybinding to view individual
   messages (default: `v`, keybinding name: `view-message`). This
   keybinding causes the selected message to be displayed alone in a
   pop-up window in which horizontal and vertical scrolling can be used
   to view the entire message. This is especially useful in cases where
   a message contains a code block with lines that could not be wrapped.
   (#377)
 * Message selection mode now supports selection of error and info
   messages in addition to just user posts.
 * Message selection mode now supports yanking the contents of all
   messages, not just messages with verbatim content as before. The
   keybinding name for this is `yank-whole-message` and defaults to `Y`.
   When this binding is used, the entire message's Markdown source is
   copied to the system clipboard. (#171)

Other changes:
 * The help screen now supports the `scroll-top` and `scroll-bottom`
   events (defaulting to the `Home` and `End` keys, respectively).
 * Exceptions related to spurious network problems with `getAddrInfo` no
   longer cause client error reports (#391).

40900.0.1
=========

Bug fixes:
 * Terminal escapes are now sanitized from local user input and
   user-provided values from the server (#390).
 * The Cabal package now includes XML syntax and DTD files in the
   manifest.
 * Releases now ship the syntax DTD.

40900.0.0
=========

This release supports Mattermost server version 4.9.

New features:
 * Matterhorn now supports activity notifications by invoking a
   user-configured external command. The external command is configured
   with the `activityNotifyCommand` setting. Matterhorn also includes
   an example notification script for Linux-based systems in the
   `notification-scripts` directory. Thanks to Jason Miller for this
   feature!
 * Matterhorn now bundles syntax highlighting language descriptions
   rather compiling them into the binary. This resolves issue #372.
   The XML descriptions are in the Kate project's language description
   format and are provided with their accompanying GPL license.
   Syntax description files are loaded from a prioritized list
   of directories. The list of directories is configurable using
   the optional `syntaxDirectories` configuration setting. See
   `sample_config.ini` for details.
 * Matterhorn now tab-completes available syntax highlighting language
   options on lines starting with the code block Markdown syntax (three
   backticks) (#354).

Other changes:
 * Tab-completion now always fills in user sigil on username and
   nickname completions, and fills in channel sigils on channel name
   completions.
 * Usernames in posts are now only highlighted when a user sigil is
   present.

Bug fixes:
 * Matterhorn now switches to a channel after it has been joined (#355).
 * Fixed emote editing (#388).

40800.0.3
=========

Bug fixes:
 * Fixed a bug that prevented DM channel changes when the DM channels
   were named with user nicknames (#384)
 * "Resource vanished" exceptions will no longer be silenced during
   async work.

40800.0.2
=========

Bug fixes:
 * Matterhorn now properly handles servers with users whose usernames
   match existing channel names.
 * The flagged post list now ignores flagged posts from channels of
   which the current user is no longer a member.
 * Tab-completion now supports nickname completion in the presence of
   user sigils (relevant to #382).
 * The `/search` command now copes better with an empty string as input
   (#359).
 * The `group-msg` command now switches to the relevant channel if it
   already exists (#367).

Other changes:
 * Message edits no longer cause channels to appear to have unread
   activity.
 * The Haskell runtime's idle garbage collector was disabled, reducing
   Matterhorn's idle CPU usage from 2-4% on some systems to zero.

40800.0.1
=========

Bug fixes:
 * User list overlays shown by `/msg`, `/add-user`, and `/members`
   restrict results to the current team only.
 * Message posts, channel view events, and channel metadata update
   events destined for other teams are now properly ignored by
   Matterhorn if the team for those events doesn't match the current
   session's team.

40800.0.0
=========

This release supports Mattermost server version 4.8.

New features:
 * Matterhorn now uses a connection pool with persistent server
   connections for improved performance (thanks to Abhinav Sarkar)
 * Matterhorn now honors the server-side user preference about whether
   join/leave messages should be shown or hidden.
 * Matterhorn now honors the server-side configuration setting that
   determines whether users are displayed by nickname if possible
   (thanks to Kelly McLaughlin)

New configuration settings:
 * The "hyperlinkURLs" setting controls whether Matterhorn emits
   hyperlinking escape sequences. It defaults to on, but can be disabled
   for users using terminal emulators that do not handle such escapes
   gracefully when they don't support hyperlinking (see #374)

Bug fixes:
 * On (re-)connection, Matterhorn now fetches all users rather than just
   the first few hundred.

40700.0.0
=========

This release supports Mattermost server version 4.7.

New features:
 * The `/focus` command with no arguments now starts channel selection
   mode, equivalent to the default binding of `C-g`.
 * The `/join` command now accepts an optional channel name argument. If
   provided, the named channel is joined (#361).
 * A new user browser was added! The user browser presents a list of
   users and the ability to search users by name. The new user list
   powers some new and existing commands:
   * A new `/msg` command is used to browse known users and select a
     user with `Enter` to begin a private chat session with the selected
     user.
   * A new `/add-user` command is used to add users to the current
     channel. The list shows users who are not already members of the
     channelcurrent and `Enter` adds the selected user to the channel.
   * The existing `/members` command now shows a browsable user list of
     members of the current channel. `Enter` begins a private chat
     session with the selected user.

Bug fixes:
 * Missing `urlOpenCommand`s are now reported as error messages rather
   than informative messages.
 * More login-related exceptions are now displayed in a more readable
   format on the login screen (#358).
 * Channel selection mode now prefers an exact match as the initial
   cursor selection if one exists (#356).
 * Replies now indicate the correct parent message in the message list.
 * The multi-line editor help message now shows the active binding
   (previously `M-e`).

40600.1.0
=========

Performance improvements:
 * Matterhorn's reconnection handling was improved to more reliably
   fetch messages that arrived while the client was disconnected.
 * Startup performance was improved by reducing redundant post and user
   metadata fetches when loading channel messages.

Other fixes:
 * The multi-line toggle help message now shows the active binding.
 * Slash commands now support multi-line input. Previously only the
   first line was passed as the command input.
 * Matterhorn now updates channel view status on updates from other
   clients (#342)

40600.0.0
=========

This release supports Mattermost server version 4.6.

New features:
 * Rebindable keys are now supported! See `/help keybindings` for
   details. Matterhorn also checks for conflicting bindings on startup.
 * The user status list now supports the Do Not Disturb status (shown as
   `×`).
 * User typing notifications are now supported. These are off by default
   but can be enabled with the `showTypingIndicator` configuration
   setting. Enabling the feature causes Matterhorn to produce such
   notifications for the server and to display typing indications from
   other users. Thanks to Abhinav Sarkar for this feature!
 * Matterhorn now remembers which channel was visited when the client is
   closed and returns to that channel on startup. Thanks to Abhinav
   Sarkar for this feature!

New commands:
 * `/message-preview` now toggles message preview mode in addition to
   default `M-p` keybinding.

Bug fixes:
 * New post reactions no longer cause a post to be indicated as
   "(edited)" (#333)

UI changes:
 * The channel list shown by `/join` now also displays the channel
   purpose for each channel when possible.

Performance improvements:
 * Matterhorn now has much lower input latency on servers with very
   large numbers of users due to user list rendering performance
   improvements.

Miscellaneous:
 * This release now uses only version 4 API endpoints, consistent with
   the upstream deprecation of version 3 API endpoints in the 4.6
   release.
 * Startup requests are now performed concurrently to improve
   performance (#347, thanks to Abhinav Sarkar)
 * Channel header strings containing newlines are now rendered more
   effectively: newlines are converted to spaces. This behavior more
   closely matches the web client, too.

40400.0.0
=========

This release supports server version 4.4.

New features:
 * The active color theme can be customized by creating a theme
   customization file and setting the new `themeCustomizationFile`
   option. This setting lets you override any foreground color,
   background color, or style of any aspect of the Matterhorn interface.
   For details on the format of the customization file, please see the
   "Themes" help in Matterhorn with the `/help themes` command.
 * Edited posts are now displayed with a trailling "edited"
   marker. This change includes some new behavior and a new
   configuration option:
   * When you visit a post that has recent edits, the "edited" marker
     will be highlighted. This highlight can be dismissed in the same
     way as the "New Messages" cutoff, using the `M-l` keybinding.
   * This feature can be turned off using by setting the
     `showOlderEdits` configuration option to `False`.
 * New commands:
   * The `/remove-user` command removes a user from a channel.
   * The `/group-msg` command creates a new private group channel
     including several users.
   * The `/search [term]` command searches the chat history for posts
     that include the provided text and displays those posts in an
     overlay. Thanks to @abhin4v for this change!
 * Matterhorn now includes embedded hyperlinks using terminal escape
   sequences in terminals that support them. For more information on
   terminal support for hyperlink escape sequences, please see [this
   gist](https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda)
   and its associated discussion.
 * The width of the channel list (in columns) is now configurable with
   `channelListWidth`, which defaults to 20.
 * The `urlOpenCommand` can now be an interactive terminal-based
   program (such as a terminal-based web browser) but this requires
   the configuration option `urlOpenCommandIsInteractive` to be set to
   `True`. This defaults to `False` and should not be changed if the
   `urlOpenCommand` is not a terminal-based program.
 * The current selection in channel select mode can be moved forward
   and backward with `C-n` and `C-p`. (fixes #139)
 * Quotation blocks now include visible characters in addition to
   indentation.
 * We now honor the server's notification settings for channels.

Bug fixes:
 * New direct-message channels are properly added to running sessions
   (fixes #264)
 * No more reporting of "resource vanished" exceptions (fixes #116)
 * Missing editing keybindings now included in edit binding list
   (fixes #139)
 * Websocket message parse failures no longer result in crashes (fixes
   #297)
 * The sidebar no longer shows deleted users (fixes #316)
 * Tab-completion no longer includes deleted users (fixes #320)
 * User status updates are now rate-limited (fixes #282)
 * Private channels can be deleted successfully (fixes #304)
 * External commands now run in the background in their own thread and
   do not block the main UI (fixes #270)
 * Channel renaming is honored at runtime and does not require a
   restart (fixes #324)
 * Group channel show/hide preferences are observed, which in practice
   means a user can now 'leave' a several-user group channel
 * New channels will not appear twice in the sidebar (fixes #327)
 * New messages to previously-hidden group channels will cause the
   group channel to be shown again (fixes #326)

Package changes:
 * PRACTICES.md is now listed in extra-doc-files.
 * Three scripts usable with the `/sh` command are now listed in
   extra-doc-files:
   * `cowsay` runs the message text through the `cowsay` shell command
     and formats the output as a verbatim block. This command requires
     the `cowsay` command-line program to be installed externally.
   * `figlet` runs the message text through the `figlet` shell command
     and formats the output as a verbatim block. This command requires
     the `figlet` command-line program to be installed externally.
   * `rot13` runs the trivial ROT13 subsitution cipher over the
     message text and otherwise passes it through unchanged.

40000.1.0
=========

New features:
 * Messages that start with a block-level element now get laid out so
   that the block level element appears underneath, rather than to the
   right of, the user name. This helps with long usernames such as bots.
   Thanks to @kellymclaughlin for this change.
 * Code blocks with fencing now display the language when syntax
   highlighting is active.
 * In channel scroll mode, Up/Down arrow keys scroll by just one row
 * All channel and user data are now fetched more efficiently on startup
   for greatly improved startup time.

Bug fixes:
 * The `/members` command now only shows active users (fixes #315)
 * Reset edit mode after handling commands and message input, provide
   reply context when running commands (fixes #305)
 * Allow all unknown client commands to fall through to server (fixes
   #306)
 * Improve uniqueness comparisons for URL lists

40000.0.2
=========

Bug fixes:
 * Attachments no longer appear in duplicate for edited messages.

Package changes:
 * CHANGELOG.md is now listed in extra-doc-files.

40000.0.1
=========

Package changes:
 * Upgraded mattermost-api to 40000.0.1.
 * Upgraded mattermost-api-qc to 40000.0.1.

Bug fixes:
 * Ignore `emoji_added` websocket events rather than crashing due to
   JSON decode failures (#296)
 * `channel_viewed` websocket events from 4.0 servers
   lacking a `channel_id` value no longer cause a client crash
   (https://mattermost.atlassian.net/browse/PLT-7252)
 * mkrelease.sh: use correct bindist file extension (#295)

40000.0.0
=========

This release supports server version 4.0.

Package changes:
 * Upgraded mattermost-api to 40000.0.0.
 * Upgraded mattermost-api-qc to 40000.0.0.

Bug fixes:
 * Attempts to `/focus` your own DM channel no longer trigger a server
   error (fixes #294)
 * Message edits now properly restore message reactions and attachments
   (fixes #292)
 * DM channels with topics now display those topics in addition to the
   DM channel user identification.
 * Rendering of attachments regressed in 31000.0.0 and is now fixed
   so that attachments are displayed beneath, not to the right of, their
   messages.

New features:
 * Markdown image alt text is now rendered when available and Markdown
   images are now available in the C-o URL list (fixes #285)
 * Added a new configuration setting, `showBackgroundActivity`, which
   determines whether the status of Matterhorn's asynchronous work queue
   is displayed in the interface. This setting is mostly for developers
   but can be a helpful diagnostic tool. When enabled, the setting
   causes the async queue length to appear in the lower-right corner of
   the interface.
 * Added a new configuration setting,
   `unsafeUseUnauthenticatedConnection`, which causes Matterhorn to use
   a non-HTTPS connection when connecting to the configured host and
   port. As the sample configuration file says, use this only if you
   know what you are doing.

Internal changes:
 * Added support for 4.0's `channel_viewed` and `channel_updated`
   websocket events.

31000.0.0
=========

This release supports server version 3.10.0.

Package changes:
 * Upgraded mattermost-api to 31000.0.0.
 * Upgraded mattermost-api-qc to 31000.0.0.

New features and keybindings:
 * Matterhorn now supports Flagged Posts. To use this feature:
   * Press M-8 or use the /flags command to show a list of your
     flagged posts. Within that view, 'f' will unflag a selected
     message.
   * In message selection mode in channels (C-s), 'f' will flag/unflag
     the selected message.
   * Flagged posts appear with a flag marker "[!]" next to the author's
     username.
 * Syntax highlighting of fenced code blocks is now supported in
   256-color terminals. Code blocks must provide the langauge hint using
   the same syntax as supported by the web client.
 * Spell checking of user input is now supported using Aspell. To use
   this feature:
   * Install the Aspell binary and dictionaries on your system.
   * Set enableAspell = true in your Matterhorn configuration.
   * Optionally, set aspellDictionary to the name of the Aspell
     dictionary you want to use (this is only necessary if you want to
     override your LANG setting, which is typically sufficient)
   * Once user input is entered into the input area, a short delay after
     typing stops, a spell check will highlight any misspelled words.
 * Home and End now move the cursor as usual in editor while in normal
   mode, and also navigate the message list while scrolling.
 * Matterhorn now displays the mention count in the channel sidebar to
   mimic the web client behavior and highlights channels with at least
   one mention in magenta in the sidebar.

Bug fixes and improvements:
 * Configuration file values can now use quoted string syntax (fixes
   #269)
 * The channel switch mode prompt now mentions anchors.
 * The URL opening command is now run asynchronously.
 * Input history files are now saved with an appropriate file mode (fixes #218)
 * Fixed a bug that caused users created after a session began not to
   appear in the sidebar.
 * Help topics (valid arguments to /help) are now displayed in the main
   help UI (fixes #225)
 * Message deletion now also deletes replies to the deleted message
   (fixes #257)
 * On channel deletion, leave before deleting to avoid a server error.
 * URLs in the URL list are now displayed in the same order in which
   they appear in messages (fixes #250)
 * User identification now appears in DM channel headers.
 * Numbered lists now render starting at first specified number (fixes
   #243)
 * The login screen now displays validation errors for hostname and port
   inputs when appropriate (fixes #242)
 * Channel select inputs that have an exact match no longer require
   ^/$ anchors.

30802.1.0
=========

This release supports server version 3.8.2. This release is our first
official public release.

Package changes:
 * Removed stale array and data-default dependencies
 * Increased lower bound on base dependency to 4.8 to relect our GHC
   testing.

Improvements:
 * Subprocess error logs are now only created on demand. Previously we
   created them on startup before any subprocess errors had occurred.
 * We now provide a QuickCheck test for some infrastructure.

Bug fixes:
 * Fixed a bug whereby some resize operations would cause a program
   crash by triggering an unsafe mutable vector operation in Vty.
 * New user creation is now handled successfully and no longer requires
   a client restart to function reliably. Previously bad behavior
   included not coloring new users' names and not showing new users'
   names at all on their messages.
 * Message selection mode is no longer blocked by errors and other
   non-post content.
 * Subprocesses that fail to run are now consistently mentioned in the
   subprocess error log.

30802.0.0
=========

This release supports server version 3.8.2.

Package changes:
* Relax constraint on containers to allow 0.5.7 or greater.
* Upgraded mattermost-api to 30802.0.0.

Bug fixes:
* The help interface now updates properly when showing either the main
  help window or the scripts help window.
* Attachments are now saved asynchronously.
* Attachments are now opened without using the browser, thus removing
  our dependency on a valid browser session with the server (fixes #128)
* Editing state is now saved before switching history entries (fixes
  #210)
* Messages with unknown authors trigger a user metadata fetch (fixes #205)

New keybindings:
* C-o now opens the URL list while scrolling through channel messages.
* C-c now cancels channel selection in C-g mode

UI changes:
* Channel scroll mode keybindings were added to the help page.
* We no longer perform string highlighting on the contents of Markdown
  code spans in messages.
* On startup, only the scrollback for Town Square is fetched to improve
  performance. Scrollback for other channels is fetched on demand.
* We now use the channel sigil "~" rather than "#" to match the web
  client behavior, and highlight ~-prefixed channel names (fixes #204).

30701.0.0
=========

This release supports server version 3.7.1.

New features:
 * Matterhorn now has basic support for 3.7's new "group channel"
   feature. If other users add you to a group channel, it will appear in
   the sidebar with the member usernames listed (e.g. "#bob, sue, ...").
   Creation of group channels will be supported in a future release.
 * When the `urlOpenCommand` program produces output on standard output
   or standard error, this output is now logged and a message is posted
   in the current channel with the path to the log file. This feature
   prevents the URL open command from poisoning the terminal state with
   unexpected output.
 * Added a `/members` command to show current channel membership.
 * Channel header changes from other users will now cause the channel
   topic string to update.
 * Added a `/delete-channel` command to delete the current channel.
 * Markdown rendering now puts empty lines between adjacent block
   elements of the same variety to improve readability.

Bug fixes:
 * Message selection now only supports reply/edit/delete on normal posts
   (fixes #174)
 * C-n/p now only change channels when a non-DM channel is selected
   (fixes #82)
 * We now handle new user events (fixed #111)
 * On channel change we now always reset the channel list to scroll to
   the top (fixes #138)
 * When a draft message is left in the editor when changing channels, a
   new sigil ("»") appears for the previous channel instead of the usual
   "#" to indicate this.
 * The help interface now resizes properly all the time.

Performance improvements:
 * On startup, all channel contents are fetched asynchronously. Town
   Square fetches are prioritized. These changes drastically improve
   startup time, even on fast connections.

Documentation changes:
 * The README now includes a feature list, a brief feature overview for
   new users, and a section on how to contribute.

Package changes:
 * Upgraded mattermost-api to 30701.0.0.
 * Binary releases now include the copyright and licensing information
   for all dependencies.

Internal changes:
 * Added preemption support for asynchronous work queue processing.
   `doAsync(With)` now both take an `AsyncPriority`.

30600.2.4
=========

New editing keybindings:
 * C-w, M-Backspace now delete the word to the left of the cursor.
 * M-d deletes the word to the right of the cursor.
 * C-k now kills text from the cursor to the end of the line and copies
   to an internal buffer; C-y pastes from said buffer.
 * C-b, C-f move back and forward by one word, respectively.

* Many editing keybindings are now present in their own Help page
  section.

Package changes:
 * Upgraded text-zipper to 0.10.
 * Upgraded mattermost-api to 30600.2.2.

30600.2.3
=========

Bug fixes:
 * Message-yanking failures due to missing programs (e.g. xclip) no
   longer trigger an unhandled exception (fixes #170)
 * Users that are not members of the current team are now hidden from
   the user list (fixes #161)
 * User statuses in the sidebar are now periodically refreshed
 * Supported server commands now appear on the help screen (fixes #162)
 * Files containing non-UTF-8 content no longer trigger unhandled
   exceptions when being read by matterhorn (fixes #168)
 * Updated repository URL in error message about reporting problems
   (thanks @tommd)

30600.2.2
=========

* Improved formatting of message attachments so that their filenames
  are shown. Previously only their hash identifiers were displayed. This
  change also entails asynchronous fetching of attachment metadata.
* Upgraded to mattermost-api 30600.2.1.

30600.2.1
=========

* Fixed a bug that caused message selection mode to trigger a Vty crash
  when a message being rendered was larger than half of the area
  available for the message list.

30600.2.0
=========

* Added a new message selection mode. This mode provides access to four
  new features: replies, edits, deletion, and verbatim text yanking. To
  enter this mode, use the default binding of C-s (and cancel with Esc).
  The resulting cursor can be used to select messages in the current
  channel and perform the following actions on them:

  * y: for messages with any verbatim text, yank the verbatim text to
    the first verbatim block in the message to the system clipboard
    (works on Linux and OSX)
  * r: reply to the selected message. To cancel a reply, press C-c or
    Esc.
  * e: for messages written by the current user, edit the selected
    message. To cancel an edit, press C-c or Esc.
  * d: for messages written by the current user, delete the selected
    message (with a confirmation).
  * o: for messages containing URLs, open all URLs in the selected
    message.
  * j/k/up/down/pgup/pgdown: change the selected message cursor
    position.

* Added a C-r binding to begin a reply to the most recently posted
  message in the current channel.
* M-e now toggles multiline editing mode rather than just enabling it.
  Previously, Esc toggled it off.
* Upgraded to mattermost-api 30600.2.0.

30600.1.0
=========

* Upgraded to mattermost-api 30600.1.0. This fixed /me and /shrug
  server-side command execution failures.

30600.0.0
=========

Initial versioned release for server version 3.6.0.
