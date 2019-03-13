50200.2.0
=========

New features:
 * Channel scrolling mode has been merged with message select mode.
   There are no longer two separate modes that the user must switched
   between for fetching additional messages and operating on displayed
   messages.
   * As part of this change, gaps in the local client history where
     the server may contain additional messages are displayed as
     special entries in the message history, along with specific key
     commands that can be used to fetch additional messages at that
     point.
 * The `/msg` command now takes optional user and message arguments to
   make it faster and easier to initiate discussions with specific
   users: `/msg [@user [message]]`.  This resolves enhancement issue
   #471.
 * If smart editing is enabled and a triple backtick is typed at the
   start of a line, input automatically switches to multi-line mode.
   Fixes issue #467.
 * Status is now tracked on a per-channel basis.  Previously some
   things (like incomplete message entries) were global, and others
   were simply reset when moving away from and back to a channel.  In
   this version, much of this state is automatically saved and
   restored on a per-channel basis:
   * Incomplete message (editor) entries.
   * Multi-line mode for incomplete message entries.
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
 * Additional information on managing attachments (relative to issue #485).

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
