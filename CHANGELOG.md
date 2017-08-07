
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
