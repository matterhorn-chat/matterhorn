
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

Initial versioned release for server verison 3.6.0.
