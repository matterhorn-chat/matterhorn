
Notification Scripts
====================

When new post notifications are enabled in user or server preferences,
Matterhorn supports invoking an external program to deliver system
notifications. Example notification scripts are provided in the
`notification-scripts` directory.

The interface for notification scripts is as follows:

* Notification scripts are expected to return immediately. The longer
  the script takes to run, the longer Matterhorn will wait on the script
  to finish, which will cause Matterhorn to delay looking at incoming
  network messages, etc.

* The notifier behavior changes according to the activityNotifyVersion
  setting in the configuration file.

* NotifyV1: Matterhorn will invoke the command with three arguments:
  * The mention argument, whose values are:
    * "1" - this value indicates that the user running Matterhorn was
      mentioned in the message body.
    * "2" - this value indicates that the user running Matterhorn was
      not mentioned in the message body.
  * The sender username argument: the username of the user that sent the
    message.
  * The message body, sanitized of tabs (converted to spaces) and escape
    characters (converted to "<ESC>").

* NotifyV2: Matterhorn will invoke the command with zero arguments,
  passing notification details via JSON to the command's standard input.
  * NotifyV2 JSON payload fields:
    * "version": "2"
    * "from": "sender name"
    * "message": "message text..."
    * "mention": boolean (true or false: did sender mention me?)

* Matterhorn will wait for the process to terminate. If the process
  emits any output to standard out OR if the command exits with a
  non-zero exit status, Matterhorn will consider that evidence that
  the command has failed and will display an error to the user in the
  current channel. The standard out and standard error output will be
  logged to a temporary subprocess log file and the log file's path will
  be included in the error message.
