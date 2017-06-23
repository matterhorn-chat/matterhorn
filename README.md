[![Build
Status](https://travis-ci.org/matterhorn-chat/matterhorn.svg?branch=master)](https://travis-ci.org/matterhorn-chat/matterhorn)

![](logo.png)

Matterhorn is a terminal client for the Mattermost chat system.

![](screenshots/screenshot-00.png)

# Quick Start

We provide pre-built binary releases for some platforms. Please see the
release list to download a binary release for your platform that matches
your server version:

https://github.com/matterhorn-chat/matterhorn/releases

To fetch a release and run Matterhorn, run the following commands (where
`VERSION` and `PLATFORM` match your setup):

    wget https://github.com/matterhorn-chat/matterhorn/releases/download/<VERSION>/matterhorn-<VERSION>-<PLATFORM>.tar.gz
    tar xf matterhorn-<VERSION>-<PLATFORM>.tar.gz
    cd matterhorn-<VERSION>-<PLATFORM>
    ./matterhorn

When you run Matterhorn you'll be prompted for your server information
and credentials. At present `matterhorn` supports only username/password
authentication.

Note: Version `ABBCC.X.Y` matches Mattermost server version `A.BB.CC`.
For example, if your Mattermost server version is `3.6.0` then you
would download matterhorn version `30600.2.4`. See [Our Versioning
Scheme](#our-versioning-scheme) for details.

# Configuring

For configuration options you have two choices:

* Interactive configuration entered on each program run
* Configuration via stored settings in a config file

The first option is useful when trying out the program because you can
get up and running without worrying about making a configuration. Once
you're ready to make your settings persistent, they can be added to
a configuration file. An example configuration file can be found at
`sample-config.ini`. Any settings omitted from the configuration will be
obtained interactively at startup.

When looking for configuration files, matterhorn will prefer
`config.ini` in the current working directory, but will look in the
typical XDG configuration directories (you'll probably want to use
`$HOME/.config/matterhorn/config.ini`) and as a last resort look for a
globally-accessible `/etc/matterhorn/config.ini`.

# Using the Client

The user interface has three main areas:

* Left: list of channels you're in, and list of users in your team and
  their statuses (`+` means online, `-` means away, and an absent sigil
  means offline)
* Right: messages in the current channel
* Bottom: editing area for writing, editing, and replying to messages

You can use built-in keybindings or `/cmd`-style commands to operate
the client. To see available keybindings and commands, use the default
binding of `F1` or run the `/help` command.

To join a channel, use the `/join` command to choose from a list of
available channels. To create a channel, use `/create-channel`. To leave
a channel, use `/leave-channel`.

To see the members in the current channel, use the `/members` command.

To send a message, type it into the editor and press Enter to send.
To send a multi-line message, toggle multi-line mode with the default
binding `M-e`. Markdown syntax is accepted.

To edit your current message in an external editor (`$EDITOR`), use the
default binding of `M-k`.

To preview the message you're about to send (e.g. to check on how your
Markdown syntax will be rendered), toggle preview mode with the default
binding `M-p`.

To change channels, use `/focus` or one of the default bindings `C-n`
(next channel), `C-p` (previous channel), `C-g` (fast channel switch).

To directly message another user, use `/focus` or `C-g`.

`C-g` channel switching mode does a substring match of the input text on
the channel and usernames; metacharacters `^` and `$` at the beginning
or end of input, respectively, anchor the match in case of multiple
matches.

To switch to the channel you were in prior to the current channel, use
the default binding `M-s` (swap). The most recent channel is marked in
the channel list with a "`<`" indicator.

To switch to the next channel with unread messages, use the default
binding `M-a`.

To quickly show a list of URLs mentioned in the current channel and then
open one in your local browser, use the default binding of `C-o` and
configure the `urlOpenCommand` configuration setting.

To edit, delete, or reply to a message, select a message with the
default binding of `C-s`. Use the default binding of `C-c` to cancel
these operations.

# Features

* Channel creation, deletion, and membership management commands
* Optimized channel-switching modes: `M-a`, `M-s`, and `C-g`
* Message posting, editing, replying, and deletion
* Markdown rendering
* Convenient URL-opening with local browser
* Secure password entry via external command (e.g. OSX keychain)
* Yank verbatim content from messages into the system clipboard
* Preview message rendering before sending
* Optional smart quoting for efficient Markdown entry
* Edit messages with `$EDITOR`
* Message editor with kill/yank buffer and readline-style keybindings
* Tab-completion of usernames, channel names, and commands

# Building

The easiest way to build `matterhorn` is to use the provided
`install.sh` script, which requires `git` and an appropriate
`ghc`/`cabal` installation. It will pull the appropriate repos and build
the application.

If you want to, you can also run the install process manually:

~~~
$ cd matterhorn
$ cabal new-build
$ ./run.sh
~~~

# Our Versioning Scheme

Matterhorn version strings will be of the form `ABBCC.X.Y` where ABBCC
corresponds to the Mattermost server version supported by the release.
For example, if a release supports Mattermost server version 1.2.3, the
ABBCC portion of the `matterhorn` version will be `10203`. The `X.Y`
portion of the version corresponds to our own version namespace for the
package. If the server version changes, `X.Y` SHOULD be `0.0`. Otherwise
the first component should increment if the package undergoes major code
changes or functionality changes. The second component alone should
change only if the package undergoes security fixes or other bug fixes.

# Contributing

If you decide to contribute, that's great! Here are some guidelines you
should consider to make submitting patches easier for all concerned:

 - If you want to take on big things, let's have a design/vision
   discussion before you start coding. Create a GitHub issue and we can
   use that as the place to hash things out. We'll be interested to
   discuss any usability / UI, performance, or compatibility issues.
 - Please make changes consistent with the conventions already used in
   the codebase.

# Frequently Asked Questions

* Q: Does matterhorn support Gitlab authentication?
* A: No. But we would be happy to work with contributors who are
  interested in investigating what this would take and/or implementing
  it.  See the Contributing section for details.
