[![Hackage](https://img.shields.io/hackage/v/matterhorn.svg)](https://hackage.haskell.org/package/matterhorn) [![Build
Status](https://travis-ci.org/matterhorn-chat/matterhorn.svg?branch=master)](https://travis-ci.org/matterhorn-chat/matterhorn)

![](logo.png)

Matterhorn is a terminal client for the Mattermost chat system.

![](screenshots/screenshot-00.png)

# New Release Notifications

Get notified about new Matterhorn releases by following our Twitter account!

[https://twitter.com/matterhorn_chat](https://twitter.com/matterhorn_chat)

# Chat With the Developers

The Matterhorn developers hang out on the official Mattermost
community server. Stop by to get support and say hello!

[https://community.mattermost.com/core/channels/matterhorn](https://community.mattermost.com/core/channels/matterhorn)

# Quick Start

We provide pre-built binary releases for some platforms. Please see the
release list to download a binary release for your platform that matches
your server version:

https://github.com/matterhorn-chat/matterhorn/releases

To run Matterhorn, unpack the binary release archive and run the
`matterhorn` binary within.  Help is available via the `--help` or
`-h` flag.

    $ matterhorn --help
    $ matterhorn

When you run Matterhorn you'll be prompted for your server URL and
credentials. To connect, just paste your web client's Mattermost URL
into the Server URL box and enter your credentials. See the next section
on the details for providing each kind of supported credentials.

Note: Version `ABBCC.X.Y` matches Mattermost server version `A.BB.CC`.
For example, if your Mattermost server version is `3.6.0` then you
would download matterhorn version `30600.2.4`. See [Our Versioning
Scheme](#our-versioning-scheme) for details.

# Authentication

`matterhorn` supports username/password authentication as well as
authentication by Personal Access Token or session token. The following
subsections provide details on using each method.

## Username / Password Authentication

Matterhorn's start-up login user interface will allow you to provide
a username and password for authentication. We recommend that you
put the username in your Matterhorn configuration file and configure
Matterhorn to obtain your password from your system keychain. See the
`docs/sample-config.ini` file for documentation on the `user` and
`passcmd` settings, respectively.

## Personal Access Token Authentication

Matterhorn's start-up login user interface will also allow you to
provide an session or Personal Access Token. If a token is provided, it
will take precedence over a username and password.

We recommend Personal Access Tokens because they don't expire except
when revoked. To use a Personal Access Token:

* Ask your Mattermost server administrator to enable Personal Access
  Tokens on your Mattermost account.
* Create a Personal Access Token.
* Add the new token to your system keychain. (Details on how to do this
  depend on your platform.)
* Configure Matterhorn's `tokencmd` configuration option to query the
  system keychain to get the token. See `docs/sample-config.ini` for
  examples of how to do this with `passcmd` and `tokencmd`.

The steps for configuring Personal Access Tokens can be found in [the
Mattermost documentation](https://docs.mattermost.com/developer/personal-access-tokens.html).

## Token Cookie Authentication

If your server does not support username and password authentication
(e.g. if GitLab authentication is the only supported method), you can
use your browser client's token cookie to authenticate:

* Authenticate to GitLab (or other identity provider) using the
  Mattermost web client.
* Once your browser has returned to the Mattermost interace, obtain the
  value of the `MMAUTHTOKEN` browser cookie.
* Start Matterhorn and enter the value of the `MMAUTHTOKEN` token in the
  login interface field labeled `Access token:` or add the token to the
  system keychain as described above.

Please note that unlike a Personal Access Token, the web client's token
cookie will expire, so these steps will need to be repeated each time
you log in with Matterhorn. As a result, we strongly recommend the use
of a Personal Access Token instead if at all possible.

Firefox users: you might consider using the
[mattermost-session-cookie-firefox](https://github.com/ftilde/mattermost-session-cookie-firefox)
script to make this process easier.

# Configuring

For configuration options you have two choices:

* Interactive configuration entered on each program run
* Configuration via stored settings in a config file

The first option is useful when trying out the program because you can
get up and running without worrying about making a configuration. Once
you're ready to make your settings persistent, they can be added to
a configuration file. An example configuration file can be found at
`docs/sample-config.ini`. Any settings omitted from the configuration
will be obtained interactively at startup.

When looking for configuration files, matterhorn will prefer
`config.ini` in the current working directory, but will look in the
typical XDG configuration directories (you'll probably want to use
`$HOME/.config/matterhorn/config.ini`) and as a last resort look for a
globally-accessible `/etc/matterhorn/config.ini`.

# Using the Client

The user interface has three main areas:

* Left: list of channels you're in, and list of users in your team and
  their statuses (`+` means online, `-` means away, `×` means Do Not
  Disturb, and an absent sigil means offline)
* Right: messages in the current channel
* Bottom: editing area for writing, editing, and replying to messages

You can use built-in keybindings or `/cmd`-style commands to operate the
client. Keybinding and command help may be obtained in a number of ways:

* The `/help` command within Matterhorn.
* The `F1` key within Matterhorn.
* Running matterhorn with the `-k` or `-m` arguments to get a text table of available keybindings or commands respectively.
* Running matterhorn with the `-k` or `-m` arguments with `-f markdown` to get a [Markdown table of available keybindings](docs/keybindings.md) or [Markdown table of available commands](docs/commands.md).

    Note: The keyboard and command output commands do not start the
    client but simply print to stdout and exit. The keybindings shown
    include any user overrides from the config files; use the `-i`
    flag to skip loading the local config files and see the Matterhorn
    default keybindings.

Keybindings may include modifiers such as Control (indicated with a
`C-` prefix) or Meta (indicated with a `M-` prefix). If your keyboard
has an `Alt` key, that will work as `Meta`. If it does not, you may be
able to configure your terminal to provide `Meta` via other means
(e.g. iTerm2 on OS X can be configured to make the left Option key
work as Meta). Keybindings can be customized in the configuration
file; see `/help keybindings` for details.

To join a channel, use the `/join` command to choose from a list of
available channels. To create a channel, use `/create-channel`. To leave
a channel, use `/leave-channel`.

To create a private group chat amongst yourself and other users, use the
`/group-msg` command, e.g., `/group-msg user1 user2`.

To see the members in the current channel, use the `/members` command.

To send a message, type it into the editor and press Enter to send.
To send a multi-line message, toggle multi-line mode with the default
binding `M-e`. Markdown syntax is accepted.

To edit your current message in an external editor (`$EDITOR`), use the
default binding of `M-k`.

To see a live preview of the message you're about to send while you
compose it (e.g. to check on how your Markdown syntax will be rendered),
toggle preview mode with the default binding `M-p`.

To change channels, use `/focus` or one of the default bindings `C-n`
(next channel), `C-p` (previous channel), `C-g` (fast channel switch).

To directly message another user, use `/focus` or `C-g`.

`C-g` channel switching mode does a substring match of the input text on
the channel and usernames; metacharacters `^` and `$` at the beginning
or end of input, respectively, anchor the match in case of multiple
matches. The cursor in this mode is usable with `C-n` and `C-p`.

To switch to the channel you were in prior to the current channel, use
the default binding `M-s` (swap). The most recent channel is marked in
the channel list with a "`<`" indicator.

To switch to the next channel with unread messages, use the default
binding `M-a`. Pressing `M-a` repeatedly will visit other channels with
unread messages, and the channel you started in when first pressing
`M-a` will be marked with `~` in the sidebar. Pressing `M-a` will return
to that channel after other channels with unread messages have been
visited.

To quickly show a list of URLs mentioned in the current channel and then
open one in your local browser, use the default binding of `C-o` and
configure the `urlOpenCommand` configuration setting.

To edit, delete, flag, or reply to a message, select a message with
the default binding of `C-s`. Use the default binding of `C-c` to
cancel these operations.

Messages that have been flagged can be viewed with either the `/flags`
command or `M-8`. This view allows you to select and unflag particular
messages, as well.

To enable spell-checking in the message editor, install Aspell and set
`enableAspell` to `True` in your configuration. To override Aspell's
choice of master dictionary, set the `aspellDictionary` option to the
name of the dictionary you'd like to use.

To attach a file to the post being edited, use the default binding of
`C-x`. The window that appears will let you browse the filesystem to
find a file to attach. In this window, `o` opens the selected file with
your URL open command to let you preview your choice, `Enter` enters the
selected directory or selects the current file for attachment, and arrow
keys change selection. Once you've attached a file, you'll see the text
`(1 attachment)` above your message editor. You can attach additional
files or remove existing attachments by pressing `C-x` again.

# Features

* Channel creation, deletion, and membership management commands
* Optimized channel-switching modes: `M-a`, `M-s`, and `C-g`
* Message posting, editing, replying, and deletion
* Markdown rendering
* Convenient URL-opening with local browser
* Secure password entry via external command (e.g. OSX keychain)
* Secure authentication token entry via external command (e.g. OSX
  keychain)
* Yank verbatim content from messages into the system clipboard
* Optional live preview during message editing
* Optional smart quoting for efficient Markdown entry
* Edit messages with `$EDITOR`
* Rebindable keys (see `/help keybindings`)
* Message editor with kill/yank buffer and readline-style keybindings
* Support for adding and removing emoji post reactions
* Tab-completion of:
  * Usernames: type `@`, then `Tab` to cycle through matches
  * Channel names: type `~`, then `Tab` to cycle through matches
  * Commands: type `/`, then `Tab` to cycle through matches
  * Emoji: type `:` and then some text, then `Tab` to display and cycle
    through matches
  * Fenced code block languages: type three backticks to begin typing a
    code block, then `Tab` to cycle through available languages
* Support for attachment upload and download
* Spell-checking via Aspell
* Syntax highlighting of fenced code blocks in messages (works best in
  256-color terminals)
* Flagging and unflagging of posts, which are then viewable with `M-8`
  or `/flags`
* Support for SOCKS 4 and 5 proxies via the `ALL_PROXY` and
  `HTTPS_PROXY` environment variables. (Plain HTTP proxies are not yet
  supported.) Also supports `NO_PROXY`.
* Multiple color themes with color theme customization support
* Custom notifications via notification scripts (see the
  `activityNotifyCommand` configuration setting and
  `docs/notification-scripts.md` for details).

# Spell Checking Support

Matterhorn uses `aspell` to perform spell-checking of your message
input. To use this feature:

 * Install `aspell` and ensure that your installation includes
   dictionaries corresponding to your `LANG` setting. To check this, ask
   `aspell` to check some input:
   ```
   $ echo stuff | aspell -a
   Error: No word lists can be found for the language "en".
   $ echo $LANG
   en_US
   ```
   If Aspell succeeds, the output will look like this:
   ```
   @(#) International Ispell Version 3.1.20 (but really Aspell 0.60.6.1)
   *
   ```
 * Set `enableAspell` to `True` in your `config.ini`
 * Enter any message input in the message editor in `matterhorn`. After
   a short delay after you stop typing, misspelled words will turn red.

# Emoji

Matterhorn loads its emoji list (for auto-completion of `:...:`) at
startup. It looks for an `emoji.json` file in the following locations
and in the following order:

* `~/.config/matterhorn/emoji.json`. This option is to support emoji
  list installation and customization.
* `MHBIN/emoji/emoji.json`, where `MHBIN` is the directory containing
  the `matterhorn` binary. This option is to make it convenient to use
  emoji with `matterhorn` from releases.

The `emoji.json` is included in the Matterhorn releases
and is obtained from [the Mattermost web client source
tree](https://github.com/mattermost/mattermost-webapp/blob/master/utils/emoji.json).

# Our Versioning Scheme

Matterhorn version strings will be of the form `ABBCC.X.Y` where ABBCC
corresponds to the lowest Mattermost server version expected to be
supported by the release.  For example, if a release supports
Mattermost server version 1.2.3, the ABBCC portion of the `matterhorn`
version will be `10203`.  There may be later versions of the
Mattermost server that are supported (e.g. Matterhorn 50200.X.Y
supports Mattermost server versions 5.2 through at least 5.8).

The `X.Y` portion of the version corresponds to our own version
namespace for the package. If the server version changes, `X.Y` SHOULD
be `0.0`. Otherwise the first component should increment if the
package undergoes major code changes or functionality changes. The
second component alone should change only if the package undergoes
security fixes or other bug fixes.

# Our Design Philosophy

Overall, we strive to build a terminal client that provides the same
basic feature set as the web client. This is reflected in the state
of the client, our issue backlog, and the content of our wiki feature
design discussions.

We intend to add web client features to Matterhorn to the extent that
they can be added sensibly in a terminal setting. Our goal is to do
so in a way that minimizes surprise to web client users migrating to
Matterhorn while also providing the best terminal user experience that
we can think of. That might entail adding the web client features but
changing their designs to ones better suited for terminal use or it
might mean omitting aspects of web client features that rely heavily on
mouse- or DOM-related UI idioms. It might also entail adding web client
features but deviating slightly on specific behaviors.

If you are used to a web client feature and don't see it in Matterhorn,
that's probably because we just haven't gotten to it yet. We would
be happy to hear from people wanting to contribute! If you can't
contribute, search existing issues to see if we already have an issue
for it, or create a new issue and let us know!

# Contributing

If you decide to contribute, that's great! Here are some guidelines you
should consider to make submitting patches easier for all concerned:

 - If you are new to Haskell and are unsure how much Haskell you need
   to know in order to contribute, please see [our list of Haskell
   skills needed](HASKELL.md).
 - Please base all patches against the `develop` branch unless you are
   specifically fixing a bug in a released version, in which case
   `master` is a fine place to start. Please also do this for submodules
   that have a `develop` branch if you need to contribute changes to
   submodules.
 - If you want to take on big things, let's have a design/vision
   discussion before you start coding. Create a GitHub issue and we can
   use that as the place to hash things out. We'll be interested to
   discuss any usability / UI, performance, or compatibility issues.
 - Please make changes consistent with the conventions already used in
   the codebase.
 - We follow a few development practices to support our project and it
   helps when contributors are aware of these. Please see
   `docs/PRACTICES.md` for more information.

## Building

If you just want to run Matterhorn, we strongly suggest running a binary
release (see above). *Building from source is only recommended if you
intend to contribute.*

If you want to contribute changes to Matterhorn, you'll need to build
it from source. To do that you'll need an appropriate `ghc`/`cabal`
installation (see the latest Travis-CI builds for tested versions).
You'll also need a GitHub account, since our Git submodules are set up
to use SSH with GitHub. (But that should be fine since you'll need a
GitHub account to contribute anyway.)

`matterhorn` is built by running the following commands:

```
$ git pull
$ git submodule update --init
$ ./build.sh
```

# Frequently Asked Questions

## Does matterhorn support Gitlab authentication?

Matterhorn supports GitLab authentication indirectly. In order to use
Matterhorn with GitLab authentication, see the Authentication section on
Matterhorn details for use of session tokens.

## Does matterhorn support Mattermost Personal Access Tokens?

Yes. See the Authentication section above.

## How can I get Matterhorn to render emphasized Markdown text with an italic font?

In `~/.config/matterhorn/theme.ini`,
```
[other]
markdownEmph.style = [italic]
```
and in `~/.config/matterhorn/config.ini`,
```
themeCustomizationFile: theme.ini
```

This is known to work on `gnome-terminal` version `3.32.2`, VTE version
`0.56.3`; it may work for you, too. Many terminal emulators do not
support italics at all or without various hacks. Let us know what works
for you!

## I enabled italicized text in my theme configuration. Why doesn't it work?

Most terminfo files for typical terminal configurations do not provide
support for italicized text. If your terminal emulator supports italics,
you must enable it in your terminfo database in order to use it in
Matterhorn. For more information, see these links:

* http://www.nerdyweekly.com/posts/enable-italic-text-vim-tmux-gnome-terminal/
* https://medium.com/@dubistkomisch/how-to-actually-get-italics-and-true-colour-to-work-in-iterm-tmux-vim-9ebe55ebc2be
* https://github.com/tmux/tmux/blob/2.1/FAQ#L355-L383

## I am seeing malformed characters or display corruption when I run matterhorn in my terminal. What could be causing this?

Some terminal emulators cannot handle the extra escaping that occurs
when the URL hyperlinking mode is enabled. Try setting `hyperlinkUrls =
False` in your `config.ini` file.

## Does Matterhorn support graphical emoji?

At present Matterhorn does not reliably support graphical emoji due to
the lack of consistent support for wide Unicode characters in various
terminal emulators. Results may vary, and use of emoji characters may
cause terminal rendering issues depending on the terminal emulator in
use.

## I'm running Matterhorn in Tmux. How can I paste tmux buffers into Matterhorn's editor?

By default, `tmux`'s `paste-buffer` binding, `prefix-]`, pastes buffer
text by replaying it as terminal input. This will cause unwanted
behavior when Matterhorn receives that text and sends out each input
line as a separate message. But `tmux` supports bracketed paste mode
to make a `tmux` paste a block of text as *one* Matterhorn message.
It can be enabled by changing the behavior of `prefix-]` in the Tmux
configuration:

```
unbind-key -T prefix ]
bind-key -T prefix ] paste-buffer -p
```

## I'm using Mattermost through an Nginx proxy and I keep getting disconnected.

You might need to adjust your Nginx proxy settings.
For context, see [this potentially related report](https://github.com/matterhorn-chat/matterhorn/issues/578).
