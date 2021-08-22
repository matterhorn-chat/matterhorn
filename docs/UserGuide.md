
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

* Left: list of channels you're in, and list of users in your
  currently-selected team and their statuses (`+` means online, `-`
  means away, `×` means Do Not Disturb, and an absent sigil means
  offline)
* Right: messages in the current channel
* Bottom: editing area for writing, editing, and replying to messages

You can use built-in keybindings or `/cmd`-style commands to operate the
client. Keybinding and command help may be obtained in a number of ways:

* The `/help` command within Matterhorn.
* The `F1` key within Matterhorn.
* Running matterhorn with the `-k` or `-m` arguments to get a text table of available keybindings or commands respectively.
* Running matterhorn with the `-k` or `-m` arguments with `-f markdown` to get a [Markdown table of available keybindings](keybindings.md) or [Markdown table of available commands](commands.md).

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

If you are a member of more than one team on your server, your teams
will be listed at the top of the screen. You can change which team is
selected by pressing the default bindings of `C-Left` and `C-Right`. You
can rearrange the team order by using the commands `/move-team-left` and
`/move-team-right`.

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

To mute or unmute a channel, use the server command `/mute` in that
channel. When muted, a channel will appear with `(m)` next to it in the
sidebar.

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

# Mouse Support

Matterhorn supports mouse interaction with some UI elements. To enable
mouse support, set `enableMouseMode` to `True` in your Matterhorn
configuration.

Mouse interaction is supported on the following user interface elements:

* Channel list entries can be clicked to switch channels.

* Channel list entries in channel selection mode (`C-g`) can be clicked
to switch to the selected channel match.

* Team names in the team list can be clicked to switch teams.

* URLs and post links in the URL list (`C-o`) can be clicked to open
them.

* URLs in messages can be clicked to open them using the configured URL
opener.

* Post links in messages can be clicked to switch to the channel
containing the post.

* Attachments in posts can be clicked to open them with the configured
URL opener.

* Usernames in messages can be clicked to switch to the direct message
channel for the clicked user.

* Usernames in the "Reactions" tab of the message view window can be
clicked to switch to the direct message channel for the clicked user.

* Reactions to messages can be toggled by clicking on them.
Click-toggling also works in the "Search Emoji" window as well as the
"Reactions" tab of the message view window.

* Settings in the `/notify-prefs` window can be manipulated with the
mouse.

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
