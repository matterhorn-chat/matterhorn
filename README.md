[![Hackage](https://img.shields.io/hackage/v/matterhorn.svg)](https://hackage.haskell.org/package/matterhorn)

![](logo.png)

Matterhorn is a terminal client for the Mattermost chat system.

![](screenshots/screenshot-00.png)

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
into the Server URL box and enter your credentials. See the [Matterhorn
User Guide](docs/UserGuide.md) on the details for providing each kind of
supported credentials.

Note: Version `ABBCC.X.Y` matches Mattermost server version `A.BB.CC`.
For example, if your Mattermost server version is `3.6.0` then you
would download matterhorn version `30600.2.4`. See [Our Versioning
Scheme](#our-versioning-scheme) for details.

## Installation Requirements

For most of our binary releases, no additional packages need to be
installed; they should just work out of the box. But here are some
additional requirements that may apply for your platform:

* CentOS Steam 8
  * The `ncurses-compat-libs` package must be installed.

## Other Ways to Install: Third-Party Snap Package

@3v1n0 maintains a Snap package here:

https://github.com/3v1n0/matterhorn-snap/ ([Snapcraft page](https://snapcraft.io/matterhorn/))

# Get Help!

We provide a number of avenues for getting support:

* [Frequently asked questions](docs/FAQ.md)
* [Matterhorn User Guide](docs/UserGuide.md)
* Built-in help (`/help`)
* [Command list](docs/commands.md)
* [Keybinding list](docs/keybindings.md)
* Chat with the developers on [the Mattermost Community server](https://community.mattermost.com/core/channels/matterhorn)
* Get notified about new Matterhorn releases by following
  [our Twitter account](https://twitter.com/matterhorn_chat)

# Features

* Channel creation, deletion, and membership management commands
* Support for multiple teams
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
* Support for channel muting
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
* Optional mouse support

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
   skills needed](docs/HASKELL.md).
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
   [our practices document](docs/PRACTICES.md) for more information.

# Building

If you just want to run Matterhorn, we strongly suggest running a binary
release (see above). *Building from source is only recommended if you
intend to contribute.*

If you want to contribute changes to Matterhorn, you'll need to build
it from source. See [our building instructions](docs/BUILDING.md) for
details.
