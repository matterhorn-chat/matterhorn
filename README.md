[![Build
Status](https://travis-ci.org/matterhorn-chat/matterhorn.svg?branch=master)](https://travis-ci.org/matterhorn-chat/matterhorn)

![](logo.png)

This is a basic terminal-based client for the MatterMost chat system.

![](screenshots/screenshot-00.png)

# Binary Releases

We provide pre-built binary releases for some platforms. Please see the
release list to download a binary release for your platform:

https://github.com/matterhorn-chat/matterhorn/releases

(If your platform is not listed, you may need to build from source if
we don't have the resources to ship a release for your platform. See
the Building section below.)

Run the `matterhorn` binary in the release archive. You'll be prompted
for your server information. At present `matterhorn` supports only
username/password authentication.

# Configuring

Right now, configuration is manual. An example configuration file
can be found at `sample-config.ini`. Matterhorn will prefer
`config.ini` in the current working directory, but will look in
the typical XDG configuration directories (you'll probably want to
use `$HOME/.config/matterhorn/config.ini`) and as a last resort look
for a globally-accessible `/etc/matterhorn/config.ini`.

# Using

You can often use built-in keybindings or `/cmd`-style commands. To
see available keybindings and commands, press `F1` or run the `/help`
command.

# Building

The easiest way to build `matterhorn` is to use the provided
`install.sh` script, which requires `git` and an appropriate
`ghc`/`cabal` installation. It will pull the appropriate repos and build
the sandbox in the appropriate way.

If you want to, you can also run the install process manually.
You'll need both the `matterhorn` repo and the `mattermost-api` repo,
neither of which are currently in Hackage. Clone both of them to
an appropriate place:

~~~
$ git clone git@github.com:matterhorn-chat/mattermost-api.git
$ git clone git@github.com:matterhorn-chat/matterhorn.git
~~~

Move into the `matterhorn` directory, create a new sandbox, add
the `mattermost-api` package as an extra dependency, install the
dependencies, and build the package:

~~~
$ cd matterhorn
$ cabal sandbox init
$ cabal sandbox add-source ../mattermost-api
$ cabal install
$ cabal build
~~~

# Contributing

If you decide to contribute, that's great! Here are some guidelines you
should consider to make submitting patches easier for all concerned:

 - If you want to take on big things, let's have a design/vision
   discussion before you start coding. Create a GitHub issue and we can
   use that as the place to hash things out. We'll be interested to
   discuss any usability / UI, performance, or compatibility issues.
 - Please make changes consistent with the conventions already used in
   the codebase.
