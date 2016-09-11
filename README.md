[![Build
Status](https://travis-ci.org/aisamanra/matterhorn.svg?branch=master)](https://travis-ci.org/aisamanra/matterhorn)

![](logo.png)

This is a basic terminal-based client for the MatterMost chat system.

# Building

The easiest way to build it is to use the provided `install.sh`
script, which requires `git` and an appropriate `ghc`/`cabal`
installation. It will pull the appropriate repos and build the sandbox
in the appropriate way.

If you want to, you can also run the install process manually.
You'll need both the `matterhorn` repo and the `mattermost-api` repo,
neither of which are currently in Hackage. Clone both of them to
an appropriate place:

~~~
$ git clone git@github.com:dagit/mattermost-api.git
$ git clone git@github.com:aisamanra/matterhorn.git
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
