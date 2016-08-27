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
the `mattermost-api` package as an extra dependency, and build the
package:

~~~
$ cd matterhorn
$ cabal sandbox init
$ cabal add-source ../mattermost-api/mattermost-api.cabal
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

You can often use built-in keybindings or `/cmd`-style commands:

~~~
Command		Keybinding		Effect
-------		----------		-------------------------------
/right		Ctrl-right		Go to the next chat channel
/left		Ctrl-left		Go to the previous chat channel
/quit		Esc				Quit the editor
/chan [c]					Go to channel c
/dm [u]						Go to direct message channel with user u
/help						Show a help dialogue
~~~
