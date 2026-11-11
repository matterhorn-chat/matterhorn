Matterhorn Build Instructions
-----------------------------

This document describes how to build Matterhorn from source. If you're
looking specifically for instructions on making a release build, see
`docs/RELEASES.md`.

Step 1: Obtain the source
=========================

To build Matterhorn from source, obtain the source in one of two ways:

1. Clone the `matterhorn` repository: `git clone --recurse-submodules git@github.com:matterhorn-chat/matterhorn.git`
2. Fetch the latest `.tar.gz` source from GitHub

The repository comes configured with submodules that use SSH URLs to
fetch from GitHub. If you need to use HTTPS URLs (e.g. if you don't have
an SSH key present for GitHub access or don't have a GitHub account),
you'll need to edit the `.gitmodules` file and convert the submodule
URLs to HTTPS. Then run `git submodule sync`.

Step 2: Install the Haskell toolchain
=====================================

1. Install [ghcup](https://www.haskell.org/ghcup/)
2. Install GHC with `ghcup install ghc`
3. Install Cabal with `ghcup install cabal`
4. Add `~/.ghcup/bin` to your `PATH`
5. If this is your first installation of GHC on the system, you'll need
   to run `ghcup set ghc <version>` (e.g. `ghcup set ghc 9.10`) to set
   the active GHC version.
5. Run `cabal update` to get the latest package database metadata

For Ubuntu systems in particular:

* Install the `zlib1g-dev` package

Building from the repository
----------------------------

1. Obtain the source code by cloning the Matterhorn repository
2. Switch to the `develop` branch
2. Perform a build with `./build.sh`
2. Run the built Matterhorn binary with `./run.sh`

Building from a source tarball
------------------------------

1. Unpack the latest `.tar.gz` release from GitHub.
2. `cd matterhorn-<VERSION>`
3. `rm cabal.project`
4. Continue with the [repository build instructions](#building-from-the-repository)
