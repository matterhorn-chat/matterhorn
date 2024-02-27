Matterhorn Build Instructions
-----------------------------

To build Matterhorn from source, there are two options:

* Clone the repo and follow the "Build from repo" instructions below, or

* Fetch the latest `.tar.gz` source from GitHub and follow the "Build
from tarball" instructions below.

In either case, first follow these steps:

1. Install [ghcup](https://www.haskell.org/ghcup/).
2. Install GHC with `ghcup install`.
3. Install Cabal with `ghcup install-cabal`.
4. Add `~/.ghcup/bin` to your `PATH`.
5. Run `cabal new-update`

Setting Up
==========

On Ubuntu systems, you may need to first install the `zlib1g-dev`
package.

Build from repo
===============

Before building Matterhorn from source, you'll need a GitHub account. By
default, a GitHub account is necessary to fetch the submodule sources.
(If you need to do a source build but don't have an SSH key present for
GitHub access, you'll need to change the submodule URLs; see below.)

1. Clone the `matterhorn` source: `git clone git@github.com:matterhorn-chat/matterhorn.git`
2. `cd matterhorn`
3. Fetch submodules with `git submodule update --init`
4. Do a local build with `./build.sh` or build a release with `./scripts/local-mkrelease.sh`.
5. Local builds can be run with `./run.sh`; to run a binary release,
just unpack the archive and run the `matterhorn` binary found therein.

Changing the submodule URLs
---------------------------

The repository comes configured with submodules that use SSH URLs to
fetch from GitHub. If you need to use HTTPS URLs (e.g. if you don't have
an SSH key present for GitHub access or don't have a GitHub account),
you'll need to edit the `.gitmodules` file and convert the submodule
URLs to HTTPS. Then run `git submodule sync`.

Build from tarball
==================

1. Unpack the latest `.tar.gz` release from GitHub.
2. `cd matterhorn-<VERSION>`
3. `rm cabal.project`
4. Do a local build with `./build.sh` or build a release with `./scripts/local-mkrelease.sh`.
5. Local builds can be run with `./run.sh`; to run a binary release,
just unpack the archive and run the `matterhorn` binary found therein.
