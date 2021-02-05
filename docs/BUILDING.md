Matterhorn Build Instructions
-----------------------------

Before building Matterhorn from source, you'll need a GitHub account. A
GitHub account is necessary to fetch the submodule sources.

Setting Up
==========

On Ubuntu systems, you may need to first install the `zlib1g-dev`
package.

Building
========

1. Install [ghcup](https://www.haskell.org/ghcup/).
2. Install GHC with `ghcup install`.
3. Install Cabal with `ghcup install-cabal`.
4. Add `~/.ghcup/bin` to your `PATH`.
5. Run `cabal new-update`
6. Clone the `matterhorn` source: `git clone git@github.com:matterhorn-chat/matterhorn.git`
7. `cd matterhorn`
8. Fetch submodules with `git submodule update --init`
9. Do a local build with `./build.sh` or build a release with `./scripts/local-mkrelease.sh`.
10. Local builds can be run with `./run.sh`; to run a binary release,
just unpack the archive and run the `matterhorn` binary found therein.
