Matterhorn Build Instructions
-----------------------------

1. Install [ghcup](https://www.haskell.org/ghcup/).
2. Install GHC with `ghcup install`.
3. Install Cabal with `ghcup install-cabal`.
4. Add `~/.ghcup/bin` to your `PATH`.
5. Run `cabal new-update`
6. Create a GitHub account (required for cloning submodules).
7. Clone the `matterhorn` source: `git clone git@github.com:matterhorn-chat/matterhorn.git`
8. `cd matterhorn`
9. Fetch submodules with `git submodule update --init`
10. Build a release with `./mkrelease.sh`
