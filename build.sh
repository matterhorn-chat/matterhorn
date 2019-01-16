#!/usr/bin/env bash

# This script builds and installs this package and its dependencies in a
# sandbox. This script is also suitable for rebuilds during development.
#
# Note that this uses cabal 'new-build' operation instead of standard
# install.

set -e

command_exists () {
  type "$1" &> /dev/null ;
}

if [[ "$OSTYPE" = darwin* ]]; then
  command_exists cabal || brew install cabal-install
  cabal update
  cabal install tasty-quickcheck
fi

HERE=$(cd `dirname $0`; pwd)
cd $HERE
cabal new-build -j --enable-tests
