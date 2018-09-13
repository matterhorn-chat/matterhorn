#!/usr/bin/env bash

# This script builds and installs this package and its dependencies in a
# sandbox. This script is also suitable for rebuilds during development.
#
# Note that this uses cabal 'new-build' operation instead of standard
# install.

set -e

HERE=$(cd `dirname $0`; pwd)
cd $HERE
git submodule update --init
cabal new-build -j --enable-tests
