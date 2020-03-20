#!/usr/bin/env bash

# This script builds and installs this package and its dependencies in a
# sandbox. This script is also suitable for rebuilds during development.

set -e

HERE=$(cd `dirname $0`; pwd)
cd $HERE
cabal new-build -j --enable-tests
