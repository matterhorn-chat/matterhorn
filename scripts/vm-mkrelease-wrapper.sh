#!/usr/bin/env bash

# This script is intended to be used to perform the steps to build
# a release on the local host and is used as part of a release
# orchestration process managed by mkrelease.sh.

set -e

HERE=$(cd `dirname $0`; pwd)
ROOT=$HERE/..
RELEASE_BRANCH=develop

if ! which cabal >/dev/null 2>/dev/null
then
    if [ -d $HOME/.cabal/bin ]
    then
        PATH=$HOME/.cabal/bin:$PATH
    fi
fi

cd $ROOT
cabal new-update || cabal update
git checkout $RELEASE_BRANCH
git pull
git submodule update --init
scripts/local-mkrelease.sh
