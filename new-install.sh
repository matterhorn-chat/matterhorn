#!/usr/bin/env bash

# This script builds and installs this package and its dependencies in a
# sandbox. This script is also suitable for rebuilds during development.
#
# Note that this uses cabal 'new-build' operation instead of standard install.

set -e

HERE=$(cd `dirname $0`; pwd)

# Where dependency repos get cloned
DEPS=$HERE/deps

# The source for the mattermost API package
MATTERMOST_API_REPO=https://github.com/matterhorn-chat/mattermost-api.git

# Where to clone the mattermost API package
MATTERMOST_DIR=$DEPS/mattermost-api

# Whether this is a first-time install (see below)
FIRST_TIME=0

function init {
    if [ ! -d "$HERE/cabal.project.local" ]
    then
        FIRST_TIME=1
        echo 'packages: deps/mattermost-api/mattermost-api.cabal' >cabal.project.local
    fi
}

# clone_or_update_repo $REPO_URL $DEST_DIR
#
# Clones if absent; pulls otherwise.
function clone_or_update_repo {
    mkdir -p $DEPS

    local branch=$1
    local repo=$2
    local destdir=$3

    if [ ! -d "$destdir" ]
    then
        git clone -b "$branch" "$repo" "$destdir"
    else
        cd $destdir && git pull
    fi
}

function install_deps {
  clone_or_update_repo "$(current_branch)" "$MATTERMOST_API_REPO" "$MATTERMOST_DIR"
}

function current_branch {
  if [[ -z "$TRAVIS_BRANCH" ]]
  then
      git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
  else
    "$TRAVIS_BRANCH"
  fi
}

function build {
    cd $HERE

    if [ $FIRST_TIME -eq 1 ]
    then
        # For first-time builds, get dependencies installed as fast as
        # possible.
        cabal new-build -j
    else
        # But for subsequent builds, build with -j1 to avoid suppression
        # of useful (e.g. warning) output.
        cabal new-build -j1
    fi
}

init
install_deps
build
