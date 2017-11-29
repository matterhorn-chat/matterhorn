#!/usr/bin/env bash

# This script builds and installs this package and its dependencies in a
# sandbox. This script is also suitable for rebuilds during development.
#
# Note that this uses cabal 'new-build' operation instead of standard
# install.

set -e

HERE=$(cd `dirname $0`; pwd)

# Where dependency repos get cloned
DEPS=$HERE/deps

# The source for the mattermost API package
MATTERMOST_API_REPO=https://github.com/matterhorn-chat/mattermost-api.git
MATTERMOST_API_QC_REPO=https://github.com/matterhorn-chat/mattermost-api-qc.git
ASPELL_PIPE_REPO=https://github.com/matterhorn-chat/aspell-pipe.git

# Where to clone the mattermost API package
MATTERMOST_API_DIR=$DEPS/mattermost-api
MATTERMOST_API_QC_DIR=$DEPS/mattermost-api-qc
ASPELL_PIPE_DIR=$DEPS/aspell-pipe

# Whether this is a first-time install (see below)
FIRST_TIME=0

function init {
    if [ ! -d "$HERE/cabal.project.local" ]
    then
        FIRST_TIME=1
        echo 'packages: deps/mattermost-api/mattermost-api.cabal'  >cabal.project.local
        echo '          deps/mattermost-api-qc/mattermost-api-qc.cabal' >>cabal.project.local
        echo '          deps/aspell-pipe' >>cabal.project.local
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
        git clone "$repo" "$destdir" 2> gitclone.err
        if git -C $destdir checkout "$branch"
        then
            tag=$branch
        else
            tag=master  # default to master
            devtag=$(git rev-parse origin/develop)
            if git rev-list --pretty=online --topo-order --simplify-by-decoration master..$branch | grep -E $devtag
            then
                # This branch was based on develop, so try to use the
                # develop branch in the dependent repo
                if git -C $destdir checkout develop
                then
                    tag=develop
                fi
            fi
        fi
        echo "Using branch ${tag}, revision $(git -C $destdir rev-parse --verify HEAD) for ${repo} in ${destdir}"
    else
        git -C $destdir pull
    fi
}

function install_deps {
  clone_or_update_repo "$(current_branch)" "$MATTERMOST_API_REPO" "$MATTERMOST_API_DIR"
  clone_or_update_repo "$(current_branch)" "$MATTERMOST_API_QC_REPO" "$MATTERMOST_API_QC_DIR"
  clone_or_update_repo "$(current_branch)" "$ASPELL_PIPE_REPO" "$ASPELL_PIPE_DIR"
}

function current_branch {
  if [[ -z "$TRAVIS_BRANCH" ]]
  then
      git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/'
  else
      echo "$TRAVIS_BRANCH"
  fi
}

function build {
    cd $HERE

    if [ $FIRST_TIME -eq 1 ]
    then
        # For first-time builds, get dependencies installed as fast as
        # possible.
        cabal new-build -j --enable-tests
    else
        # But for subsequent builds, build with -j1 to avoid suppression
        # of useful (e.g. warning) output.
        cabal new-build -j1 --enable-tests
    fi
}


init
set -x
install_deps
set +x
build
