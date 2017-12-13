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
    if [ ! -f "$HERE/cabal.project.local" ]
    then
        FIRST_TIME=1
        echo 'packages: deps/mattermost-api/mattermost-api.cabal'  >cabal.project.local
        echo '          deps/mattermost-api-qc/mattermost-api-qc.cabal' >>cabal.project.local
        echo '          deps/aspell-pipe/aspell-pipe.cabal' >>cabal.project.local
    fi
}

# clone_or_update_repo TARGET_BRANCH REPO_URL DEST_DIR
#
# Clones if absent; pulls otherwise.
#
# If this was a clone, then it tries to update the repo to the same
# branch as the matterhorn repo (the expectation is that parallel
# changes in the this related repo will have parallel branches).
# Sometimes changes in the matterhorn repo do not have associated
# changes in the this repo; in that case, the "develop" branch is
# tried in this repo, followed by the master branch if there is no
# develop branch.
#
# This is based on the branching strategy for matterhorn and
# associated repos of (1) master is current release, (2) develop is
# where completed work is collected for the next release candidate,
# (3) feature branches are made off of develop, and (4) bugfix
# branches can be made off of either master or develop, and (5) the
# "branch" name can actually be a release tag, and related repos
# should either bear the same release tag or use the head of the
# master branch for their release.
#
# If this was a pull, assume the directory containing the dependent
# repo is already on the branch it should have and just use the pull
# to move to the latest HEAD on that branch.
function clone_or_update_repo {
    mkdir -p $DEPS

    local branch=$1
    local repo=$2
    local destdir=$3
    local origdir=$(pwd)

    if [ ! -d "$destdir" ]
    then
        git clone "$repo" "$destdir" 2> gitclone.err
        cd $destdir
        for tag in "$branch" develop master; do
            if git checkout "$tag" 2>/dev/null
            then
                break
            fi
        done
        echo "Using branch ${tag}, revision $(git -C $destdir rev-parse --verify HEAD) for ${repo} in ${destdir}"
    else
        cd $destdir
        git pull
    fi
}

function install_deps {
  branch=$(current_branch)
  clone_or_update_repo "${branch}" "$MATTERMOST_API_REPO" "$MATTERMOST_API_DIR"
  clone_or_update_repo "${branch}" "$MATTERMOST_API_QC_REPO" "$MATTERMOST_API_QC_DIR"
  clone_or_update_repo "${branch}" "$ASPELL_PIPE_REPO" "$ASPELL_PIPE_DIR"
}

# The install.sh is normally run from a git checkout of the matterhorn
# repository, but if TRAVIS_BRANCH is set, this is a Continuous
# Integration (CI) automated build for a specific version.
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

    if [ $FIRST_TIME -eq 1 -a "$GHCVER" != "8.0.1" ]
    then
        # Build with '-j' for natural parallelism to get dependencies
        # installed as fast as possible if this is the first build
        # (but this frequently fails for GHC 8.0.1)
        cabal new-build -j --enable-tests
    else
        # But for subsequent builds, build with -j1 to avoid suppression
        # of useful (e.g. warning) output.
        cabal new-build -j1 --enable-tests
    fi
}


init
install_deps
build
