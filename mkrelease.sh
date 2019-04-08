#!/usr/bin/env bash

# This script will generate a release image appropriate to the local
# platform type.  These are not natively packaged (e.g. an RPM or DEB
# file) but are instead tarballs containing the binary as built on/for
# that platform.
#
# 1. git clone -b develop https://github.com/matterhorn-chat/matterhorn
# 2. cd matterhorn
# 3. ./mkrelease.sh
# 4. Copy the generated FILENAME result to the appropriate location.
#
# Note that this script will perform a submodule update; the default
# submodule access is via git/ssh, which requires a local key.  If
# this script is run from an automated build process, it is
# recommended to convert the git/ssh submodule url references in
# .gitmodules to https references itself (e.g.
#
#   $ sed -i -e /url/s,git@github.com:,https://github.com/, .gitmodules
#
# The symptom of using the git/ssh without an available key is
#
#    ...
#    Cloning into 'submodules/aspell-pipe'...
#    Permission denied (publickey).
#    fatal: Could not read from remote repository.
#
#    Please make sure you have the correct access rights
#    and the repository exists.
#    Clone of 'git@github.com:matterhorn-chat/aspell-pipe.git' into submodule path 'submodules/aspell-pipe' failed
#
# When this occurs, the above sed update to gitmodules will fix the
# references, but the cached URL's must also be dismissed:
#
#    $ git submodules sync

set -e

HERE=$(cd `dirname $0`; pwd)

function get_platform {
    if [ -f "/etc/os-release" ]
    then
        # Use ID from /etc/os-release because it is a valid
        # single-word element, whereas NAME may contain spaces.
        . /etc/os-release
        echo $ID
    else
        uname -s
    fi
}

function get_arch {
    uname -m
}

function output_dirname {
    if [ -f /etc/os-release ]
    then
        # sets vars, incl VERSION_ID and potentially VERSION_CODENAME
        . /etc/os-release

        # Sometimes this one isn't set, so we only want to put a "-" in
        # the directory name if it actually is.
        if ! [ -z "$VERSION_CODENAME" ]
        then
            VERSION_CODENAME="-$VERSION_CODENAME"
        fi

        if ! [ -z "$VERSION_ID" ]
        then
            VERSION_ID="-$VERSION_CODENAME"
        fi

        echo $BASENAME-$MHVERSION-$PLATFORM${VERSION_ID}${VERSION_CODENAME}-$ARCH
    else
        echo $BASENAME-$MHVERSION-$PLATFORM-$ARCH
    fi
}

MHVERSION=$(grep "^version:" matterhorn.cabal | awk '{ print $2 }')
BASENAME=matterhorn
ARCH=$(get_arch)
PLATFORM=$(get_platform)
LONG_HEAD=$(git log | head -1 | awk '{ print $2 }')
SHORT_HEAD=${LONG_HEAD:0:8}
DIRNAME=$(output_dirname)
FILENAME=$DIRNAME.tar.bz2
CABAL_DEPS_REPO=https://github.com/matterhorn-chat/cabal-dependency-licenses.git
CABAL_DEPS_TOOL_DIR=$HOME/.cabal/bin

function prepare_dist {
    local ver=$1
    local dest=$2
    cp $(find dist-newstyle -type f -name matterhorn | grep $ver) $dest
    strip $dest/matterhorn
    cp $HERE/sample-config.ini $dest
    cp $HERE/README.md $dest
    cp $HERE/CHANGELOG.md $dest
    cp -r $HERE/syntax $dest/
    cp -r $HERE/notification-scripts $dest/
    echo $LONG_HEAD > $dest/COMMIT

    cd $HERE && $CABAL_DEPS_TOOL_DIR/cabal-dependency-licenses > $dest/COPYRIGHT
}

function install_tools {
    if [ ! -f $CABAL_DEPS_TOOL_DIR/cabal-dependency-licenses ]
    then
        BUILD=$(mktemp -d)
        cd $BUILD
        git clone $CABAL_DEPS_REPO

        cd cabal-dependency-licenses
        cabal install
        mkdir -p $CABAL_DEPS_TOOL_DIR
        cd $HERE && rm -rf $BUILD
    fi
}

install_tools

echo Version: $MHVERSION
echo Filename: $FILENAME

# Perform a build
cd $HERE
git submodule update --init
./build.sh

echo %
echo % Verify that the keybindings are up-to-date
echo %
diff keybindings.md <(./run.sh -i -K)
echo

TMPDIR=$(mktemp -d)
function cleanup {
    rm -rf $TMPDIR
}
trap cleanup EXIT

# Package the build results into a tarball
mkdir $TMPDIR/$DIRNAME
prepare_dist $MHVERSION $TMPDIR/$DIRNAME
cd $TMPDIR && tar -cj $DIRNAME > $HERE/$FILENAME

echo %%%%%%%%%% completed package build %%%%%%%%%%
echo Version: $MHVERSION
echo Filename: $FILENAME
