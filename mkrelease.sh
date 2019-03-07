#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

function get_platform {
    if [ -f "/etc/redhat-release" ]
    then
        if grep Fedora /etc/redhat-release >/dev/null
        then
            echo "Fedora"
        elif grep CentOS /etc/redhat-release >/dev/null
        then
            echo "CentOS"
        else
            echo "Unknown-Redhat"
        fi
    elif grep -i ubuntu /etc/apt/sources.list 2>/dev/null >/dev/null
    then
        echo "Ubuntu"
    else
        uname -s
    fi
}

function get_arch {
    uname -m
}

VERSION=$(grep "^version:" matterhorn.cabal | awk '{ print $2 }')
BASENAME=matterhorn
ARCH=$(get_arch)
PLATFORM=$(get_platform)
LONG_HEAD=$(git log | head -1 | awk '{ print $2 }')
SHORT_HEAD=${LONG_HEAD:0:8}
DIRNAME=$BASENAME-$VERSION-$PLATFORM-$ARCH
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

echo Version: $VERSION
echo Filename: $FILENAME
cd $HERE && git submodule update --init && ./build.sh

TMPDIR=$(mktemp -d)
function cleanup {
    rm -rf $TMPDIR
}
trap cleanup EXIT

mkdir $TMPDIR/$DIRNAME
prepare_dist $VERSION $TMPDIR/$DIRNAME
cd $TMPDIR && tar -cj $DIRNAME > $HERE/$FILENAME
