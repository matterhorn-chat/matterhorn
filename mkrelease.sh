#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

function get_platform {
    uname -s
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
FILENAME=$DIRNAME.tar.gz

function prepare_dist {
    local dest=$1
    cp $HERE/dist-newstyle/build/matterhorn-$VERSION/build/matterhorn/matterhorn $dest
    cp $HERE/sample-config.ini $dest
    cp $HERE/README.md $dest
    cp $HERE/CHANGELOG.md $dest
    echo $LONG_HEAD > $dest/COMMIT
}

cd $HERE
echo Version: $VERSION
echo Filename: $FILENAME
./new-install.sh

TMPDIR=$(mktemp -d)
function cleanup {
    rm -rf $TMPDIR
}
trap cleanup EXIT

mkdir $TMPDIR/$DIRNAME
prepare_dist $TMPDIR/$DIRNAME
cd $TMPDIR && tar -cj $DIRNAME > $HERE/$FILENAME
