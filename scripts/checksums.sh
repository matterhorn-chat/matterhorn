#!/usr/bin/env bash

# This script is used to generate checksums of the tarballs of the
# current version of Matterhorn during the release-building process.

set -e

HERE=$(cd `dirname $0`; pwd)
ROOT=$HERE/..
CABALFILE=$ROOT/matterhorn.cabal
VER=$(grep "^version:" $CABALFILE | awk '{ print $2 }')

function compute_sha {
    local file=$1

    if which shasum 2>/dev/null >/dev/null
    then
        shasum -a 256 $file | awk '{ print $1 }'
    elif which sha256sum 2>/dev/null >/dev/null
    then
        sha256sum $file | awk '{ print $1 }'
    else
        echo "Error: could not locate SHA-256 command-line utility"
        exit 1
    fi
}

if [ -z "$VER" ]
then
    echo "Error: could not detect current matterhorn version from $CABALFILE"
    exit 1
fi

cd $ROOT
first=""
for tarball in $ROOT/matterhorn-$VER-*.tar.bz2
do
    if [ ! -f "$tarball" ]
    then
        echo "Error: no matterhorn release tarballs found in $ROOT"
        exit 1
    fi

    if [ -z "$first" ]
    then
        echo "SHA-256 checksums for Matterhorn $VER"
        first="1"
    fi

    echo
    basename $tarball
    compute_sha $tarball
done
