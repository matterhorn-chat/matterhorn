#!/usr/bin/env bash

set -e

function usage {
    echo "Usage: $0 <skylighting version>"
}

HERE=$(cd `dirname $0`; pwd)

SKYLIGHTING_VER=$1
SYNTAX=$HERE/../syntax
PKG=skylighting-core-$SKYLIGHTING_VER

if [ -z "$SKYLIGHTING_VER" ]
then
    usage
    exit 1
fi

mkdir -p $SYNTAX

BASE=$(mktemp -d)

function rm_base {
    rm -r $BASE
}

trap "rm -r $BASE" exit

cd $BASE
cabal unpack $PKG
cp -f $PKG/xml/*.xml $SYNTAX
echo $SKYLIGHTING_VER > $SYNTAX/skylighting-version
