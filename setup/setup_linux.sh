#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

function ubuntu {
    grep -i ubuntu /etc/apt/sources.list 2>/dev/null >/dev/null
}

function fedora {
    grep -i fedora /etc/redhat-release 2>/dev/null >/dev/null
}

function centos {
    grep -i centos /etc/redhat-release 2>/dev/null >/dev/null
}

function install_packages {
    if ubuntu
    then
        sudo apt-get install --yes \
            ghc ghc-prof \
            happy \
            alex \
            zlib1g-dev
    elif fedora
    then
        sudo dnf install -y \
            ghc \
            happy \
            alex \
            zlib-devel
    elif centos
    then
        sudo yum install -y gmp-devel gmp zlib-devel ncurses-devel wget
        sudo yum groupinstall -y "Development Tools"

        # Yes, Debian 8 GHC builds are used for CentOS.
        GHC_VER=8.4.4
        GHC_DIR=ghc-$GHC_VER
        GHC_PKG=$GHC_DIR-x86_64-deb8-linux.tar.xz

        CABAL_VER=2.4.1.0
        CABAL_DIR=cabal-install-$CABAL_VER
        CABAL_PKG=$CABAL_DIR.tar.gz

        wget -c https://downloads.haskell.org/~ghc/$GHC_VER/$GHC_PKG
        wget -c http://hackage.haskell.org/package/$CABAL_DIR/$CABAL_PKG

        tar -xf $GHC_PKG
        cd $GHC_DIR && ./configure --prefix=/usr/local/ && sudo make install && cd ..

        tar -xf $CABAL_PKG
        cd $CABAL_DIR && ./bootstrap.sh && cd ..

        echo "Setup done. NOTE! Add $HOME/.cabal/bin/ to your PATH."
    else
        echo "Unsupported Linux distribution"
        exit 1
    fi
}

function install_cabal {
    local ver=1.24.0.2
    local pkg=cabal-install-$ver.tar.gz
    local url=https://hackage.haskell.org/package/cabal-install-$ver/$pkg
    local tmp=$(mktemp -d)

    if ! which cabal >/dev/null 2>/dev/null
    then
        cd $tmp
        wget $url
        tar -xf $pkg
        cd cabal-install-$ver
        bash bootstrap.sh
    fi

    rm -rf $tmp
}

install_packages
install_cabal
