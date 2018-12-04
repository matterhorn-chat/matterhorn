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
        sudo yum install -y gmp-devel gmp zlib-devel ncurses-devel
        sudo yum groupinstall -y "Development Tools"
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
