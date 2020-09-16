#!/usr/bin/env bash

# This script starts a Tmux session to make a release on all release
# hosts. The script makes some assumptions:
#
# * You are on the Galois, Inc. VPN., which is required to access the
#   build hosts. This script is not intended to be run by third
#   parties; if you want to make a release for your platform, use
#   scripts/local-mkrelease.sh.
# * You have tmux installed locally.
# * Each build host has cabal and GHC in its PATH.
# * You can authenticate to each build host with your SSH key.
# * The Github host key is authorized on all target hosts.
#
# The script does NOT assume that Matterhorn is cloned on the target
# host, and it will take care of that when necessary.

set -e

HERE=$(cd `dirname $0`; pwd)
ROOT=$HERE/..
REPO=git@github.com:matterhorn-chat/matterhorn.git
SESSION=matterhorn-release
RELEASE_BRANCH=develop

CMD="([ -d matterhorn ] || git clone $REPO) && cd matterhorn && git checkout $RELEASE_BRANCH && git pull && bash scripts/vm-mkrelease-wrapper.sh"

tmux \
    new-session -d -s $SESSION -n "ubuntu-1804" "ssh -A galois@vm-35-dd.eic.galois.com '$CMD'" \; \
    set-option -t $SESSION:0 remain-on-exit on \; \
    new-window -d -t $SESSION -n "ubuntu-2004" "ssh -A galois@vm-36-83.eic.galois.com '$CMD'" \; \
    set-option -t $SESSION:1 remain-on-exit on \; \
    new-window -d -t $SESSION -n "fedora" "ssh -A galois@vm-35-de.eic.galois.com '$CMD'" \; \
    set-option -t $SESSION:2 remain-on-exit on \; \
    new-window -d -t $SESSION -n "centos" "ssh -A galois@vm-35-a7.eic.galois.com '$CMD'" \; \
    set-option -t $SESSION:3 remain-on-exit on \; \
    new-window -d -t $SESSION -n "macos" "ssh -A grannysmith.galois.com '$CMD'" \; \
    set-option -t $SESSION:4 remain-on-exit on
