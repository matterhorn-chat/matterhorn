#!/bin/bash

# This script is modified from the
# [verus](https://github.com/verus-lang/verus/) release script.

set -e

script_dir=$(dirname "${BASH_SOURCE[0]}")
cd "$script_dir"

function remove_quarantine {
    local attributes
    attributes=$(xattr "$1")
    if printf '%s\n' "$attributes" | grep -Fxq 'com.apple.quarantine'
    then
        xattr -d com.apple.quarantine "$1"
    fi
}

remove_quarantine matterhorn
