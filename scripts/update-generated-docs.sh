#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)
ROOT=$HERE/..

COMMANDS=$ROOT/docs/commands.md
KEYBINDINGS=$ROOT/docs/keybindings.md

$ROOT/run.sh -m -f markdown > $COMMANDS
$ROOT/run.sh -k -f markdown > $KEYBINDINGS

if ! git diff --exit-code $COMMANDS $KEYBINDINGS >/dev/null
then
    echo "Generated docs updated, please commit by hand."
else
    echo "Generated docs unchanged."
fi
