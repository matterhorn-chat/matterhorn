#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

COMMANDS=$HERE/docs/commands.md
KEYBINDINGS=$HERE/docs/keybindings.md

$HERE/run.sh -m -f markdown > $COMMANDS
$HERE/run.sh -k -f markdown > $KEYBINDINGS

if ! git diff --exit-code $COMMANDS $KEYBINDINGS >/dev/null
then
    echo "Generated docs updated, please commit by hand."
else
    echo "Generated docs unchanged."
fi
