#!/bin/sh

# Run the most recently-modified matterhorn binary in the matterhorn
# working tree.
#
# Portability: Linux, OS X

set -e

# Before you ask: yes, I know about -executable, but it only works for
# GNU find. Using -perm +111 should work solidly across platforms.
OPTIONS=$(find . -name matterhorn -type f \( -perm -u=x -o -perm -g=x -o -perm -o=x \) )
SORTED=$(ls -t ${OPTIONS})

# This is not elegant but I believe it's portable across shells. If
# there's a more elegant way to do it, please patch.
for OPTION in ${SORTED}; do
  exec "${OPTION}" ${1+$@}
done

echo "No executables found."

