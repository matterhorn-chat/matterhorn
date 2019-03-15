#!/bin/sh

# Run the most recently-modified matterhorn binary in the matterhorn
# working tree.
#
# Portability: Linux, OS X

set -e

HERE=$(cd `dirname $0`; pwd)

# Portability note: -executable is only compatible with GNU find but
# this invocation should be more portable.  Also be flexible about old
# and new cabal output locations, preferring new, but avoiding
# slurping up the entire current directory.
OPTIONS=$(find $(for D in dist-newstyle dist; do [ -d $D ] && echo $D; done) -name matterhorn -type f \( -perm -u=x -o -perm -g=x -o -perm -o=x \) )
if [ -z ${OPTIONS} ] ; then
    echo "No matterhorn executable found; try $ cabal new-build"
    exit 2
fi
SORTED=$(ls -t ${OPTIONS})

# Run the most recently-modified binary that we found. Note that since
# we use exec, this loop never makes it past one iteration.
for OPTION in ${SORTED}; do
  exec "${OPTION}" ${1+$@}
done

echo "No executables found."
