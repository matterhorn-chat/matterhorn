
Matterhorn Release Process
==========================

This is the release procedure for making a release of `matterhorn` and
its related packages, `mattermost-api` and `mattermost-api-qc`. Before
beginning, the person making the release should have access to the
platforms on which binary distributions will be built.

All operations in this process start from the `develop` branch of each
package repository.

1. Set the `matterhorn` package version. The version string must be of
   the form `ABBCC.X.Y` where ABBCC corresponds to the Mattermost
   server version supported by the release. For example, if the release
   supports Mattermost server version 1.2.3, the ABBCC portion of the
   `matterhorn` version should be `10203`. The `X.Y` portion of the
   version corresponds to our own version namespace for the package.
   If the server version changes, `X.Y` SHOULD be `0.0`. Otherwise the
   first component should increment if the package undergoes major code
   changes or functionality changes. The second component alone should
   change only if the package undergoes security fixes or other bug
   fixes.

   Also set the versions of the API packages similary depending on what
   changed in each one.

2. Generate a changelog entry list from the git log since the last
   release tag. In the changelog, include

   * changes to supported server versions
   * bugs fixed
   * contributions received (and thank contributors)
   * UI changes
   * new configuration options and keybindings
   * server feature support changes

   The changes listed in the changelog should inform the choice of
   version number.

   Also check the dependency version bounds for the `mattermost-api` and
   `mattermost-api-qc` packages in case those changed or need to be
   updated.

   Also make sure that contributors mentioned in the changelog are
   present in AUTHORS.txt.

3. Commit the changelog changes and Cabal version changes for each
   package.

4. Check for a passing Travis CI build.

5. Generate platform binary distributions using `mkrelease.sh` on the
   relevant platforms.

6. Upload each package to Hackage, starting with `mattermost-api`:

   * Generate a `cabal sdist`
   * Unpack the `sdist` archive
   * Perform a complete build on the unpacked archive
   * If any issues arise, repair and go to (2)
   * Otherwise, `cabal upload` the package

7. Tag the release commit using the package version as the tag string.

8. `git push --tags`

9. Merge the `develop` branch to the `master` branch for each
   repository.

10. Make binary release available on GitHub by editing the pushed tag and
    uploading the release archives.

11. Tweet about the release using the `@matterhorn_chat` account:
    "We're pleased to announce the release of Matterhorn version <VERSION> for @mattermosthq!
    Get it at https://github.com/matterhorn-chat/matterhorn/releases/tag/<VERSION>"

Release Hosts
=============

OS X:
* `gala.galois.com`
* `fuji.galois.com`

Ubuntu:
* `vm-35-59.eic.galois.com`

Fedora:
* `vm-35-58.eic.galois.com`
