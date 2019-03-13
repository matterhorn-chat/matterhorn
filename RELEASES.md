
Matterhorn Release Process
==========================

This is the release procedure for making a release of `matterhorn` and
its related packages, `mattermost-api` and `mattermost-api-qc`. Before
beginning, the person making the release should have access to the
platforms on which binary distributions will be built.

All operations in this process start from the `develop` branch of each
package repository.  It's recommended that the release engineer create
a `staging_ABBCC.X.Y` branch to protect against other developer
commits during the release process.

1. Set the `matterhorn` package version in the cabal file. The version
   string must be of the form `ABBCC.X.Y` where ABBCC corresponds to
   the Mattermost server version supported by the release. For
   example, if the release supports Mattermost server version 1.2.3,
   the ABBCC portion of the `matterhorn` version should be
   `10203`. The `X.Y` portion of the version corresponds to our own
   version namespace for the package.  If the server version changes,
   `X.Y` SHOULD be `0.0`. Otherwise the first component should
   increment if the package undergoes major code changes or
   functionality changes. The second component alone should change
   only if the package undergoes security fixes or other bug fixes.

   Also set the versions of the API packages similarly depending on
   what changed in each one.  Note: originally the API packages were
   synchronized with Matterhorn via identical version numbers; this is
   no longer the case and the git submodules references are used for
   synchronization.

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

5. Ensure that the latest `keybindings.md` is committed. Note that the
   `-i` argument *MUST* be provided because it causes Matterhorn to
   ignore your local configuration. The generated keybinding table
   includes any customized bindings, so your local config must be
   ignored so that the generated table contains only default bindings.

   ```
   $ matterhorn -i -K > keybindings.md
   $ git add keybindings.md
   $ git commit
   $ git push
   ```

6. Generate platform binary distributions using `mkrelease.sh` on the
   relevant platforms.

7. Upload each package to Hackage, starting with `mattermost-api`:

   * Generate a `cabal sdist`
   * Unpack the `sdist` archive
   * Perform a complete build on the unpacked archive
   * If any issues arise, repair and go to (2)
   * Otherwise, `cabal upload` the package

8. Tag the release commit using the package version as the tag string.

9. `git push --tags`

10. Merge the `develop` branch to the `master` branch for each
   repository.

11. Make binary release available on GitHub by editing the pushed tag and
    uploading the release archives.

12. Tweet about the release using the `@matterhorn_chat` account:
    "We're pleased to announce the release of Matterhorn version <VERSION> for @mattermost!
    Get it at https://github.com/matterhorn-chat/matterhorn/releases/tag/<VERSION>"

The above process should be sufficient to generate a release on any of
the supported platforms (e.g. CentOS, Fedora, Ubuntu, MacOS). A Linux
release host be initialized to perform Matterhorn builds with our Linux
setup script:

```
$ bash setup/setup_linux.sh
```

Galois-Internal Release Notes
=============================

The Matterhorn development team thanks Galois, Inc. for supporting the
development and maintenance of Matterhorn. The internal Galois computing
resources used to make Matterhorn releases are described in our internal
repository.
