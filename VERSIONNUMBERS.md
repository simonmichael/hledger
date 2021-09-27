# Version numbers

Some places version numbers appear:

- --version (and sometimes --help) output of all hledger* executables
- web manuals on hledger.org
- download page
- changelogs
- release notes
- release announcements
- hackage/stackage uris
- cabal tarball filenames
- platform-specific packages

Some old version numbering goals:

1. automation, robustness, simplicity, platform independence
2. cabal versions must be all-numeric
3. release versions can be concise (without extra .0's)
4. releases should have a corresponding VCS tag
5. development builds should have a precise version appearing in --version
6. development builds should generate cabal packages with non-confusing versions
7. there should be a way to mark builds/releases as alpha or beta
8. avoid unnecessary compiling and linking
9. minimise VCS noise and syncing issues (commits, unrecorded changes)

Current version numbering policy:

- We (should) follow <https://haskell.org/haskellwiki/Package_versioning_policy>

- The "full release version" is ma.jor.minor, where minor is 0 for a
  normal release or 1..n for bugfix releases. Each component is a
  natural number (can be >= 10). Eg: 1.13 major release, 1.13.1
  bugfix release.

- The "release version", which we prefer to use when possible, is
  just ma.jor when minor is 0. Ie elide the dot zero.

- The build version is ma.jor.minor+patches, where patches is the number
  of patches applied in the current repo since the last release tag.

- `hledger --version` shows the release version or build version as
  appropriate.

- Release tags in the VCS are like PKG-VERSION. Eg hledger-1.13,
- hledger-ui-1.13.1.

Current process:

- In each hledger package directory there's a `.version` file
  containing its desired version number.
  
- After changing a `.version` file: run `./Shake setversion` to
  propagate the versions to all other places in the packages where
  they should appear. This is not perfect (see Shake.hs) so review and
  manually adjust the proposed changes before committing.  Those
  places include (you can also run these rules individually):

  - `PKG/package.yaml` contains the cabal package version declaration,
    bounds on other hledger packages, and a CPP VERSION macro used in
    `hledger/Hledger/Cli/Version.hs`. Changes in package.yaml will be
    propagated to `PKG/PKG.cabal` on the next stack or Shake build, or
    by `make gencabal`.

  - `PKG/.version.m4` contains the _version_ macro used in  documentation source files (*.m4.md). It is updated by `./Shake setversion`.

  - `PKG/.date.m4` contains the _monthyear_ macro used in  man pages. It is updated by `./Shake manuals`.

- At release time:

  - `./Shake PKG/CHANGES.md-finalise` converts the topmost heading, if
    it is an interim heading (just a commit hash), to a permanent
    heading containing the version and today's date.

  - for each package being released, a PKG-VERSION git tag is created.

- At major release time:

  - A new snapshot of the reference docs is added to the website, by
    `./Shake site/doc/VERSION/.snapshot`, and added to the links in
    `site/js/site.js`.

