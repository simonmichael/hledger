# Releasing

Guidance for release managers and maintainers.

Terminology:

- "main" = the master branch, might get renamed in future.
- "release" = a release branch, such as 1.22.

## Changelogs

Always maintain changelogs in main.

Apply [CONTRIBUTING.md#commit-messages](CONTRIBUTING.html#commit-messages) rules
when commiting, pushing, or reviewing/merging pull requests. 
`tools/commitlint` helps with this, run locally and in CI.

Use `./Shake changelogs` to update them from recent commit messages.

## Minor release

Checklist:

- create release branch if none\
  `git branch RELEASEBRANCH RELEASETAG`\
  `git branch 1.22-branch 1.22`
- update main changelogs\
  - `./Shake changelogs`
  - do at least basic editing - drop things, move things
  - `./Shake changelogs -c`
- review changes so far, estimate which packages will be released
- add "unreleased" minor release heading in main changelogs, immediately above previous release heading
  ```
  # LATESTHASH

  ...CHANGES...
  
  # X.Y.1 unreleased  <- new heading

  # X.Y YYYY-MM-DD
  ```
- cherry pick changes to release
  1. always update main changelogs first
  2. cherry pick commits
  3. move corresponding change items under minor release heading in main changelogs
- finalise release
  - add date to minor release heading in main changelogs
  - copy the minor release section from main changelogs to release changelogs
