# Releasing

Guidance for release managers and maintainers.

Terminology:

- "main" = the `master` branch in the main hledger repo (might be renamed to main in future).
- "release" = a release branch in the main hledger repo, such as `1.22`.

## Changelogs

Always maintain changelogs in main.

Apply [CONTRIBUTING.md#commit-messages](CONTRIBUTING.html#commit-messages) rules
when commiting, pushing, or reviewing/merging pull requests. 
`tools/commitlint` helps with this, run locally and in CI.

Use `./Shake changelogs` to update them from recent commit messages.

## Minor release

Checklist:

1. create release branch if none\
  `git branch RELEASEBRANCH RELEASETAG`\
  `git branch 1.22-branch 1.22`

1. update main changelogs
    - `./Shake changelogs`
    - do at least basic editing - drop things, move things
    - `./Shake changelogs -c`

1. review changes so far, estimate which packages will be released

1. add "unreleased" minor release heading in main changelogs, immediately above previous release heading
    ```
    # LATESTHASH

    ...
    
    # X.Y.1 unreleased  <- new heading

    # X.Y YYYY-MM-DD
    ```

1. cherry pick changes to release
    1. always update main changelogs first
    2. cherry pick minor-release-worthy commits
        - don't cherry pick changelog commits, "dev: doc: update ..."
    3. in main changelogs, move corresponding change items under minor release heading

1. finalise release
    - add date to minor release heading in main changelogs
    - copy the minor release section from main changelogs to release changelogs
