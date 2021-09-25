# Releasing

<div class=pagetoc>
<!-- toc -->
</div>

Guidance for release managers and maintainers.

Some terminology used on this page:

|                         |                                                                                       |
|-------------------------|---------------------------------------------------------------------------------------|
| *OLD*                   | previous release version, eg `1.22` or `1.22.1`                                       |
| *NEW*                   | new release version, eg `1.22.2` or `1.23`                                            |
| *MAJORVER*              | just the major version part, eg `1.22` or `1.23`                                      |
| *master (branch)*       | `master` branch in the `hledger` repo, ie the main line of development                |
| *release&nbsp;(branch)* | a release branch in the `hledger` repo, eg `1.22-branch`                              |
| *site*                  | `master` branch in the `hledger_website` repo. Usually checked out as `hledger/site`. |
|                         |                                                                                       |

## When committing / reviewing commits

Follow/encourage [commit conventions](CONTRIBUTING.html#commit-messages). Recap:
- commit messages must begin with one or more colon-terminated words
- user-visible changes must begin with a `feat:`/`imp:`/`fix:` prefix, and will appear in release notes
- other changes can begin with a topic prefix (`bal:`/`areg:`/`test:`/`doc:`/`lib:`/...)
- add a leading `;` to skip wasteful CI builds
- add a `!` to indicate breaking/incompatible changes
- mention any relevant #issue numbers, usually parenthesised at the end
- write the summary and at least the first part of the body, if any,
  as clear change documentation for the intended audience 
  (users/installers/packagers/developers)

**When committing/pushing/merging:**
- run `bin/commitlint` before push, to check recent commits
- or, run it automatically before each commit (`make installcommithook` to configure your local repo)
- it also runs in CI on github for pull requests, etc.

## Changelogs

Always maintain changelogs in master branch (not in release branches).

**Frequently**, especially after merging changes, and before cherry picking into release branch:

- dry run: `./Shake changelogs -n`
- add new changes: `./Shake changelogs`
- edit
  - drop things
  - move things
  - Add headings: Features, Improved, Fixes
  - rewrite things
  - format ([#ISSUE](https://github.com/simonmichael/hledger/issues/), AUTHOR) on its own line
- commit: `./Shake changelogs -c`

**After cherry-picking** changes to a release branch:
- in the master branch changelogs, move the corresponding changelog items under a pending release heading,
  creating that when necessary:
    ```
    # LATESTHASH

    ...CHANGES ONLY IN MASTER...

    # NEXTVER unreleased

    ...CHANGES CHERRYPICKED INTO RELEASE BRANCH...

    # LASTVER YYYY-MM-DD
    ```

**At release:**

- do final update/edits; check organisation, wording, formatting, issue links
- replace "unreleased" with the release date
- copy the new sections from master changelogs to release branch changelogs

## Release prep

1. create release branch when needed:\
  `git branch MAJORVER-branch BRANCHPOINT`\
   Sometimes when convenient we make major releases on master (adapt the steps below as needed).
   And make the release branch later, when a minor release becomes necessary, eg:\
  `git branch 1.22-branch 1.22`

1. update changelogs in master

1. review changes so far, estimate which packages will be released

1. cherry pick changes to release
    - cherry pick release-worthy commits 
        - from: magit, `l o MAJORVER-branch..master`, `M-x magit-toggle-buffer-lock`, `M-x toggle-window-dedicated` (`C-c D`)
        - to:   magit, `l o master..MAJORVER-branch`, `M-x magit-toggle-buffer-lock`, `M-x toggle-window-dedicated`
        - ignore commits already seen in previous cherry picking sessions
        - ignore changelog commits / other boring commits 
          ("dev: doc: update changelogs")

    - update changelogs in master (move corresponding change items under pending release heading)

## Release day

In master:

- `./Shake.hs` to ensure `Shake` is current, review commands

- finalise [changelogs](#changelogs), copy each new section (emacs `C-x r s a` ... `b` ... `c` ... `d`)

In release branch:

- paste new sections into changelogs

- `./Shake setversion NEWVER [-c]` (first without `-c` to review, then with `-c` to commit).
  <!-- Also `touch hledger/Hledger/Cli/Version.hs` ? -->

- `./Shake cmdhelp [-c]`

- `./Shake mandates`

- `./Shake manuals -B [-c]`  (using up to date doctool versions)

- `make tag`  (signed annotated tags for each package and the overall release)

- `stack clean && stack install && hledger --version && hledger-ui --version && hledger-web --version`
  to build locally and check version strings

- push to CI branches to test and to build release binaries
  - magit `P -f e origin/ci-windows`
  - magit `P -f e origin/ci-mac`
  - magit `P -f e origin/ci-linux-static`
  - magit `P -f e origin/ci-linux-static-arm32` (at release time only)
  - Tips:
    - build these release binaries at the very last possible moment
    - last commit should be a notable one - not docs only, not beginning with ;

In site repo:

- update `release-notes.md`
  - copy template, uncomment
  - replace date
  - replace XX with NEW
  - add new changelog sections, excluding hledger-lib
  - remove any empty sections
  - add contributors, `git shortlog -sn OLD..NEW`
  - add summary (major release) or remove it (minor release)
  - check preview in vs code
  - commit: `relnotes: NEW`

- update `download.md`
  - query-replace OLD -> NEW in 
    - "current hledger release"
    - CI binaries badges/links, including linux-static-arm32v7 if built
    - "building from source"
    - stack install command
    - cabal install command
  - query-replace OLD-brightgreen -> OLD-red
  - only after release binaries are built (preferably after release is published):
    update --version outputs (search: hledger --version)
  - final output line from `hledger test` (run in terminal for normal speed)
  - Total count from `make functest`
  - commit: `download: NEW`

In release branch:

- update `doc/ANNOUNCE` (major release)
  - summary, contributors from release notes
  - any other edits
  - commit: `;doc: ANNOUNCE`

In master:

- cherry pick useful release branch changes
  - `;doc: ANNOUNCE`

- update `hledger-install/hledger-install.sh`
  - HLEDGER_INSTALL_VERSION
  - RESOLVER
  - HLEDGER_*_VERSION
  - EXTRA_DEPS

- wait for CI binaries, https://ci.hledger.org

- pre-release pause: take a break away from keyboard

## Release

In release branch:

- review status, reflect

- `make hackageupload`

- push tags: magit `P t origin`

- create github release
  - https://github.com/simonmichael/hledger/releases, copy last release's body
  - create new release from NEW tag if CI workflow has failed
  - tag NEW, title NEW, body similar to previous release
  - at https://ci.hledger.org download CI binary artifacts, check sizes look similar to previous
  - select downloaded artifacts in Finder, drag into github release
  - publish release

- announce
  - push site download/relnotes updates
  - push master hledger-install update
  - share release notes link, then markdown content, in #hledger chat
  - send ANNOUNCE to hledger@googlegroups.com, haskell-cafe@googlegroups.com (major release)
    or brief announcement to hledger@googlegroups.com (minor release)
    - ANN: hledger NEW
    - release notes link, summary
    - release notes html (copied from browser)
  - tweet at https://twitter.com/simonkwmichael ?
  - toot at https://fosstodon.org/web/accounts/106304084994827771 ?

## Post release

- merge/check/update download page changes
  - docker - expect/merge PR
  - homebrew - expect badge to update soon
  - nix - expect `make nix-hledger-version` to update after a few days, find and update to that commit hash
  - linux distros - once in a while, follow the links & search for newer versions, update

- support

- handle issues

- update procedures, tools, docs
