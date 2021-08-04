# Releasing

<div class=pagetoc>
<!-- toc -->
</div>

Guidance for release managers and maintainers.
Some of this might move elsewhere later.

## Terminology

- "main" / "main branch" = `master` branch in the main hledger repo (might get renamed in future)
- "release" / "release branch" = a release branch in the main hledger repo (eg `1.22-branch`)

## Commits

**When committing/reviewing:**

Follow our [commit conventions](CONTRIBUTING.html#commit-messages):
- in the summary, use a `feat:`/`imp:`/`fix:` prefix (required for user-visible changes),
  and/or topic prefixes (`bal:`/`areg:`/`test:`/`doc:`/`lib:`/...)
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

Always maintain changelogs in main branch (not in release branches).

**At start of release cycle:**
- add the next release heading, with date "unreleased", above last release heading:

    ```
    # LATESTHASH

    ...CHANGES...

    # NEXTVER unreleased  <- ADD

    # LASTVER YYYY-MM-DD
    ```

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
- move the corresponding changelog items under the pending release heading.

**At release:**

- do final update/edits; check organisation, wording, formatting, issue links
- replace "unreleased" with the date in the pending release heading
- copy the new release sections from main changelogs to release branch changelogs

## Pre release

1. create release branch when needed:\
  `git branch MAJORVER-branch BRANCHPOINT`\
   Sometimes, we make the major release tag (`1.22`) on master,
   and create a release branch (`1.22-branch`) when there's a followup minor release:\
  `git branch 1.22-branch 1.22`

1. update changelogs in main

1. review changes so far, estimate which packages will be released

1. cherry pick changes to release
    - cherry pick release-worthy commits 
        - from: magit, `l o X.Y..master`, `M-x magit-toggle-buffer-lock`, `C-x D`
            (`M-x toggle-window-dedicated`)
        - to: magit, `l o master..X.Y`, `M-x magit-toggle-buffer-lock`, `C-x D`
        - ignore commits already seen in previous cherry picking sessions
        - ignore changelog commits / other boring commits 
          ("dev: doc: update changelogs")

1. in changelogs in main, move corresponding change items under pending release heading

## Release

- finalise [changelogs](#changelogs) in main,
  copy to changelogs in release branch

- `./Shake.hs` to update `Shake` and review release tasksm

- `./Shake setversion VER [-c]` (first without `-c` to review, then with `-c` to commit).
  Also `touch hledger/Hledger/Cli/Version.hs` ?

- `./Shake cmdhelp [-c]`

- `./Shake mandates`

- `./Shake manuals [-c]`

- `make tag`

- `make hackageupload`

- push tags: magit `P t`

- push to CI branches to test & generate binaries
  - magit `P -f e origin/ci-windows`
  - ... `origin/ci-mac`
  - ... `origin/ci-linux-static`
  - ... `origin/ci-linux-static-arm32`

- in site: update `download.md`

- in site: update `relnotes.md`
  - copy template
  - add new changelog sections, omitting hledger-lib
  - add summary
  - add contributors

- update `doc/ANNOUNCE`
  - summary/contributors from release notes

- create github release
  - tag VER, title VER, body similar to previous release
  - at https://ci.hledger.org download each CI branch artifact when ready, to ~/Downloads/hledger-VER/
  - drag artifacts into github draft release
  - publish release

- announce
  - send ANNOUNCE to hledger@googlegroups.com, haskell-cafe@googlegroups.com
  - link release notes/summary in #hledger:matrix.org, #hledger:libera.chat
  - tweet at https://twitter.com/simonkwmichael
  - toot at https://fosstodon.org/web/accounts/106304084994827771

## Post release

- merge/check/update download page changes (docker, homebrew, nix, linux distros..)

- support

- handle issues

- update procedures, tools, docs
