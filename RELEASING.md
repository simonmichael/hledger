# Releasing

<div class=pagetoc>
<!-- toc -->
</div>

Guidance for release managers and maintainers.

## Terminology

|                         |                                                                                       |
|-------------------------|---------------------------------------------------------------------------------------|
| *OLD*                   | previous release version, eg `1.22` or `1.22.1`                                       |
| *NEW*                   | new release version, eg `1.22.2` or `1.23`                                            |
| *MAJORVER*              | just the major version part, eg `1.22` or `1.23`                                      |
| *master (branch)*       | `master` branch in the `hledger` repo, ie the main line of development                |
| *release&nbsp;(branch)* | a release branch in the `hledger` repo, eg `1.22-branch`                              |
| *site*                  | `master` branch in the `hledger_website` repo. Usually checked out as `hledger/site`. |
|                         |                                                                                       |

## Phases of release cycle:


### 0. Dev
  
Normal development, on master and PR branches.


### 1. Pre-release
  
Preparations to make just before a release.

#### Resolve issues

Review, select, resolve PRs and issues.

#### Polish changelogs

Complete and polish changelogs.

#### Plan release

Plan the release number and any extra release-time activities.


### 2. Release

The sequence of steps to follow when making a release.

#### Freeze

- Set version.
- Finalise changelogs.
- Generate release notes.
- Prepare announcement.
- Tag.
- Generate CI release binaries.
- Draft github release.
- 24 hour release countdown with no changes.
- If any problems found, return to Pre-release.

#### Publish

- Website changes.
  - release notes
  - install page
  - manuals
  - webserver redirects
- Publish hackage packages.
- Push tags.
- Publish github release.
- Publish website changes.
- Announce

### 3. Post-release

Monitor, support, respond.


## Release preparation detail

### Any time before release

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

### On release day

In master:

- `./Shake.hs` to ensure `Shake` is current, review commands

- finalise [changelogs](CHANGELOGS.html), copy each new section (emacs `C-x r s a` ... `b` ... `c` ... `d`)

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
  - `git push -f origin master:ci-windows`  (or magit `P -f e origin/ci-windows`)
  - `git push -f origin master:ci-mac`
  - `git push -f origin master:ci-linux`
  - `git push -f origin master:ci-linux-static` (at release time only))
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

## Release detail

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

## Post release detail

- merge/check/update download page changes
  - docker - expect/merge PR
  - homebrew - expect badge to update soon
  - nix - expect `make nix-hledger-version` to update after a few days, find and update to that commit hash
  - linux distros - once in a while, follow the links & search for newer versions, update

- support

- handle issues

- update procedures, tools, docs

## Add major release to website

In site: 

- make snapshot-NEW
- (cd src; rm current; ln -s NEW current)

- js/site.js: add NEW, update currentrelease

In hledger.org caddy config:

- add `path` and `redir`s for NEW
- `systemctl reload caddy`

## Tips

- During pre/post release phases, update RELEASING.md in a copy,
  RELEASING2.md, to reduce commit noise and git interference.

