# Releasing

<div class=pagetoc>
<!-- toc -->
</div>

Tips for hledger release managers and maintainers.

## Glossary



|                                 |                                                                                                                                                                                                                                                                                                                                                                                                 |
|---------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|                                 | **Releases and versions**                                                                                                                                                                                                                                                                                                                                                                       |
| *Release*                       | A snapshot of the software and related artifacts like executable binaries, which is named, tagged, documented, announced, and usually picked up by packaging systems on various platforms.                                                                                                                                                                                                      |
| *Version number*                | A 2-4 part dotted number naming a hledger release, see examples below. For unreleased code it ends with .99, eg a post-1.25 / pre-1.26 build of master will show `1.25.99`.                                                                                                                                                                                                                     |
| *Version string*                | A line of text describing a hledger binary, shown by `--version`, containing program name, version number, commit hash and date, machine architecture etc. Eg: <br>`hledger 1.24.1-g7799d526b-20211210, mac-x86_64`                                                                                                                                                                             |
| *Full, partial, mixed releases* | A release of all the core hledger packages (hledger-lib, hledger, hledger-ui, hledger-web) is called *full*. A release of only some of the core packages is called *partial*. A *mixed* release is one where, because of a previous partial release, some packages have different versions. Minor releases are sometimes partial or mixed.                                                      |
|                                 | **hledger release types**                                                                                                                                                                                                                                                                                                                                                                       |
| *Major release*                 | hledger major releases include new features and incompatible API changes, and normally happen at the start of each quarter's last month (Mar 1, Jun 1, Sep 1, Dec 1). Example version number: `1.25`                                                                                                                                                                                            |
| *Minor release*                 | Minor releases include only bug fixes, without API changes. These happen only when needed, to fix significant bugs in a major release. Example version number: `1.25.2`                                                                                                                                                                                                                         |
| *Fixup release*                 | Fixup releases include only fixes for packaging errors, and no changes to the hledger software. These should be rare. Example version number: `1.25.0.1` or `1.25.2.1`                                                                                                                                                                                                                          |
| *Dev snapshot*                  | A tested snapshot of development code, usually from the master branch, on a certain date. These are not official hledger releases, and will not appear on Hackage, but are published as a Github release for testers/early adopters and to test the release process. They can happen at any time, eg at the start of a quarter's first or second month. Example version number: `1.25.20220102` |
| *CI build*                      | A particular run of the linux/mac/windows Github workflows in the hledger repo, producing downloadable binaries. These may run more often than releases/snapshots, eg weekly.                                                                                                                                                                                                                   |
|                                 | **Version control**                                                                                                                                                                                                                                                                                                                                                                             |
| *version control system, VCS*   | A tool used for storing and sharing and viewing the history and different lines of development of a set of files, especially the source files of a software project. hledger uses Git.                                                                                                                                                                                                          |
| *repository, repo*              | A set of files being stored and managed by a VCS. Often published on a *repository hosting service*, such as Github.                                                                                                                                                                                                                                                                            |
| *working copy, clone*           | A local copy of a repository's files. Typically each developer has one or more of these, and can share changes easily with the official public repository.                                                                                                                                                                                                                                      |
| *branch*                        | Some VCS's, including Git, can store multiple branching lines of development within one repository. A working copy can be quickly switched to a different branch to show its content.                                                                                                                                                                                                           |
|                                 | **hledger repos and branches**                                                                                                                                                                                                                                                                                                                                                                  |
| *hledger repo*                  | The `hledger` git repository, containing the hledger software, reference manuals, and developer docs. <br><https://github.com/simonmichael/hledger>                                                                                                                                                                                                                                             |
| *site repo*                     | The `hledger_website` git repository, containing most of the hledger website which appears at <https://hledger.org>. Usually checked out under the hledger repo as `site/`. <br><https://github.com/simonmichael/hledger_website>                                                                                                                                                               |
| *release&nbsp;branch*           | Branches named `MA.JOR-branch` in the hledger repo, eg `1.24-branch`. Releases are always made from a corresponding release branch (from 2022 on).                                                                                                                                                                                                                                              |
| *master*                        | The branch named `master` in the hledger repo; the main line of hledger development. Dev snapshots, and pull requests, can be made from here.                                                                                                                                                                                                                                                   |
|                                 |                                                                                                                                                                                                                                                                                                                                                                                                 |

## 2021-12 notes

- All the stuff below the horizontal rule has bitrotted already; consider it old and in need of review.
- Don't try to write down, let alone automate, every step of releasing; it's too much and too unstable.
- Practice releasing as often as possible.
- Keep making things a little better each time through. Simpler, more reliable, easier, faster, cheaper, higher quality.
- The different aspects of releasing have complex interdependencies and sequencing constraints.
  Chunk and separate them as far as possible:
  - **Software** - selecting changes, packages, release dates; coordinating contributions; ensuring release readiness
  - **Branch Management** - coordinating main and release branch, local and remote repos, CI branches
  - **Version Bumping** - choosing and applying new version numbers and related things like tags, github releases, urls, ghc and dep versions, stackage resolvers, everywhere needed
  - **Docs** - command help, manuals, changelogs, release notes, github release notes, install page, install scripts, announcements, process docs
  - **Testing** - local testing, CI testing, extra release-specific testing
  - **Artifacts** - generating binaries, zip files, github releases etc.
  - **Publishing** - uploading, pushing, making visible, finalising
  - **Announcing** - various announcement stages and channels
- All releases must now be made from a release branch, for uniformity and to avoid mishaps like uploading 1.24.99 dev code to hackage.

## Some next goals

- Update/consolidate release process docs.
- Develop a process for making test releases at any time.
- Establish routine weekly test releases.

----

## Review/update/consolidate:

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

- update `install.md`
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

- js/site.js: add NEW, update currentrelease
- Makefile: add NEW, two places
- make snapshot-NEW
- (cd src; rm current; ln -s NEW current)

In hledger.org caddy config:

- add `path` and `redir`s for NEW
- `systemctl reload caddy`

On hledger.org:

- make clean all

## Tips

- During pre/post release phases, update RELEASING.md in a copy,
  RELEASING2.md, to reduce commit noise and git interference.

