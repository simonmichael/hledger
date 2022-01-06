# Releasing

<div class=pagetoc>
<!-- toc -->
</div>

Tips for hledger release managers and maintainers.

## Glossary

Here are terms and concepts related to the hledger release process as of 2022, 
in sufficient detail to guide release management and release automation.

|                                     |                                                                                                                                                                                                                                                                                                                                                                                                                            |
|-------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|                                     | **General concepts**                                                                                                                                                                                                                                                                                                                                                                                                       |
| *release*                           | A snapshot of the software and related artifacts like executable binaries, which is named, tagged, documented, announced, and usually picked up by packaging systems on various platforms.                                                                                                                                                                                                                                 |
| *version control system, VCS*       | A tool used for storing and sharing and viewing the history and different lines of development of a software project, or other set of files. hledger uses Git.                                                                                                                                                                                                                                                             |
| *repository, repo*                  | A set of files being stored and managed by a VCS. Often published on a *repository hosting service*, such as Github.                                                                                                                                                                                                                                                                                                       |
| *working copy, clone*               | A local copy of a repository's files. Typically each developer has one or more of these, and can share changes easily with the official public repository.                                                                                                                                                                                                                                                                 |
| *branch*                            | Some VCS's, including Git, can store multiple branching lines of development within one repository. A working copy can be quickly switched to a different branch to show its content.                                                                                                                                                                                                                                      |
| *master, main*                      | The main branch in a repo, usually named `master` or `main`. Pull requests are usually relative to this.                                                                                                                                                                                                                                                                                                                   |
| *pull request, PR*                  | A request to merge a development branch with master, and any related discussion. On Github, these are kept alongside issues in the issue tracker.                                                                                                                                                                                                                                                                          |
| *continuous integration, CI*        | Automated actions that run when new code is pushed to a shared repo, such as running tests or producing binaries. On Github this is called Github Actions and action scripts are called *workflows*.                                                                                                                                                                                                                       |
|                                     | **hledger concepts**                                                                                                                                                                                                                                                                                                                                                                                                       |
| *package*                           | A releasable unit of Haskell software. hledger has several core packages usually released together: hledger-lib, hledger, hledger-ui, hledger-web.                                                                                                                                                                                                                                                                         |
| *hledger version number*            | A 2-4 part dotted number naming a hledger release or hledger package version: `MA.JOR[.MINOR[.FIXUP]]` or `MA.JOR.99[.PREVIEW]` where 99 means "unreleased (MAJOR+1)". See examples below.                                                                                                                                                                                                                                 |
| *hledger version string*            | A line of text describing a hledger binary, shown by `--version`. It contains program name, version number, commit hash and date, machine architecture etc. Eg: `hledger 1.24.1-g7799d526b-20211210, mac-x86_64`                                                                                                                                                                                                           |
| *Complete, partial, mixed releases* | A release of all the core hledger packages (hledger-lib, hledger, hledger-ui, hledger-web) is called *complete*. A release of only some of the core packages is called *partial*. A release where some packages have different versions (because of a previous partial release) is called *mixed*. Major and preview releases are always complete, minor and fixup releases can be partial and/or mixed.                   |
| *changelog*                         | A CHANGES.md file listing the release history and the changes in each release. There is one for each hledger package and one for the hledger project as a whole.                                                                                                                                                                                                                                                           |
| *release notes*                     | The Release Notes page on the hledger website: the combined release history of the core hledger packages, showing user visible changes only.
|                                     | **hledger release/build types**                                                                                                                                                                                                                                                                                                                                                                                            |
| *Major release*                     | Major releases include new features and incompatible API changes, and normally happen at the start of each quarter's third month (3/1, 6/1, 9/1, 12/1). Example version number: `1.25`                                                                                                                                                                                                                                     |
| *Minor release*                     | Minor releases include only bug fixes, without API changes. These happen when needed, to fix significant bugs in the previous major release. Example version number: `1.25.2` (*"second bugfix release for 1.25"*)                                                                                                                                                                                                         |
| *Fixup release*                     | Fixup releases fix packaging errors, with no changes to the hledger software. These should be rare. Example version number: `1.25.0.1` or `1.25.2.1` (*"first fixup release for 1.25 / 1.25.2"*)                                                                                                                                                                                                                           |
| *Preview release*                   | A preview of the upcoming major release for testers/early adopters, and a test of the release process, published on Github. Not a formal hledger release, eg not published on Hackage, usually not packaged, no bugfix releases, no regression bounties, not shown in release notes. These typically appear in the quarter's first and second month if needed. Example version number: `1.25.99.1` (*"preview 1 of 1.26"*) |
| *CI binaries*                       | Temporary downloadable binaries produced by a run of the `linux`/`mac`/`windows` workflows in the hledger repo. This may happen periodically, eg weekly. Downloading requires a Github login.                                                                                                                                                                                                                              |
| *Dev build*                         | A local developer build of unreleased code. This is typically in `master` or a development/PR branch. Example version number: `1.25.99` (*"unreleased 1.26-dev"*)                                                                                                                                                                                                                                                          |
|                                     | **hledger repos and branches**                                                                                                                                                                                                                                                                                                                                                                                             |
| *hledger repo*                      | The `hledger` git repository, containing the hledger software, reference manuals, and developer docs. <https://github.com/simonmichael/hledger>                                                                                                                                                                                                                                                                            |
| *site repo*                         | The `hledger_website` git repository, containing most of the hledger website which appears at <https://hledger.org>. Usually checked out under the hledger repo as `site/`. <https://github.com/simonmichael/hledger_website>                                                                                                                                                                                              |
| *master*                            | The branch named `master` in the hledger repo; the main line of hledger development. Pull requests are usually relative to this.                                                                                                                                                                                                                                                                                           |
| *release&nbsp;branch*               | Branches named `MA.JOR-branch` in the hledger repo, eg `1.25-branch`. Releases and release previews are always made from a release branch.                                                                                                                                                                                                                                                                                 |

## Cadence

hledger major releases happen quarterly, at or near the start of the third month of each quarter. 
Here is the normal release/build schedule (as of 2022):

| Time                              | Events                                                                                               |
|-----------------------------------|------------------------------------------------------------------------------------------------------|
| First month                       | Preview 1 ("alpha"), if needed, from release branch                                                  |
| Second month                      | Preview 2 ("beta"), if needed, from release branch                                                   |
| Third month (3/1, 6/1, 9/1, 12/1) | Major release, from release branch; <br>Bump major version in master; <br>Create next release branch |
| As needed                         | Minor/fixup releases, from release branch                                                            |
| Weekly                            | CI downloadable binaries, produced weekly from master for mac & windows currently                    |
| On push to a PR or master         | Run CI tests in that branch                                                                          |

## 2021-12 tips

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
- All releases must be made from a release branch, for uniformity and to avoid mishaps like uploading 1.24.99 dev code to hackage.
- Don't tag until the three main platform binaries have been produced from the same commit.
- Update and edit changelogs as early and often as possible. Eg before or right after merging a PR, and before creating a release branch.

## Some next goals

- Update/consolidate release process docs.
- Develop a process for making test releases at any time.
- Establish routine weekly test releases.

## Release prerequisites

- release-ready master or release branch:
  - contains desired bugfixes
  - contains desired PRs
  - changelogs are up to date
  - tests pass
- packages to be released
- release version
- release date

## Procedures

### Preview release
- ensure master is in releasable state
- `./Shake changelogs`, edit, commit (`./Shake changelogs -c`)
- `tools/release prep OLDMA.JOR.99.PREVIEW` (eg 1.24.99.1 for 1.25 preview 1)
- edit changelogs, amend commit
- `make functest`
- `tools/release bin` (produce CI binaries)
- babysit/fix/repeat CI binary jobs until all succeed on the same commit
- `make tag`
- github release
  - push commits & tags
  - draft
  - test
  - publish 

### Major release
- as above, with MA.JOR
- other major release stuff - release notes, site updates, hackage, announce
- merge release changelog back to master
- bump version in master

### Minor/Fixup release
- switch to release branch
- cherry pick changes from master
- proceed as above, with MA.JOR.MINOR[.FIXUP]

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
  - `git push -f origin master:ci-linux-x64`
  - `git push -f origin master:ci-linux-arm32v7` (slow, unreliable)
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

