# RELEASING

<div class=pagetoc>

<!-- toc -->
</div>
Notes for hledger release managers and maintainers.

## Goals

### 2023
- [ ] Make releasing easy

### 2022
- [x] Update/consolidate release process docs
- [x] Establish routine <s>monthly</s> release cadence
- [ ] Make releasing easy

## Release types
|                       | Major&nbsp;release<br>A.B      | Bugfix&nbsp;release<br>A.B.C | Fixup&nbsp;release<br>A.B.C.D                | Preview&nbsp;release<br>A.B.99.D         |
|-----------------------|--------------------------------|------------------------------|----------------------------------------------|------------------------------------------|
| **Contains:**         | New features, breaking changes | Only bug fixes               | Trivial packaging fixes, no software changes | Early snapshot of the next major release |
| **When:**             | Third month of quarter: March, June, September, December | When needed                  | Never                                        | First & second months of quarter |
|                       |                                |                              |                                              |                                          |
| **Deliverables:**     |                                |                              |                                              |                                          |
| Changelogs            | ✓                              | ✓                            | ✓                                            | ✓                                        |
| Github release        | ✓                              | ✓                            | ✓                                            | ✓                                        |
| Binaries              | ✓                              | ✓                            | ✓                                            | ✓                                        |
| Hackage release       | ✓                              | ✓                            | ✓                                            |                                          |
| Install page          | ✓                              | ✓                            | ✓                                            |                                          |
| hledger-install       | ✓                              | ✓                            | ✓                                            |                                          |
| [Regression bounty]   | ✓                              | ✓                            | ✓                                            |                                          |
| Release notes         | ✓                              | ✓                            |                                              |                                          |
| Web manuals           | ✓                              |                              |                                              |                                          |
| Announcements         | ✓                              |                              |                                              |                                          |

[Regression bounty]: http://hledger.org/regressionbounty

hledger major releases happen in the third month of each quarter, normally at or close to the start of the month.
Preview releases happen in the other months, if needed.
Here's the ideal release schedule:

| Q1               | Q2               | Q3               | Q4               |
|------------------|------------------|------------------|------------------|
| Jan 1: preview 1 | Apr 1: preview 1 | Jul 1: preview 1 | Oct 1: preview 1 |
| Feb 1: preview 2 | May 1: preview 2 | Aug 1: preview 2 | Nov 1: preview 2 |
| Mar 1: major     | Jun 1: major     | Sep 1: major     | Dec 1: major     |


## Glossary
Here are some definitions, useful eg when executing or automating release procedures.

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
| *Complete, partial, mixed releases* | A release of all the core hledger packages (hledger-lib, hledger, hledger-ui, hledger-web) is called *complete*. A release of only some of the core packages is called *partial*. A release where some packages have different versions (because of a previous partial release) is called *mixed*. Major and preview releases are always complete, bugfix and fixup releases can be partial and/or mixed.                  |
| *changelog*                         | A CHANGES.md file listing the release history and the changes in each release. There is one for each hledger package and one for the hledger project as a whole.                                                                                                                                                                                                                                                           |
| *release notes*                     | The Release Notes page on the hledger website: the combined release history of the core hledger packages, showing user visible changes only.                                                                                                                                                                                                                                                                               |
|                                     | **hledger release/build types**                                                                                                                                                                                                                                                                                                                                                                                            |
| *Major release*                     | Major releases include new features and incompatible API changes, and normally happen at the start of each quarter's third month (3/1, 6/1, 9/1, 12/1). Example version number: `1.25`                                                                                                                                                                                                                                     |
| *Bugfix release*                    | Bugfix releases include only bug fixes, without API changes. These happen when needed, to fix significant bugs in the previous major release. Example version number: `1.25.2` (*"second bugfix release for 1.25"*)                                                                                                                                                                                                        |
| *Fixup release*                     | Fixup releases fix packaging errors, with no changes to the hledger software. These should be rare. Example version number: `1.25.0.1` or `1.25.2.1` (*"first fixup release for 1.25 / 1.25.2"*)                                                                                                                                                                                                                           |
| *Preview release*                   | A preview of the upcoming major release for testers/early adopters, and a test of the release process, published on Github. Not a formal hledger release, eg not published on Hackage, usually not packaged, no bugfix releases, no regression bounties, not shown in release notes. These typically appear in the quarter's first and second month if needed. Example version number: `1.25.99.1` (*"preview 1 of 1.26"*) |
| *CI binaries*                       | Temporary downloadable binaries produced by a run of the `linux`/`mac`/`windows` workflows in the hledger repo. This may happen periodically, eg weekly. Downloading requires a Github login.                                                                                                                                                                                                                              |
| *Dev build*                         | A local developer build of unreleased code. This is typically in `master` or a development/PR branch. Example version number: `1.25.99` (*"unreleased 1.26-dev"*)                                                                                                                                                                                                                                                          |
|                                     | **hledger repos and branches**                                                                                                                                                                                                                                                                                                                                                                                             |
| *hledger repo*                      | The `hledger` git repository, containing the hledger software, reference manuals, and developer docs. <https://github.com/simonmichael/hledger>                                                                                                                                                                                                                                                                            |
| *site repo*                         | The `hledger_website` git repository, containing most of the hledger website which appears at <https://hledger.org>. Usually checked out under the hledger repo as `site/`. <https://github.com/simonmichael/hledger_website>                                                                                                                                                                                              |
| *master*                            | The branch named `master` in the hledger repo; the main line of hledger development. Pull requests are usually relative to this.                                                                                                                                                                                                                                                                                           |
| *release&nbsp;branch*               | Branches named `MA.JOR-branch` in the hledger repo, eg `1.25-branch`. Releases and release previews are always made from a release branch.                                                                                                                                                                                                                                                                                 |


## Activities
Release manager activities/responsibilities include:
- **Software** - selecting changes, packages, release dates; coordinating contributions; ensuring release readiness
- **Branch Management** - coordinating main and release branch, local and remote repos, CI branches
- **Version Bumping** - choosing and applying new version numbers and related things like tags, github releases, urls, ghc and dep versions, stackage resolvers, everywhere needed
- **Docs** - command help, manuals, changelogs, release notes, github release notes, install page, install scripts, announcements, process docs
- **Testing** - local testing, CI testing, extra release-specific testing
- **Artifacts** - generating binaries, zip files, github releases etc.
- **Publishing** - uploading, pushing, making visible, finalising
- **Announcing** - various announcement stages and channels

These have complex interdependencies and sequencing constraints. Chunk, separate, routinise, document and automate them as far as possible.

## Procedures

### Map

Here's a map of the value flows/artifacts/dependencies in a hledger release (arrows mean "depends on").
To do a release, start at the bottom of the diagram and work up
(and work down through the procedures described below).

[![release diagram](RELEASING.png)](RELEASING.png)
<!-- source: RELEASING.canvas (Obsidian) -->

### General tips
- Release (or practice releasing) often to improve the process.
- Use and continually update RELEASING.md and RELEASING.canvas.
- At the start of any release work, copy these (eg Obsidian > CMD-p > Make a copy of this file). Edit in the copies until done, to avoid blocking git branch switching. (Likewise if editing other docs, like CHANGELOGS.md.)
- Don't document procedures in too much detail / prematurely. 
- Make things a little better each time through: simpler, more reliable, better documented, more automated, easier, faster, cheaper, higher quality.
- `make`, `./Shake` and `./bake`.
- Update changelogs early and often, eg during/after a PR, to spread the work. 
  See also [CHANGELOGS](CHANGELOGS.html).
- Do releases from a release branch, not from master.
- All platform binaries should be built from the same commit, the one with the release tags.
- Binaries' --version shows their git hash and build date; these should match the release tag and release date.                                                   

### LEVEL 1 - DEV

#### Check dev readiness
- open issues (bugs), https://bugs.hledger.org
- open PRs, https://prs.hledger.org
- project boards, https://github.com/simonmichael/hledger/projects?query=is%3Aopen
- https://hledger.org/ROADMAP.html, https://hledger.org/BACKLOG.html
- personal notes & backlogs

### LEVEL 2 - TEST

Give it a few shakes.

#### Up-to-date tools
- ./Shake is up to date
  - Shake.hs uses same resolver, extra deps etc. as stack.yaml
  - Shake binary is up to date (`./Shake.hs`)
  - commit any changes (message: "tools: shake")

#### Up-to-date cabal files
- `./Shake cabalfiles`
- if there are changes, `./Shake cabalfiles -c`

#### Up-to-date help
- `./Shake cmdhelp`
- if there are changes, `./Shake cmdhelp -c`

#### Up-to-date manuals
- `./Shake mandates`
- `./Shake manuals`
- if there are changes, `./Shake manuals -c`

#### Up-to-date changelogs
In main repo, master branch:
- `./Shake changelogs`
- clean up the five `CHANGES.md`s
- `./Shake changelogs -c`
See [CHANGELOGS](CHANGELOGS.md).

#### Passing tests and CI
- local tests, mimicking CI
  - `make test`
  - `make doctest`
  - `make haddocktest`
- single-platform CI tests in github main repo master
  (if needed; not easy for a release branch, multi-platform CI tests will come later)
  - push to a PR, wait for green
  - or push to `simon` branch, wait for green at http://ci.hledger.org
  - or `tools/push` (pushes to `simon`, then to `master`)

### LEVEL 3 - RELEASE DOCS

Bake it ? Try `bash -x ./bake prep VERSION` next time. Warning, avoid possible bug with being in master instead. Does a bunch of stuff including partly finalising changelogs.

#### Release branch

Bugfix/fixup release:
- switch to release branch, cherry pick changes from master

Preview/major release:
- `PAUSE=1 ECHO=1 ./bake prep MA.JOR[.99.PREVIEWNUM]` (eg 1.24.99.1 for 1.25 preview 1)
  (XXX seems to go wrong without PAUSE`)
- clean up changelogs, amend changelogs commit (see also [CHANGELOGS](CHANGELOGS.html))
- cherry pick changes from master (if needed)
  - list changes in three side-by-side magit windows
    - 1. NEW IN MASTER: `l o MAJORVER-branch..master`, `M-x magit-toggle-buffer-lock`, `M-x toggle-window-dedicated` (`C-c D`)
    - 2. HEAD: regular magit status view
    - 3. RELEASE BRANCH: `l o MAJORVER-branch`, `M-x magit-toggle-buffer-lock`, `M-x toggle-window-dedicated`
  - in master window, working from bottom upward, cherry-pick all non-conflicting changes
    - skip changes already in release branch
    - skip changelog, command help, and manuals updates
- release branch testing
  - `stack build --test`
  - `make functest`

#### Release version number
- Choose new version (NEW): MA.JOR, MA.JOR.MINOR, or MA.JOR.MINOR.FIXUP
- In release branch do `./Shake setversion -c NEW`

#### Release changelogs
- add release version/date headings manually (or fix `bake prep`, then clean up manually)

#### Release notes
In site repo, update `src/release-notes.md`:
- copy template, uncomment
- replace date
- replace XX with NEW
- add new content from changelogs, excluding hledger-lib
- remove any empty sections
- add contributors, `git shortlog -sn OLD..NEW`
- add summary (major release) or remove it (bugfix release)
- site repo commit: `relnotes: NEW`

### LEVEL 4 - RELEASE BINARIES

#### Release build tests
  - touch/change Version.hs to encourage recompilation
  - `stack build`
  - `stack exec -- hledger --version`, check version, hash, release date, no '+'
  - `stack exec -- hledger help | tail`, check version, month matches release

#### Platform CI tests
Multi-platform CI tests:
- push to `github/binaries` (or `./bake bin`)
- resolve failures
- wait for green on all platforms

#### Release binaries
Once all platform CI tests are green:
- in local downloads dir, delete any zip files from last release
- in each successful platform job: right click, Download linked file
- unpack the zip file for local platform
- on that same commit, build native local binaries:
  - `make install-as-VERSION`

#### hledger-install script
(major/bugfix/fixup release)
- update `hledger-install/hledger-install.sh`
  - HLEDGER_INSTALL_VERSION (release date)
  - hledger official packages (NEW)
  - hledger third-party packages (latest versions on hackage/pypi)
  - RESOLVER and EXTRA_DEPS (same as stack.yaml, or one of them)
- test ? (won't work until new hledger packages are on hackage)
  `cd; bash ~/src/hledger/hledger-install/hledger-install.sh`
- commit: `install: NEW`

### LEVEL 5 - RELEASED

#### Pre-release pause
- stop, go afk, take a break
- review time, energy, availability, decide go/no-go

#### Pre-release tests
Sanity checks:
- appropriate dates/versions in changelogs and release notes (if late in day, watch for time zone issues)
- hledger-install script
  - `rg '^HLEDGER(_\w+)?_VERSION' hledger-install/hledger-install.sh`
- binaries' --version output
  - `cd ~/Downloads`
  - `./hledger --version`
  - `./hledger-ui --version`
  - `./hledger-web --version`
- binaries' man pages
  - `./hledger --man | tail -1`
  - `./hledger-ui --man | tail -1`
  - `./hledger-web --man | tail -1`

#### Release tag
- ensure new version has been set first with Shake or bake
- ensure no new commits have been made since push to `github/binaries`
- don't run this in an attempt to make editor tags files
- in the release branch (?): `make tag`

#### Github release
- in main repo, release branch:
  - `git push github MA.JOR-branch` or magit `P p`
  - `git push --tags` or magit `P t github`
- create new release, https://github.com/simonmichael/hledger/releases/new
- choose release tag
- title: VERSION
- description:
  - copy doc/github-release-doc.tmpl.md to editor
  - insert latest release notes (minus topmost heading) from site/src/release-notes.md
  - replace A-BB, A.BB versions
  - copy & paste
  - preview, sanity check
- upload platform binary zip files
- Save draft
- (and after successful hackage upload: Publish release)

#### Hackage packages
in main repo, release branch:
- `make hackageupload` (major/bugfix/fixup release)

#### Release manuals
(major release)

In site repo:

- js/site.js: add NEW, 3 places
- Makefile: add NEW, 2 places
- commit: `manuals: add NEW`
- make snapshot-NEW (after ensuring main repo has been release-tagged)
- push

### LEVEL 6 - PUBLISHED

#### Install page
(major/bugfix/fixup release)

In site repo:
- update `install.md`
  - query-replace OLD -> NEW in 
    - "current hledger release"
    - CI binaries badges/links, including linux-static-arm32v7 if built
    - "building from source"
    - stack install command
    - cabal install command
  - query-replace OLD-brightgreen -> OLD-red
  - only after release binaries are built (preferably after release is published):
    update --version outputs (version, hash, date, but not platform)
  - final output line from `hledger test` (run local build and in terminal for normal speed)
  - Total count from `make functest`
  - preview
  - commit: `install: NEW`

#### Update website
- push to github: in site repo, `git push github` or magit `P u`
- on hledger.org server:
  - in main repo (both main and site repos on master):
    - `git -C site pull && git pull && make site`
    - (or to rebuild all pages, also run `make -C site all`)
  - in /opt/hledger/site/hledger.org.caddy:
    - @oldmanpath: add `path` for NEW
    - @unversionedmanpath: update to NEW
  - `systemctl reload caddy`
- check site in browser
  - install page versions, badge colours
  - manuals' versions list in browser. If not updating, try
    - checking https://hledger.org/js/site.js
    - another browser (not safari)
    - shift-reload
    - https://dash.cloudflare.com/f629035917dd3b99b1e37ae20c15ff09/hledger.org/caching/configuration > purge
  - redirects
    - hledger.org/hledger.html redirects to https://hledger.org/NEW/hledger.html

### LEVEL 7 - ANNOUNCED

#### Prepare announcements
(major/notable bugfix release)

In release branch:

- update `doc/ANNOUNCE` (major release)
  - summary, contributors from release notes
  - any other edits
  - commit: `;doc: announce`

#### Announce
(major/bugfix release)

- update last release date on plaintextaccounting.org
- share github release link and release notes markdown in #hledger chat
- send ANNOUNCE as email announcement
  - (major release): `ANN: hledger NEW` to hledger@googlegroups.com, haskell-cafe@googlegroups.com
  - (bugfix release): brief announcement to hledger@googlegroups.com 
- condense to 500 & post at https://fosstodon.org/@simonmic
- maybe condense to 140 & post at https://twitter.com/simonkwmichael


### POST RELEASE

#### Merge release branch changes to master
- switch back to master
- check out release branch in another working copy (hledger2)
- manually merge release changelogs into master changelogs (see also [CHANGELOGS](CHANGELOGS.html))
- list commits only in release branch: magit `l o master..MA.JOR-branch`
- cherry-pick any other useful commits

#### Bump master to next version
(major release)
- `./Shake setversion MA.JOR.99 -c`
- `./Shake cmdhelp [-c]`  (might be empty)
- `./Shake mandates`
- `./Shake manuals -c`

#### Commit RELEASING.md
- move copies back to RELEASING.md, RELEASING.canvas
- re-export RELEASING.png: obsidian > CMD-p > Export as image, don't show logo
- commit

#### Push master
in main repo, master branch:
- pass CI checks in dev branch:
  - ensure latest commit will trigger CI (does not begin with semicolon) (?)
  - `git push github master:simon` or magit `P e github/simon`, with `-f` if needed
  - wait for CI success at http://ci.hledger.org
- `git push github master` or magit `P u`

#### Post-release followup
- monitor packaging status, update install page
  - docker - expect/merge PR
  - homebrew - expect badge to update soon
  - nix - expect `make nix-hledger-version` to update after a few days, find and update to that commit hash
  - linux distros - once in a while, follow the links & search for newer versions, update
- provide support, monitor issues
- prepare followup releases if needed
- update process docs and tools

#### Update stackage

1.  update
    <https://github.com/fpco/stackage/blob/master/build-constraints.yaml>
    if needed

2.  monitor for new package versions in nightly: check
    <https://www.stackage.org/package/hledger>
    
#### Update nix install command

1.  Wait for the new hledger version to land in nixpkgs

    1.  make nix-hledger-version

2.  Find the nixpkgs haskell-packages commit post-dating the hledger
    release:

    1.  make nix-view-commits \#
        <https://github.com/NixOS/nixpkgs/commits/master/pkgs/development/haskell-modules/hackage-packages.nix>

3.  Test the nix-env install command with that commit hash

    1.  if it fails with \"SSL peer certificate or SSH remote key was
        not OK\"

        1.  . *Users/simon*.nix-profile/etc/profile.d/nix.sh

        2.  or re-install:

            1.  curl -sO <https://nixos.org/nix/install>

            2.  less install

            3.  sh install

            4.  . *Users/simon*.nix-profile/etc/profile.d/nix.sh

    2.  on linux

    3.  on mac

#### Update homebrew formula (old)

1.  ref

    1.  helpers: chenrui & stack issue commenters on github, Athas on
        #haskell

    2.  <https://docs.brew.sh/How-To-Open-a-Homebrew-Pull-Request>

    3.  doc

        If a URL is specified, the SHA-256 checksum of the new download
        should also be specified. A best effort to determine the SHA-256
        and formula name will be made if either or both values are not
        supplied by the user.

        If a tag is specified, the Git commit revision corresponding to
        that tag must also be specified.

        Note: this command cannot be used to transition a formula from a
        URL-and-SHA-256 style specification into a tag-and-revision
        style specifi- cation, nor vice versa. It must use whichever
        style specification the for- mula already uses.

2.  update homebrew working copy

    1.  cd \~/src/DEVTOOLS/homebrew-core

    2.  git reset --hard HEAD\~1 && git fetch origin && git merge
        origin/master && git push -f

3.  get release tarball checksums

    export V=X.Y; for P in -lib \"\" -ui -web; do curl -sO
    <https://hackage.haskell.org/package/hledger$P-$V/hledger$P-$V.tar.gz>;
    done && shasum -a256 \*.gz

4.  update Formula/hledger.rb

    1.  update tarball urls, shas

    2.  update other version references

    3.  maybe update stackage snapshot

    4.  possible new process, see:
        <https://docs.brew.sh/How-To-Open-a-Homebrew-Pull-Request#create-your-pull-request-from-a-new-branch>

    5.  test (if possible without too many mac/brew hassles)

        1.  brew audit --strict Formula/hledger.rb \# some tests are
            post-install, try also after brew upgrade

        2.  brew upgrade Formula/hledger.rb -s -n -v \# \[scrub download
            cache, dry run, verbose\]

            1.  super slow, how to speed up ?

                1.  comment out stack update

                2.  add , \"--stack-root=/Users/simon/.stack\" to stack
                    install \# fails on permissions

        3.  brew test Formula/hledger.rb

    6.  commit with name \"hledger X.Y\" and description \"Update to
        hledger X.Y.\"

5.  get merged

    1.  git push -f simonmichael

    2.  gh pr create -f

    3.  monitor: PR CI, PR merge,
        <https://formulae.brew.sh/formula/hledger> page

    4.  ping brew contributors/maintainers if necessary: @chenrui,
        @carlocab, @SMillerDev

