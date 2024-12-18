# RELEASING

<div class=pagetoc>

<!-- toc -->
</div>

Notes for hledger release managers and maintainers.

## Goals

**2024**
- [ ] Make releasing easier

**2023**
- [x] Make releasing eas<s>y</s>ier

**2022**
- [x] Update/consolidate release process docs
- [x] Establish routine <s>monthly</s> release cadence
- [ ] Make releasing easy

## Release types

hledger major releases happen each quarter, normally at the start of the third month (see [past releases](relnotes.md)).
Bugfix releases follow when needed.
Preview releases may happen in the other months if wanted.

|                     | Major&nbsp;release<br>A.B                                | Bugfix&nbsp;release<br>A.B.C | Fixup&nbsp;release<br>A.B.C.D                | Preview&nbsp;release<br>A.B.99.D         |
|---------------------|----------------------------------------------------------|------------------------------|----------------------------------------------|------------------------------------------|
| **Contains:**       | New features, breaking changes                           | Only bug fixes               | Trivial packaging fixes, no software changes | Early snapshot of the next major release |
| **When:**           | Third month of quarter: March, June, September, December | When needed                  | Never                                        | First & second months of quarter         |
|                     |                                                          |                              |                                              |                                          |
| **Deliverables:**   |                                                          |                              |                                              |                                          |
| Changelogs          | ✓                                                        | ✓                            | ✓                                            | ✓                                        |
| Github release      | ✓                                                        | ✓                            | ✓                                            | ✓                                        |
| Binaries            | ✓                                                        | ✓                            | ✓                                            | ✓                                        |
| Hackage release     | ✓                                                        | ✓                            | ✓                                            |                                          |
| Install page        | ✓                                                        | ✓                            | ✓                                            |                                          |
| hledger-install     | ✓                                                        | ✓                            | ✓                                            |                                          |
| [Regression bounty] | ✓                                                        | ✓                            | ✓                                            |                                          |
| Release notes       | ✓                                                        | ✓                            |                                              |                                          |
| Web manuals         | ✓                                                        |                              |                                              |                                          |
| Announcements       | ✓                                                        |                              |                                              |                                          |

[Regression bounty]: http://hledger.org/regressionbounty

## Release manager activities

These have complex interdependencies and sequencing constraints.
Chunk, separate, routinise, document and automate them as far as possible.

|                       |                                                                                                                                                            |
|-----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Software**          | selecting changes, packages, release dates; coordinating contributions; ensuring release readiness                                                         |
| **Branch Management** | coordinating main and release branch, local and remote repos, CI branches                                                                                  |
| **Version Bumping**   | choosing and applying new version numbers and related things like tags, github releases, urls, ghc and dep versions, stackage resolvers, everywhere needed |
| **Docs**              | command help, manuals, changelogs, release notes, github release notes, install page, install scripts, announcements, process docs                         |
| **Testing**           | local testing, CI testing, extra release-specific testing                                                                                                  |
| **Artifacts**         | generating binaries, zip files, github releases etc.                                                                                                       |
| **Publishing**        | uploading, pushing, making visible, finalising                                                                                                             |
| **Announcing**        | various announcement stages and channels                                                                                                                   |

## Glossary

Some terminology useful when precision is needed, eg in release scripts.

### General

**release**\
A snapshot of the software and related artifacts like executable binaries, which is named, tagged, documented, announced, and usually picked up by packaging systems on various platforms.

**version control system, VCS**\
A tool used for storing and sharing and viewing the history and different lines of development of a software project, or other set of files. hledger uses Git.

**repository, repo**\
A set of files being stored and managed by a VCS. Often published on a **repository hosting service**, such as Github.

**working copy, clone**\
A local copy of a repository's files. Typically each developer has one or more of these, and can share changes easily with the official public repository.

**branch**\
Some VCS's, including Git, can store multiple branching lines of development within one repository. A working copy can be quickly switched to a different branch to show its content.

**master, main**\
The main branch in a repo, usually named `master` or `main`. Pull requests are usually relative to this.

**pull request, PR**\
A request to merge a development branch with master, and any related discussion. On Github, these are kept alongside issues in the issue tracker.

**continuous integration, CI**\
Automated actions that run when new code is pushed to a shared repo, such as running tests or producing binaries. On Github this is called Github Actions and action scripts are called **workflows**.

**release engineering**\
<https://en.wikipedia.org/wiki/Release_engineering>

### hledger-specific

**package**\
A releasable unit of Haskell software. hledger has several core packages usually released together: hledger-lib, hledger, hledger-ui, hledger-web.

**hledger version number**\
A 2-4 part dotted number naming a hledger release or hledger package version: `MA.JOR[.MINOR[.FIXUP]]` or `MA.JOR.99[.PREVIEW]` where 99 means "unreleased (MAJOR+1)". See examples below.

**hledger version string**\
A line of text describing a hledger binary, shown by `--version`. It contains program name, version number, commit hash and date, machine architecture etc. Eg: `hledger 1.24.1-g7799d526b-20211210, mac-x86_64`

**Full release**\
A release of all four core hledger packages (hledger-lib, hledger, hledger-ui, hledger-web). Major and preview releases are always full releases.

**Partial release**\
A release of just some of the hledger packages. Bugfix and fixup releases are sometimes partial.

**Single-version release**\
A release where all packages have the same version. Major and preview releases are always single-version.

**Mixed-version release**\
A release where the packages have different versions, because of a previous partial release. Bugfix and fixup releases are sometimes mixed-version.

**changelog**\
A CHANGES.md file listing the release history and the changes in each release. There is one for each hledger package and one for the hledger project as a whole.

**release notes**\
The Release Notes page on the hledger website: the combined release history of the core hledger packages, showing user visible changes only.

### Releases and builds

**Major release**\
Major releases include new features and incompatible API changes, and normally happen at the start of each quarter's third month (3/1, 6/1, 9/1, 12/1). Example version number: `1.25`

**Bugfix release**\
Bugfix releases include only bug fixes, without API changes. These happen when needed, to fix significant bugs in the previous major release. Example version number: `1.25.2` (**"second bugfix release for 1.25"**)

**Fixup release**\
Fixup releases fix packaging errors, with no changes to the hledger software. These should be rare. Example version number: `1.25.0.1` or `1.25.2.1` (**"first fixup release for 1.25 / 1.25.2"**)

**Preview release**\
A preview of the upcoming major release for testers/early adopters, and a test of the release process, published on Github. Not a formal hledger release, eg not published on Hackage, usually not packaged, no bugfix releases, no regression bounties, not shown in release notes. These typically appear in the quarter's first and second month if needed. Example version number: `1.25.99.1` (**"preview 1 of 1.26"**)

**CI binaries**\
Temporary downloadable binaries produced by a run of the `linux`/`mac`/`windows` workflows in the hledger repo. This may happen periodically, eg weekly. Downloading requires a Github login.

**Dev build**\
A local developer build of unreleased code. This is typically in `master` or a development/PR branch. Example version number: `1.25.99` (**"unreleased 1.26-dev"**)

### Repos and branches

**hledger repo**\
The `hledger` git repository, containing the hledger software, reference manuals, and developer docs. <https://github.com/simonmichael/hledger>

**site repo**\
The `hledger_website` git repository, containing most of the hledger website which appears at <https://hledger.org>. Usually checked out under the hledger repo as `site/`. <https://github.com/simonmichael/hledger_website>

**finance repo**\
The `hledger_finance` git repository, containing the hledger project's financial ledger. Usually checked out under the hledger repo as `finance/`. <https://github.com/simonmichael/hledger_finance>

**master**\
The branch named `master` in the hledger repo; the main line of hledger development. Pull requests are usually relative to this.

**release&nbsp;branch**\
Branches named `MA.JOR-branch` in the hledger repo, eg `1.25-branch`. Releases and release previews are always made from a release branch.


## Release artifacts / value chain

Higher things depend on lower things.
Release readiness and the release process go from the bottom of this diagram to the top.


[![release diagram](RELEASING.png)](RELEASING.png)
<!-- source: RELEASING.canvas (Obsidian) -->

## Checklist
Waypoints and required artifacts.

- [x] product
  - [x] blocking defects resolved
  - [x] desired improvements landed and stabilised
  - [x] building and passing tests with current ghcs, deps, and stackage snapshots
  - [x] building and passing tests on all platforms  
        `just relbin` (or push to github `binaries[-*]` branch)
- [x] product docs and metadata
  - [x] release branch
  - [x] version strings (in **/.version, */.version.m4, */package.yaml)
  - [x] cabal files x 4 (hledger*/hledger*.cabal)  
        `just relbranch`
  - [x] options help texts up to date (in CliOptions.hs, UIOptions.hs, WebOptions.hs)
  - [x] embedded manuals x 3
    - [x] generaloptions macro (in doc/common.m4)
    - [x] tool specific options in manuals (hledger*/hledger*.m4.md > # Options)
    - [x] man page dates (*/.date.m4)
    - [x] man  (hledger*/hledger*.1)
    - [x] info (hledger*/hledger*.info)
    - [x] text (hledger*/hledger*.txt)  
        `./Shake manuals -c`
  - [x] embedded tldr pages synced with upstream (doc/tldr/*)  
        `just tldr-diff`
  - [x] embedded asciinema demos (hledger/embeddedfiles/*.cast)
  - [x] shell completions (hledger/shell-completion/hledger-completion.bash)  
        `just completions`, commit any changes
  - [x] changelogs x 5 (**/CHANGES.md)  
        `just changelogs [-c]`  
        `just changelogs-finalise`
- [x] release docs and artifacts
  - [x] draft binaries building started  
      `just relbin`
  - [x] hledger.org html manuals x 3 (site/src/VER/*.md)  
        `just site-manuals-snapshot VER`  
        update `site/Makefile`, `site/js/site.js`, `site/hledger.org.caddy`
  - [x] release notes @ hledger.org (doc/relnotes.md)  
      `just relnotes`,  
      select & transform with `md-issue-refs`,  
      uniquify issue refs,  
      unwrap long lines,  
      add author github nicks,  
      commit
  - [x] github binary install docs up to date and pre-tested (doc/ghrelnotes.md)
  - [x] hledger-install up to date and pre-tested (hledger-install/hledger-install.sh)  
      `./Shake hledger-install-version`,  
      check/update dep versions,  
      select/test snapshot version
  - [x] hledger.org Install page up to date and pre-tested (site/src/install.md)
  - [x] draft announcement for chat / mail list (doc/ANNOUNCE)
  - [x] draft announcement for mastodon (doc/ANNOUNCE.masto)
  - [x] release tags  
      `just reltags`
  - [x] release binaries built from tag  
      `just relbin`
- [x] published
  - [x] all packages uploaded correctly to hackage  
      `just hackageupload`
  - [x] hledger-install merged to master and pushed to github
  - [x] site repo pushed to github and hledger.org, new manuals rendering correctly
  - [x] release branch and tags pushed to github  
      `just reltags-push`
  - [x] github release with release binaries attached (https://github.com/simonmichael/hledger/releases/new)  
      should be autocreated by the above; if not, https://github.com/simonmichael/hledger/releases/new,
      `just ghrelnotes`, paste, download release binaries, upload release binaries, review  
      publish
  - [x] install instructions tested and working
    - [x] hledger-install
    - [x] stack
    - [x] cabal
    - [x] source checkout
    - [x] github release > How to install, each platform
  - [x] announced
    - [x] mail list(s) hledger@googlegroups.com, + haskell-cafe@googlegroups.com for major
    - [x] matrix
    - [x] irc
    - [x] pta forum
    - [x] mastodon
- [x] cleanup and support
  - [x] remaining relevant release branch work merged to master
  - [x] new dev tag/versions/man dates in master  
      `j devtag`
  - [x] pta.o project stats updated
  - [x] process notes updated/cleaned
  - [x] monitor/support/handle issues:  
      [issue tracker](https://github.com/simonmichael/hledger/issues?q=is%3Aopen+is%3Aissue), matrix, irc, mail list, forum, reddit

After release, it's a good time for:

  - finance repo updates
  - hledger.org doc updates
  - pta.o doc updates

## How-to notes

More detailed procedure notes.

### General tips

- Release, or practice releasing, often to improve the process.

- Use and continually update RELEASING.md.
  Document procedures and gotchas to save time and enable automation in future.

- Also the diagram (RELEASING.canvas, made with Obsidian).

- But don't document prematurely or in too much detail.

- Make things a little better each time through: simpler, more reliable, better documented, more automated, easier, faster, cheaper, higher quality.

- Optionally save this file as RELEASING2.md and update notes there until after release, if it's interfering with git branch switching.

- Use and update scripts, in `Justfile`, `Shake.hs`, `tools/` etc.

- Do all releases from a release branch.

- Update dev changelogs frequently in master. Finalise changelogs in the release branch. Merge back to master after release.
  (Related older doc: [CHANGELOGS](CHANGELOGS.md))

- All release binaries should be built from the release-tagged commit.
  The binaries' --version output should match the release tag and release date.

- Try to do only full releases including all four main hledger packages; partial releases add complexity.

- Try to avoid pre-announcing a hard release date. 
  It will always take more time than you think,
  if you go late you might miss your intended date in many timezones,
  and there's no point adding unnecessary pressure.

- The biggest potential time sinks are:

  - reviewing/relearning the process/docs/infrastructure
  - updating/improving the process/docs/infrastructure
  - preparing changelogs
  - building binaries for all platforms
  - troubleshooting github workflow issues
  - followup work due to release mistakes, bugs in new features, or regressions

- Hard/risky/intensive tasks should happen without time pressure;
  during the final countdown, things should be easy.

### Check release readiness

- Any blocking open issues ? <https://bugs.hledger.org>
- Any blocking open PRs ? <https://prs.hledger.org>
- Any blocking items on <https://hledger.org/ROADMAP.html> ?
- Any blocking items in personal notes & backlogs ?

### Prepare release

In main repo, release branch:
1. Create/switch to release branch, update versions/dates/docs: `just relprep NEW` (single-version releases; for mixed-version releases, take more care)
1. If not the first release in this branch, cherry-pick changes from master: `magit l o ..master` (minor releases)
1. Update shell completions: `make -C hledger/shell-completion`
1. (Could start building/testing/fixing release binaries/workflows/caches here, it takes time: `just relbin`)
1. Update install script: `hledger-install/hledger-install.sh`
1. Update changelogs (`**/CHANGES.md`): `./Shake changelogs`, manually edit, `./Shake changelogs -c`
1. Update release notes (`doc/relnotes*`): `tools/relnotes.hs`, select & transform with `md-issue-refs`, uniquify issue refs, add github nicks to authors, commit\
   *TODO:
    tools/relnotes.hs: last release's summary, headings, unwrap long lines
    md-issue-refs: uniquify*
1. Update announcements (`doc/ANNOUNCE*`) (major releases)
1. Build/test release binaries: `just relbin`. Troubleshoot/repeat as needed.

In site repo:
1. Update config in `hledger.org.caddy` (@oldmanpath, @unversionedmanpath, any new redirects) (major releases, usually)
1. [Update online manuals](#release-manuals): bump versions in `site/Makefile`, `site/js/site.js`, `make -C site snapshot-NEW` (major releases)
   *TODO: snapshot: don't switch to master, don't discard uncommitted changes, record git hash in commit message, clarify late update procedure*
1. Update install page: `site/src/install.md`
1. Don't push yet. Keep in local branch if needed.

In main repo, master:
1. Cherry-pick the hledger-install update, and other finished useful updates, from the release branch (maybe not release docs yet): `magit l o LASTREL..REL-branch`
1. [Bump version](#bump-master-to-next-version) in master (major releases)
1. Add a new dev tag on the "bump version to ..." commit (magit `t t REL.99`)

### Publish release

In main repo, release branch:
1. Build final release binaries (`just relbin`) and tag the release (`just reltag`)
1. Push release branch & tags (not more than 5 tags at once):\
   `git push github HEAD REL hledger-REL hledger-lib-REL`\
   `git push github HEAD hledger-ui-REL hledger-web-REL`\
   Don't push all tags (don't push the dev tag; if you do, manually delete the corresponding draft release.)\
1. release.yml creates a draft release when the REL tag is pushed. Check/fix its content.
   *TODO: fix release.yml*
1. Publish on hackage (final check): `just hackageupload`
1. Publish github release

In main repo, master:
1. Push master: `just push`

In site repo:
1. Push to github (& site): `git push github` or magit `P p`

In hledger.org [cloudflare caching settings](https://dash.cloudflare.com/f629035917dd3b99b1e37ae20c15ff09/hledger.org/caching/configuration):
1. Custom Purge `https://hledger.org/js/site.js`  (major release)

On hledger.org VPS:  (major release, usually)
1. Restart caddy to enable new redirects
1. Test https://hledger.org/hledger.html redirect
1. Test manuals are displaying and highlighting the new version
1. If needed, `make buildall`

### Announce

(major releases, others if needed)
1. Update hledger entry at https://plaintextaccounting.org/#pta-apps
1. hledger matrix & irc chats
1. PTA forum
1. hledger mail list (& optionally haskell-cafe)
1. mastodon with #hledger and #plaintextaccounting tags

### Post release

1. Cherry-pick any final useful updates from the release branch (eg release docs): `magit l o LASTREL..REL-branch`
1. Add/commit any process updates: `doc/RELEASING.md`
1. Monitor packaging status (stackage, brew, docker, linux, nix etc); keep install page updated
1. Monitor, follow up on issues, especially regressions; keep doc/REGRESSIONS.md updated

### Up-to-date tools
- Check for consistent stackage snapshot(s) and extra deps used in
  stack.yaml, Shake.hs, hledger-install.sh, bin scripts, tools scripts
- Shake binary is up to date
  `./Shake.hs`
- `hpack --version` matches the one in `stack --version`

### Local tests
- `just test`
- `just doctest`
- `just haddocktest`

### CI tests
- push to a PR, wait for green
- or push to `ci` branch, wait for green at <https://ci.hledger.org>
- or `just push` (pushes to `ci`, then to `master`)

### Release branch tests
- `just test`
- `stack exec -- hledger --version`, check version, hash, release date, no '+'
- `stack exec -- hledger help | tail`, check version, month matches release

### Download release binaries
With all platform binary tests green on same commit
- clear out any old `hledger*` from local Downloads dir
- in each github binary job: in safari, right click the artifact, Download linked file
- optional: unpack and save the github binaries for local platform (mac-arm64)
- optional: also build native local binaries from that same commit: `just installallas VER`

### Update Install page
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

### Update RELEASING.png
- edit RELEASING.canvas in obsidian
- CMD-p > Export as image, don't show logo
- commit

### Monitor packaging status, update Install page
  - homebrew - expect badge to update soon
  - docker - expect/merge PR
  - nix - expect badge to update after a few days; can check with `make nix-hledger-version`
  - linux distros - once in a while, follow the links & search for newer versions, update

### Update hledger in stackage

- monitor packaging status in lts and nightly: <https://www.stackage.org/package/hledger>
- update <https://github.com/fpco/stackage/blob/master/build-constraints.yaml> as needed

### Update homebrew formula

Usually automatic; contacts: chenrui, other past packagers, Athas on #haskell

<!--
Old:

1.   <https://docs.brew.sh/How-To-Open-a-Homebrew-Pull-Request>

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

-->
