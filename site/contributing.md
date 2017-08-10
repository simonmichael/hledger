# Contributor guide

<style>
#toc > ol > li > a { display:none; }
#toc > ol > li > ol > li { padding-left:0; }
</style>
* toc

New contributors are always welcome in the hledger project. 
Jump in! Or [ask us](/docs.html#getting-help) to help you find a task.


## Get started as a...

### Funder

Become a financial backer to
sustain and grow this project,
increase your influence,
express gratitude,
build prosperity consciousness,
and help transform world finance!

- Use the donate links on the [home page](/)
- Configure a recurring donation
- Contribute or pledge bounties on issues you care about
- Ask your organization to contribute
- Work on project sustainability, accountability, fundraising

### Tester

- Test installation on platforms you have access to
- Test examples, advice, and links in the docs
- Run the latest release or developer build in daily use
- Run [tests](#tests)
- Run [benchmarks](#benchmarking)
- Report packaging, documentation, UX, functional, performance issues
- Report and help analyse problems via irc/mail list/bug tracker

When reporting bugs, don't forget to search the tracker for a similar bug report.
Otherwise, open a new bug by clicking "New issue", or <http://bugs.hledger.org/new>.

Enhancement requests are sometimes added to the tracker,but for these consider using
the IRC channel and mail list (see [Getting help](/docs.html#getting-help)).
Both are archived and linkable, so the idea won't be lost.
There is also a collection of wishes at the old [trello board](http://trello.hledger.org).

### Developer

#### Review code

- review and discuss new [pull requests](http://prs.hledger.org) and commits on github
- build hledger and test the latest changes in your own repo
- read the existing [code docs and source](#quick-links)
- send feedback or discuss via [IRC or mail list](/docs.html#get-helpgive-feedback)

#### Build hledger

1. get [`stack`](/download.html#b) and (except on Windows, where stack provides it) [`git`](http://git-scm.com), then:
2. `git clone http://github.com/simonmichael/hledger hledger && cd hledger && stack install`

<div style="margin-left:1em; margin-right:1em; padding:.5em; border:thin solid #ddd; border-radius:.5em;">
In more detail:

**1. Get tools**

[`stack`](/download.html#b)
is the recommended tool for building hledger.  You can use cabal-install
if you prefer, but that requires more expertise;
the hledger docs assume stack.

[`git`](http://git-scm.com) is the version control tool needed to
fetch the hledger source and submit changes. On Windows, stack will
install this as well.

<!--
While you're installing, here are some optional extra tools:

- `ghcid`: gives real-time feedback as you make code changes, reliable and useful.
- `hasktags`: generates tag files for quick code navigation in editors like Emacs and vi.
- `shelltestrunner`: if you want to run hledger's functional tests.
- [GNU Make](http://www.gnu.org/software/make): if you want to use some convenient [Makefile rules](#make).

```shell
$ stack install ghcid hasktags shelltestrunner
```
-->

**2. Get the source**

```shell
$ git clone http://github.com/simonmichael/hledger hledger   # or git:
```

**3. Build/install**

```shell
$ cd hledger
$ stack install
```

This builds all the hledger packages, and installs executables in
`$HOME/.local/bin/` (or the Windows equivalent), which you should 
[add to your `$PATH`](/download.html#b).

This can take a while!
To save time, you can build fewer [packages](/manual.html#official-add-ons), eg just the CLI:
```shell
$ stack install hledger
```

You can also build and run in place, without installing executables:
```shell
$ stack build; stack exec -- hledger [ARGS]
```

Note stack fetches most required dependencies automatically,
but not C libraries such as curses or terminfo, which you might need
to install yourself, using your system's package manager.
In case of trouble, see [download](/download.html#b).

</div>

#### Use GHCI

These all work from the main hledger source directory (at least).

First, ensure all required dependencies are installed with these
commands. (You might also need to install some system libs like
terminfo or curses.)

```shell
$ stack test
$ stack bench
```

Get a GHCI prompt for hledger-lib:
```shell
$ cd hledger-lib; stack ghci hledger-lib
```
Changing into the package directory isn't actually needed, but it
enables a custom .ghci script which sets a more useful short prompt.

Get a GHCI prompt for hledger:
```shell
$ cd hledger; stack ghci hledger
```

Get a GHCI prompt for hledger-ui:
```shell
$ cd hledger-ui; stack ghci hledger-ui
```
Get a GHCI prompt for hledger-web:
```shell
$ cd hledger-web; stack ghci hledger-web
```
hledger-web also needs to find some things in its current directory (like the static/ directory).
This normally just works, if not please [send details](https://github.com/simonmichael/hledger/issues/274).

<!--
Get a GHCI prompt for hledger and hledger-lib:
```shell
$ make ghci
```

Get a GHCI prompt for hledger-web, hledger and hledger-lib:
```shell
$ make ghci-web
```

For the dev.hs developer script:
```shell
$ make ghci-dev
```
-->

#### Add a test

- identify what to test
- choose the test type: unit ? functional ? benchmark ?
- currently expected to pass or fail ?
- figure out where it goes
- write test, verify expected result
- get it committed

#### Fix a bug or add a feature

- research, discuss, validate the issue/feature on irc/list/bug tracker
- look for related tests, run the tests and check they are passing
- add a test ?
- develop a patch
- include any related issue numbers in the patch name, eg: "fix for blah blah (#NNN)"
- get it committed

#### Get your changes accepted

Follow the usual github workflow:

- fork the main hledger repo on github,
- git clone it to your local machine,
- git commit, after (?) pulling and merging the latest upstream changes
- git push back to github,
- open a pull request on github,
- follow up on any discussion there.

If you're new to this process, [help.github.com](http://help.github.com) may be useful.

#### Add yourself to the contributor list

- after getting something into the master branch, read and sign the [contributor list & agreement](contributors.html). Or, [ask](/docs.html#get-helpgive-feedback) to be added.
- give yourself a high five!

### Technical Writer

- get familiar with the website and documentation online, review and test
- get familiar with the site/doc source files (see [Shake.hs](#shake))
- get the latest hledger source
- send patches with names prefixed with "doc: " (or "site: ")

### Graphics Designer

- more/better logos & graphics
- illustrations and diagrams
- web design mockups for home page, site, hledger-web UI

<!-- ### Product Designer -->
### Communicator

Marketing and market understanding is vital.

- clarify project goals, value proposition, brand, mission, story
- monitor product-market fit
- identify new opportunities
- influence developer priorities
- spread the word!

### Maintainer

#### Help with issue management

- watch tracker activity, report status
- apply/update labels where needed
- follow up on dormant issues
- facilitate a consistently good bug-reporting & PR-contributing experience

#### Help with packaging

- package hledger for linux distros, macports, etc.
- develop mac/windows installers
- find and assist distro packagers/installer developers

#### Help with project management

- clarify/update goals and principles
- monitor, report on project progress and performance
- research, compare and report on successful projects, related projects
- identify collaboration opportunities
- marketing, communication, outreach
- release management, roadmap planning

#### Do a major release

- review the release how-to in the developer guide
    - and update as needed
      (make site-preview, http://localhost:8000/contributing.html#do-a-major-release)

- clean working copy
    - commit/stash/clear any pending changes in working copy
    - merge any commits from other branches & working copies
    - check out master, or release branch.
      Major releases are done in master if possible.
      If not, do as much of the below as is feasible in master,
      then start a release branch (git checkout -b X.Y)

- ensure tests pass
    - make unittest
    - make functest
    - make haddocktest
    - make cabalfiletest

- update dependencies
    - check & fix any outdated upper bounds
      (dev guide -> quick links -> hackage)

- update cabal files
    - */hledger*.cabal
        - descriptions
        - tested-with
        - file lists
            - data-files
            - extra-tmp-files
            - extra-source-files
            - exposed-modules
            - other-modules

- update stack.yaml file
    - resolver
    - extra-deps
    - flags

- update docs
    - haddocks
    - changelogs
    - man pages
    - site/release-notes.md
    - site/manual.md (commands, options, --help, ledger compatibility..)
    - site/contributing.md
    - site/step-by-step.md
    - site/how-to-*
    - site/faq.md (ledger compatibility)
    - site/download.md
    - site/contributors.md
    - doc/ANNOUNCE

- update version
    - edit .version
    - make setversion
    - double-check: cabal files, man pages ?, manual, download, release-notes, devguide..
    - commit

- make tarballs/binaries
    - ensure no packages are commented out in Makefile's PACKAGES
    - make cabalsdist
    - [make windows binaries]
    - [make mac binaries]

- release tests
    - make haddocktest
    - make cabalfiletest
    - cabal tarballs install into a clean directory without warnings
    - cabal upload --dry reports no problems

- tag
    - make tagrelease

- publish
    - stack upload hledger-lib; stack upload hledger; stack upload hledger-ui; stack upload hledger-web
    - [wait a day for it to appear in stackage nightly ?]
    - ensure hackage is showing the latest haddocks
    - check the hackage build matrix
    - git push --tags
    - deploy at demo.hledger.org
    - [upload binaries to hledger.org]
    - ensure the website is showing latest docs
      (download links, release notes, manual, how-tos, dev guide links, etc.)

- announce
    - review/close open issues in tracker
    - email doc/ANNOUNCE to hledger, haskell-cafe, haskell, [ledger] lists
    - tweet
    - [blog]
    - [reddit]
    - update release notes with announcement link & short description

- post-release
    - handle problem reports, support requests


#### Do a minor release

Differences from a major release:
work in a release branch,
set PACKAGES only to the affected package(s),
don't run make setversion.

1. cleanup
    - review working copies (laptop, server, website) & branches, commit pending changes
2. document
    - \*/\*.cabal for affected package(s) (descriptions, tested-with, files..)
    - \*/CHANGES for affected package(s)
    - site/release-notes.md
    - site/manual.md (commands, options, --help, ledger compatibility..)
    - site/step-by-step.md
    - site/how-to-*
3. test
    - make unittest
    - make functest
    - make haddocktest
4. branch
    - switch to release branch (git checkout X.Y)
5. version
    - edit .version (don't make setversion)
    - manually bump version for affected package(s): cabal files, manual..
6. package
    - set Makefile's PACKAGES to affected package(s)
    - make cabalsdist
7. test
    - install from tarball(s) into a clean directory
8. tag
    - make tagrelease
9. push
    - git push --tags
10. upload
    - make cabalupload
11. announce
    - [email hledger]
    - [tweet]



## More about...
### Project

#### Mission

Why was hledger created ?

Mainly:

- to provide a more usable, robust, documented, cross-platform-installable version of Ledger for users
- to provide a more maintainable and hackable version of Ledger for developers 

Also:

- to provide a useful library and toolbox for finance-minded haskell programmers
- to explore the suitability of Haskell for such applications
- to experiment with building a successful time-and-money-solvent project in a thriving ecosystem of financial software projects

What is the hledger project's current mission ?

1. Provide peace of mind: bring clarity, relief, and peace of mind to folks stressed, confused, overwhelmed by finances.
2. Educate and empower: help individuals and communities achieve clarity, accountability and mastery with money and time.

#### Roles and activities

- newcomer/potential user
- user
- library user
- field tester
- bug wrangler
- support
- documentor
- qa
- developer
- packager
- communicator
- project manager

### Issue tracking

The hledger project's issue tracker is on github. It contains:

- BUG issues - failures in some part of the hledger project (the main hledger packages, docs, website..)
- WISH issues - feature proposals, enhancement requests
- uncategorised issues - we don't know what these are yet
- pull requests - proposed changes to code and docs

Use these shortcut urls for quick access:

- <http://bugs.hledger.org>     - show open BUG issues
- <http://wishes.hledger.org>   - show open WISH issues
- <http://issues.hledger.org>   - show all issues, open or closed
- <http://prs.hledger.org>      - show open pull requests
<!-- - <http://bugs.hledger.org/N>   - show issue number N -->
<!-- - <http://bugs.hledger.org/new> - create a new issue -->

Labels are used to categorise:

- the issue's type: "A BUG" or "A WISH", in shades of red (The A makes it appear as first label)
- relevant subsystems/topics, in light blue
- relevant platforms, in light purple
- resolution if not fixed: "closed:cant-reproduce/duplicate/invalid/wont-fix", in dark grey
- "bounty", in bright yellow: issues with bountysource funding
- "easy?", in dim yellow: issues which are probably relatively easy to fix
- "imported" etc., in white: miscellaneous information

Clicking blue topic labels is a good way to review issues in a topic you're interested in.

In 2017, milestones are not used much. Projects are being used to organise the roadmap.

You might see some experiments in estimate tracking, where
some issue names might have a suffix noting estimated and spent time.
Basic format: [ESTIMATEDTOTALTASKTIME|TIMESPENTSOFAR]. Examples:
```
[2]       two hours estimated, no time spent
[..]      half an hour estimated (a dot is ~a quarter hour, as in timedot format)
[1d]      one day estimated (a day is ~4 hours)
[1w]      one week estimated (a week is ~5 days or ~20 hours)
[3|2]     three hours estimated, about two hours spent so far  
[1|1w|2d] first estimate one hour, second estimate one week, about two days spent so far 
```
Estimates are always for the total time cost (not time remaining).
Estimates are not usually changed, a new estimate is added instead.
Numbers are very approximate, but better than nothing.

The [trello board](http://trello.hledger.org) (trello.hledger.org) is a categorised collection of wishlist items,
this should probably be considered deprecated.

### Make

A Makefile is provided to make common developer tasks easy to remember,
and to insulate us a little from the ever-evolving Haskell tools ecosystem.
Using it is entirely optional, but recommended.
You'll need [GNU Make](http://www.gnu.org/software/make) installed.

The Makefile is self-documenting. Run `make` to see a list of the main make rules:

```shell
$ make
Makefile:37: -------------------- hledger make rules --------------------
Makefile:39: make [help] -- list documented rules in this makefile. make -n RULE shows more detail.
Makefile:204: (INSTALLING:)
Makefile:206: make install -- download dependencies and install hledger executables to ~/.local/bin or equivalent (with stack)
Makefile:231: (BUILDING:)
Makefile:235: make build -- download dependencies and build hledger executables (with stack)
Makefile:304: make hledgerdev -- quickly build the hledger executable (with ghc and -DDEVELOPMENT)
...
```

To see what a make rule will do without actually doing it, use the `-n` flag:

```shell
$ make build -n
stack build
```
```shell
$ make test -n
(stack test \
		&& echo pkgtest PASSED) || echo pkgtest FAILED
(stack exec hledger test \
		&& echo builtintest PASSED) || echo builtintest FAILED
(COLUMNS=80 PATH=`pwd`/bin:/home/simon/src/hledger/bin:/home/simon/.local/bin:/home/simon/.cabal/bin:/opt/ghc/7.10.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/var/lib/gems/1.9.1/bin stack exec -- shelltest --execdir -- -j16 --hide-successes tests \
		&& echo functest PASSED) || echo functest FAILED
```

### Shake

`Shake.hs` in the top directory complements the Makefile; it is used for some more complex 
scripts relating to documentation.

### Tests

Run the package tests of all (or selected) packages.
Does not include hledger's functional tests.
```shell
$ stack test [PKG]
```

Run some hledger unit tests via a built-in hledger command:
```shell
$ [stack exec] hledger test
```

Run hledger's functional tests:
```shell
$ stack install shelltestrunner
$ make functest
```

Run both unit and functional tests:
```shell
$ make test
```

Test generation of haddock docs:
```shell
$ make haddocktest
```


### Benchmarking

Benchmarks are standard performance measurements,
which we define using `bench` declarations in cabal files.
There is [one in hledger.cabal](https://github.com/simonmichael/hledger/blob/master/hledger/hledger.cabal#L228),
with related code and data files in [hledger/bench/](https://github.com/simonmichael/hledger/tree/master/hledger/bench).

To run the standard hledger benchmark, use `stack bench hledger`.
This installs haskell dependencies (but not system dependencies) and rebuilds as needed,
then runs [hledger/bench/bench.hs](https://github.com/simonmichael/hledger/blob/master/hledger/bench/bench.hs),
which by default shows quick elapsed-time measurements for several operations on a standard data file:

```shell
$ stack bench hledger
NOTE: the bench command is functionally equivalent to 'build --bench'
...
hledger-0.27: benchmarks
Running 1 benchmarks...
Benchmark bench: RUNNING...
Benchmarking hledger in /Users/simon/src/hledger/hledger with timeit
read bench/10000x1000x10.journal        [1.57s]
print                                   [1.29s]
register                                [1.92s]
balance                                 [0.21s]
stats                                   [0.23s]
Total: 5.22s
Benchmark bench: FINISH
```

bench.hs has some other modes, which you can use by compiling and running it directly.
`--criterion` reports more detailed and dependable measurements, but takes longer:

```shell
$ cd hledger; stack exec -- ghc -ibench bench/bench && bench/bench --criterion
...
Linking bench/bench ...
Benchmarking hledger in /Users/simon/src/hledger/hledger with criterion
benchmarking read bench/10000x1000x10.journal
time                 1.414 s    (1.234 s .. 1.674 s)
                     0.996 R²   (0.989 R² .. 1.000 R²)
mean                 1.461 s    (1.422 s .. 1.497 s)
std dev              59.69 ms   (0.0 s .. 62.16 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking print
time                 1.323 s    (1.279 s .. 1.385 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.305 s    (1.285 s .. 1.316 s)
std dev              17.20 ms   (0.0 s .. 19.14 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking register
time                 1.995 s    (1.883 s .. 2.146 s)
                     0.999 R²   (0.998 R² .. NaN R²)
mean                 1.978 s    (1.951 s .. 1.995 s)
std dev              25.09 ms   (0.0 s .. 28.26 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking balance
time                 251.3 ms   (237.6 ms .. 272.4 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 260.4 ms   (254.3 ms .. 266.5 ms)
std dev              7.609 ms   (3.192 ms .. 9.638 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking stats
time                 325.5 ms   (299.1 ms .. 347.2 ms)
                     0.997 R²   (0.985 R² .. 1.000 R²)
mean                 329.2 ms   (321.5 ms .. 339.6 ms)
std dev              11.08 ms   (2.646 ms .. 14.82 ms)
variance introduced by outliers: 16% (moderately inflated)
```

`--simplebench` shows a table of elapsed-time measurements for the commands defined in [bench/default.bench](https://github.com/simonmichael/hledger/blob/master/hledger/bench/default.bench).
It can also show the results for multiple h/ledger executables side by side, if you tweak the bench.hs code.
Unlike the other modes, it does not link with the hledger code directly, but runs the "hledger" executable found in $PATH (so ensure that's the one you intend to test).

```shell
$ cd hledger; stack exec -- ghc -ibench bench/bench && bench/bench --simplebench
Benchmarking /Users/simon/.local/bin/hledger in /Users/simon/src/hledger/hledger with simplebench and shell
Using bench/default.bench
Running 4 tests 1 times with 1 executables at 2015-08-23 16:58:59.128112 UTC:
1: hledger -f bench/10000x1000x10.journal print	[3.27s]
1: hledger -f bench/10000x1000x10.journal register	[3.65s]
1: hledger -f bench/10000x1000x10.journal balance	[2.06s]
1: hledger -f bench/10000x1000x10.journal stats	[2.13s]

Summary (best iteration):

+-----------------------------------------++---------+
|                                         || hledger |
+=========================================++=========+
| -f bench/10000x1000x10.journal print    ||    3.27 |
| -f bench/10000x1000x10.journal register ||    3.65 |
| -f bench/10000x1000x10.journal balance  ||    2.06 |
| -f bench/10000x1000x10.journal stats    ||    2.13 |
+-----------------------------------------++---------+
```

bench's --simplebench mode is based on a standalone tool, [tools/simplebench.hs](https://github.com/simonmichael/hledger/blob/master/tools/simplebench.hs).
simplebench.hs is a generic benchmarker of one or more executables (specified on the command line) against one or more sets of command-line arguments (specified in a file).
It has a better command-line interface than bench.hs, so you may find it more convenient
for comparing multiple hledger versions, or hledger and ledger. Eg:

```shell
$ stack exec -- ghc tools/simplebench
[1 of 1] Compiling Main             ( tools/simplebench.hs, tools/simplebench.o )
Linking tools/simplebench ...
```
```shell
$ tools/simplebench -h
tools/simplebench -h
simplebench: at least one executable needed
bench [-f testsfile] [-n iterations] [-p precision] executable1 [executable2 ...]

Run some functional tests with each of the specified executables,
where a test is "zero or more arguments supported by all executables",
and report the best execution times.

  -f testsfile   --testsfile=testsfile    file containing tests, one per line, default: bench.tests
  -n iterations  --iterations=iterations  number of test iterations to run, default: 2
  -p precision   --precision=precision    show times with this precision, default: 2
  -v             --verbose                show intermediate results
  -h             --help                   show this help

Tips:
- executables may have arguments if enclosed in quotes
- tests can be commented out with #
- results are saved in benchresults.{html,txt}
```
```shell
cd hledger; $ ../tools/simplebench -f bench/default.bench hledger ledger
Using bench/default.bench
Running 4 tests 2 times with 2 executables at 2015-08-24 04:24:37.257068 UTC:

Summary (best iteration):

+-----------------------------------------++---------+--------+
|                                         || hledger | ledger |
+=========================================++=========+========+
| -f bench/10000x1000x10.journal print    ||    3.24 |   0.43 |
| -f bench/10000x1000x10.journal register ||    3.80 |   3.48 |
| -f bench/10000x1000x10.journal balance  ||    2.05 |   0.18 |
| -f bench/10000x1000x10.journal stats    ||    2.10 |   0.19 |
+-----------------------------------------++---------+--------+
```

Finally, for quick, fine-grained performance measurements when troubleshooting or optimising, I use
[dev.hs](https://github.com/simonmichael/hledger/blob/master/dev.hs).

#### Sample journal files

Synthetic data files like `examples/100x100x10.journal` are useful for benchmarks and testing.
The numbers describe the number of transactions, number of accounts, and maximum account depth respectively.
They are generated by [`tools/generatejournal.hs`](https://github.com/simonmichael/hledger/blob/master/tools/generatejournal.hs).
They should get built automatically as needed, if not you can use `make samplejournals`:

```shell
$ make samplejournals
ghc tools/generatejournal.hs
[1 of 1] Compiling Main             ( tools/generatejournal.hs, tools/generatejournal.o )
Linking tools/generatejournal ...
tools/generatejournal 100 100 10 >examples/100x100x10.journal
tools/generatejournal 1000 1000 10 >examples/1000x1000x10.journal
tools/generatejournal 1000 10000 10 >examples/1000x10000x10.journal
tools/generatejournal 10000 1000 10 >examples/10000x1000x10.journal
tools/generatejournal 10000 10000 10 >examples/10000x10000x10.journal
tools/generatejournal 100000 1000 10 >examples/100000x1000x10.journal
tools/generatejournal 3 5 5 >examples/ascii.journal
tools/generatejournal 3 5 5 --chinese >examples/chinese.journal
tools/generatejournal 3 5 5 --mixed >examples/mixed.journal
```

### Documentation

Project documentation lives in a number of places:

- `site/*.md` is the hledger.org website content, which is generated with hakyll[-std] and pandoc
- haddock documentation in the code appears on Hackage
- short blurbs: cabal files, module headers, HCAR, GSOC project, ..
- `doc/notes.org` has some old developer notes
- developer reports (profiles, benchmarks, coverage..) in doc/profs, sometimes published at hledger.org/profs

How to prepare changelogs & release notes

Draft:

- `make changelog-draft >> doc/CHANGES.draft.org` (or `>` if this is the first draft)
- open this org file and sort the nodes (`C-c ^ a`)
- categorisation pass 1: go through and add topic prefixes where missing
- sort the nodes again
- categorisation pass 2: move significant items to the appropriate package subnode as appropriate; keep "soft" items that might appear in release notes; delete the rest
- cleanup pass: combine/rewrite items for clarity

Changelogs:

- choose release version and date
- add new sections at top of {doc,hledger*}/CHANGES
- move the items from CHANGES.draft.org to these CHANGES files
- remove CHANGES.draft.org

Release notes:

- add a new TOC entry and section in site/release-notes.md
- copy/rewrite/summarise package changelogs 
- note any other items of interest
- list release contributors
- write release summary

### Code

hledger is a suite of applications, tools and libraries.
The main hledger code repository is [github.com/simonmichael/hledger](http://github.com/simonmichael/hledger)
(aka `code.hledger.org`).
There are also various hledger addons maintained as separate projects with their own repos.

Within the main repo, there are a number of separate cabal packages,
making it easier to pick and choose parts of hledger to install or to package.
They are:

#### hledger-lib

[package](http://hackage.haskell.org/package/hledger-lib),
[exported modules](http://hackage.haskell.org/package/hledger-lib#modules),
[code](https://github.com/simonmichael/hledger/tree/master/hledger-lib)

Core data models, parsing, standard reports, and utilities.
Most data types are defined in [Hledger.Data.Types](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html),
while functions that operate on them are defined in Hledger.Data.TYPENAME.
Under [Hledger.Read](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read.hs)
are parsers for the supported input formats.
Data files are parsed into a
[Journal](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Journal),
which contains a list of
[Transactions](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Transaction),
each containing multiple
[Postings](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Posting)
of some
[MixedAmount](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:MixedAmount)
(multiple
single-[CommoditySymbol](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:CommoditySymbol)
[Amounts](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Amount))
to some
[AccountName](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:AccountName).
When needed, the Journal is further processed to derive a
[Ledger](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Ledger),
which contains summed 
[Accounts](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html#t:Account).
In [Hledger.Reports](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Reports.html)
there are standard reports, which extract useful data from the Journal or Ledger.

Here's a diagram of the main data model:

<a href="images/data-model.png" class="highslide" onclick="return hs.expand(this)">
<img src="images/data-model.png" alt="diagram" title="main data types" style="max-width:100%;">
</a>

<!-- generated by plantuml from:
<uml>
hide empty members
hide circle
skinparam packageStyle Rect

Ledger *-- Journal
Ledger *-- "*" Account
note top of Ledger: A Journal and all its accounts with their balances.\nUsed for balance report
note top of Journal: A journal file and parsed transactions & directives.\nUsed for print & register reports
note bottom of Account: An account's name, balance (inclusive &\nexclusive), parent and child accounts
Account o-- "*" Account :subaccounts, parent
Journal o-- File
File o-- "*" File :include
Journal *-- "*" MarketPrice
Journal *-- "*" Transaction
MarketPrice -- Date
MarketPrice -- Amount
Transaction -- Date
Transaction *-- "*" Posting
Transaction o-- "*" Tag
Posting o- "*" Tag
Posting -- "0..1" Date
Account -- AccountName
Posting -- AccountName
Account -- "2" MixedAmount
Posting -- MixedAmount
MixedAmount *-- "*" Amount
Amount -- CommoditySymbol
Amount -- Quantity
Amount -- Price
Amount -- AmountStyle
</uml>
-->

#### hledger

[package](http://hackage.haskell.org/package/hledger),
[exported modules](http://hackage.haskell.org/package/hledger#modules),
[code](https://github.com/simonmichael/hledger/tree/master/hledger),
[manual](/manual.html#hledger)

hledger's command line interface, and command line options and utilities for other hledger tools.

Try tracing the execution of a hledger command:

1. [Hledger.Cli.Main:main](https://github.com/simonmichael/hledger/blob/master/hledger/Hledger/Cli/Main.hs#L302)
parses the command line to select a command, then
2. gives it to
[Hledger.Cli.Utils:withJournalDo](https://github.com/simonmichael/hledger/blob/master/hledger/Hledger/Cli/Utils.hs#L73),
which runs it after doing all the initial parsing.
3. Parsing code is under
[hledger-lib:Hledger.Read](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read.hs),
eg [Hledger.Read.JournalReader](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read/JournalReader.hs).
4. Commands extract useful information from the parsed data model using
[hledger-lib:Hledger.Reports](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Reports),
and
5. render in plain text for console output (or another output format, like CSV).
6. Everything uses the data types and utilities from
[hledger-lib:Hledger.Data](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Data)
and [hledger-lib:Hledger.Utils](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Utils.hs).

#### hledger-ui

[package](http://hackage.haskell.org/package/hledger-ui),
<!-- [exported modules](http://hackage.haskell.org/package/hledger-ui#modules), -->
[code](https://github.com/simonmichael/hledger/tree/master/hledger-ui),
[manual](/manual.html#hledger-ui)

A curses-style text interface.

#### hledger-web

[package](http://hackage.haskell.org/package/hledger-web),
[exported modules](http://hackage.haskell.org/package/hledger-web#modules),
[code](https://github.com/simonmichael/hledger/tree/master/hledger-web),
[manual](/manual.html#hledger-web)

A web interface.
hledger-web starts a web server built with the yesod framework,
and (by default) opens a web browser view on it.
It reads the journal file(s) at startup and again whenever they change.
It can also write (append) new transactions to the journal file.

There are two main views, which can be filtered with
[queries](/manual.html#queries):

- [/journal](http://demo.hledger.org/journal), showing general journal entries (like `hledger print`)

- [/register](http://demo.hledger.org/register?q=inacct:Expenses:Food),
  showing transactions affecting an account (slightly different from
  hledger's [register](/manual.html#register) command, which shows postings).

There is also:

- a sidebar (toggled by pressing `s`) showing the chart of accounts (like `hledger balance`)
- an [add form](http://demo.hledger.org/journal?add=1) for adding new transactions (press `a`)
- a help dialog showing quick help and keybindings (press `h` or click ?)

Most of the action is in

- [config/routes](https://github.com/simonmichael/hledger/tree/master/hledger-web/config/routes)
- [templates/default-layout-wrapper.hamlet](https://github.com/simonmichael/hledger/tree/master/hledger-web/templates/default-layout-wrapper.hamlet)
- [Foundation](https://github.com/simonmichael/hledger/tree/master/hledger-web/Foundation.hs)
- [Handler.*](https://github.com/simonmichael/hledger/tree/master/hledger-web/Handler)
- [static/hledger.js](https://github.com/simonmichael/hledger/tree/master/hledger-web/static/hledger.js)
- [static/hledger.css](https://github.com/simonmichael/hledger/tree/master/hledger-web/static/hledger.css)

Handler module and function names end with R, like the yesod-generated route type they deal with.

Dynamically generated page content is mostly inline hamlet.
Lucius/Julius files and widgets generally are not used, except for the default layout.

Here are some ways to run it during development:

- `yesod devel`: runs in developer mode, rebuilds automatically when config, template, static or haskell files change
(but only files in the hledger-web package):
```shell
$ (cd hledger-web; yesod devel)
```

- [yesod-fast-devel](https://hackage.haskell.org/package/yesod-fast-devel)
  may be a good alternative, also reloads the browser page

- `stack ghci`: runs the server in developer mode from GHCI.
Changes to static files like hledger.js will be visible on page reload;
to see other changes, restart it as shown.
```shell
$ (cd hledger-web; stack ghci hledger-web)
hledger-web> :main --serve   # restart: ctrl-c, :r, enter, ctrl-p, ctrl-p, enter
```

- `make ghci-web`: runs the server in developer mode from GHCI, also
interprets the hledger-lib and hledger packages so that :reload picks
up changes in those packages too:
```shell
$ make ghci-web
ghci> :main --serve
```
(This rule also creates symbolic links to hledger-web's `config`, `messages`, `static` and `templates`
directories, needed in developer mode, so it can run from the top directory. This may not work on Windows.)

#### hledger-api

[package](http://hackage.haskell.org/package/hledger-api),
<!-- [exported modules](http://hackage.haskell.org/package/hledger-api#modules), -->
[code](https://github.com/simonmichael/hledger/tree/master/hledger-api),
[manual](/manual.html#hledger-api)

A web API server. Uses the servant framework.

### Quality

Relevant tools include:

- unit tests (HUnit, make unittest)
- functional tests (shelltestrunner, make functest)
- performance tests (simplebench, make bench)
- documentation tests (make haddocktest + manual)
- ui tests (manual)
- installation tests (manual)
- code reviews

### Code review

- Code review party 2014/7/21-25:
  [discussion](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1070)<!-- missing ,
  [log](http://hledger.org/static/irc-20140725-code-review.html) -->
- Dev sprint/party 2015/10/10:
  [discussion](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1254)<!-- ircbrowse down ,
  [pre-chat](http://ircbrowse.net/day/hledger/2015/10/10),
  [log](http://ircbrowse.net/day/hledger/2015/10/11) -->

## Quick links

<style>
tr {
    border-top:thin solid #bbb;
}
</style>
<!-- | hledger.org&nbsp;&nbsp; | [combined release notes](release notes), [pre-compiled binaries](download) | -->
<!-- [web ui demo](http://demo.hledger.org/register?q=inacct%3Aassets%3Abank%3Achecking+sym%3A\%24) -->
<!-- [![travis build status](https://img.shields.io/travis/simonmichael/hledger.svg)](https://travis-ci.org/simonmichael/hledger) -->
<!-- [![hackage deps](https://img.shields.io/hackage-deps/v/hledger.svg?label=hackage+deps)](http://packdeps.haskellers.com/feed?needle=hledger) -->
<!-- [![github issues](https://img.shields.io/github/issues/simonmichael/hledger.svg)](http://bugs.hledger.org) -->
<!-- [![bountysource](https://api.bountysource.com/badge/team?team_id=75979&style=bounties_received)](https://github.com/simonmichael/hledger/issues?q=label:bounty) -->
|
|-------------------------|----------------------------------------------------------------------------|
| IRC                     | Join [#hledger](http://irc.hledger.org) ([chat log](http://ircbrowse.net/browse/hledger); see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1)) |
| Mail list               | [list.hledger.org](http://list.hledger.org) ([Gmane](http://dir.gmane.org/gmane.comp.finance.ledger.hledger)) |
| Twitter                 | [#hledger](https://twitter.com/search?q=%23hledger&src=typd&f=realtime), see also [#ledgercli](https://twitter.com/search?q=%23ledgercli&src=typd&f=realtime), [#plaintextaccounting](https://twitter.com/search?q=%23plaintextaccounting&src=typd&f=realtime), <a href="https://twitter.com/ledgertips">@LedgerTips</a> |
| hledger-web demo&nbsp;&nbsp;        | [demo.hledger.org](http://demo.hledger.org) |
| hledger-api demo        | [demo.hledger.org/api](http://demo.hledger.org/api/swagger.json), [in swagger editor](http://editor.swagger.io/#/?import=demo.hledger.org/api/swagger.json&no-proxy)
| Trello                  | [old backlog/wishlist planning board](http://trello.hledger.org) |
| Github                  | [simonmichael/hledger](http://github.com/simonmichael/hledger) (alias: code.hledger.org), [forks](http://forked.yannick.io/simonmichael/hledger) <br> [commits](http://github.com/simonmichael/hledger/commits), <!-- [unreleased commits](https://github.com/simonmichael/hledger/compare/0.23...master), [release branch commits](https://github.com/simonmichael/hledger/compare/master...0.23), --> [COMMITS!](http://starlogs.net/#simonmichael/hledger) <br> [open bugs](http://bugs.hledger.org), [open wishes](http://wishes.hledger.org), [open unknowns](https://github.com/simonmichael/hledger/issues?utf8=✓&q=is%3Aissue%20is%3Aopen%20-label%3A%22A%20BUG%22%20-label%3A%22A%20WISH%22%20), [open pull requests](http://prs.hledger.org), [all issues](https://github.com/simonmichael/hledger/issues?q=) <br> [issues with bounty tag](https://github.com/simonmichael/hledger/issues?q=label:bounty), [bountysource bounties](https://github.com/simonmichael/hledger/issues?q=%22Add%20to%20the%20bounty%20at%20Bountysource%22%20OR%20%22claim%20the%20bounty%20on%20Bountysource%22%20OR%20%22bounty%20on%20this%20issue%20has%20been%20claimed%20at%20Bountysource%22%20), [codemill bounties](https://github.com/simonmichael/hledger/issues?q=codemill), [codefund bounties](https://github.com/simonmichael/hledger/issues?utf8=✓&q=codefund) <br> stars:  <a class="github-button" href="https://github.com/simonmichael/hledger" data-icon="octicon-star" data-count-href="/simonmichael/hledger/stargazers" data-count-api="/repos/simonmichael/hledger#stargazers_count" data-count-aria-label="# stargazers on GitHub" aria-label="Star simonmichael/hledger on GitHub"></a> (#99 of ~30k [starred haskell projects](https://github.com/search?o=desc&q=language%3AHaskell+stars%3A%3E440&ref=searchresults&s=stars&type=Repositories) in 2016/04, #71 in 2016/12, #65 in 2017/3, #64 in 2017/5, #75 in 2017/8) <br> [![Throughput Graph](https://graphs.waffle.io/simonmichael/hledger/throughput.svg){width=520 height=170}](https://waffle.io/simonmichael/hledger/metrics) |
| Travis CI               | [![travis ubuntu build status](https://img.shields.io/travis/simonmichael/hledger.svg){height=20}](https://travis-ci.org/simonmichael/hledger)
| Appveyor CI             | <a name=appveyor></a>[![appveyor windows build status](https://ci.appveyor.com/api/projects/status/5vejw0w5n5igdr42?svg=true){height=20}](https://ci.appveyor.com/project/simonmichael/hledger) [latest developer builds](https://ci.appveyor.com/project/simonmichael/hledger/build/artifacts)
| Hackage                 | packages: [hledger-lib](http://hackage.haskell.org/package/hledger-lib), [hledger](http://hackage.haskell.org/package/hledger), [hledger-ui](http://hackage.haskell.org/package/hledger-ui), [hledger-web](http://hackage.haskell.org/package/hledger-web), [hledger-api](http://hackage.haskell.org/package/hledger-api), [hledger-diff](http://hackage.haskell.org/package/hledger-diff), [hledger-iadd](http://hackage.haskell.org/package/hledger-iadd), [hledger-interest](http://hackage.haskell.org/package/hledger-interest), [hledger-irr](http://hackage.haskell.org/package/hledger-irr), [\*hledger\*](http://hackage.haskell.org/packages/search?terms=hledger) <!-- [![](https://img.shields.io/hackage/v/hledger.svg?label=current+release)](http://hackage.haskell.org/package/hledger) --> <!-- 2017/4 not updating  <br> GHC compatibility: [hledger-lib](http://matrix.hackage.haskell.org/package/hledger-lib), [hledger](http://matrix.hackage.haskell.org/package/hledger), [hledger-ui](http://matrix.hackage.haskell.org/package/hledger-ui), [hledger-web](http://matrix.hackage.haskell.org/package/hledger-web), [hledger-api](http://matrix.hackage.haskell.org/package/hledger-api) --> <br> reverse deps: [hledger-lib](http://packdeps.haskellers.com/reverse/hledger-lib), [hledger](http://packdeps.haskellers.com/reverse/hledger), [hledger-ui](http://packdeps.haskellers.com/reverse/hledger-ui), [hledger-web](http://packdeps.haskellers.com/reverse/hledger-web), [hledger-api](http://packdeps.haskellers.com/reverse/hledger-api) <br> [![](https://img.shields.io/hackage-deps/v/hledger-lib.svg?label=hledger-lib+bounds){height=20}](http://packdeps.haskellers.com/feed?needle=hledger-lib) [![](https://img.shields.io/hackage-deps/v/hledger.svg?label=hledger+bounds){height=20}](http://packdeps.haskellers.com/feed?needle=hledger) [![](https://img.shields.io/hackage-deps/v/hledger-ui.svg?label=hledger-ui+bounds){height=20}](http://packdeps.haskellers.com/feed?needle=hledger-ui) [![](https://img.shields.io/hackage-deps/v/hledger-web.svg?label=hledger-web+bounds){height=20}](http://packdeps.haskellers.com/feed?needle=hledger-web) [![](https://img.shields.io/hackage-deps/v/hledger-api.svg?label=hledger-api+bounds){height=20}](http://packdeps.haskellers.com/feed?needle=hledger-api) |
| Stackage                | [build-constraints.yaml](https://github.com/fpco/stackage/blob/master/build-constraints.yaml), [open hledger-related issues](https://github.com/fpco/stackage/search?q=hledger+is%3Aopen&type=Issues) |
| Debian                  | source packages: [haskell-hledger-lib](http://tracker.debian.org/pkg/haskell-hledger-lib), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-lib), [haskell-hledger](http://tracker.debian.org/pkg/haskell-hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger), [haskell-hledger-ui](http://tracker.debian.org/pkg/haskell-hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-ui), [haskell-hledger-web](http://tracker.debian.org/pkg/haskell-hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-web) <br> binary packages: <br>&nbsp;stable [hledger](https://packages.debian.org/stable/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=stable), <!-- [hledger-ui](https://packages.debian.org/stable/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=stable), --> [hledger-web](https://packages.debian.org/stable/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=stable) <br>&nbsp;testing [hledger](https://packages.debian.org/testing/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=testing), <!-- [hledger-ui](https://packages.debian.org/testing/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=testing), --> [hledger-web](https://packages.debian.org/testing/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=testing) <br>&nbsp;unstable [hledger](https://packages.debian.org/unstable/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=unstable), [hledger-ui](https://packages.debian.org/unstable/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=unstable), [hledger-web](https://packages.debian.org/unstable/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=unstable) <br>&nbsp;experimental [hledger](https://packages.debian.org/experimental/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=experimental), [hledger-ui](https://packages.debian.org/experimental/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=experimental), [hledger-web](https://packages.debian.org/experimental/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=experimental) <br>&nbsp;all [\*hledger\*](https://packages.debian.org/search?searchon=names&keywords=hledger) <br> popularity stats: [hledger](https://qa.debian.org/popcon.php?package=haskell-hledger), [hledger-ui](https://qa.debian.org/popcon.php?package=haskell-hledger-ui), [hledger-web](https://qa.debian.org/popcon.php?package=haskell-hledger-web) <br> [PTS help](https://www.debian.org/doc/manuals/developers-reference/resources.html#pkg-tracking-system) |
| Ubuntu                  | source packages: [haskell-hledger-lib](https://launchpad.net/ubuntu/+source/haskell-hledger-lib), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-lib), [haskell-hledger](https://launchpad.net/ubuntu/+source/haskell-hledger), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger), [haskell-hledger-ui](https://launchpad.net/ubuntu/+source/haskell-hledger-ui), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-ui), [haskell-hledger-web](https://launchpad.net/ubuntu/+source/haskell-hledger-web), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-web) <br> binary packages: [\*hledger\*](http://packages.ubuntu.com/search?suite=all&searchon=names&keywords=hledger) |
| Gentoo                  | [hledger](http://gpo.zugaina.org/dev-haskell/hledger), [hledger-web](http://gpo.zugaina.org/dev-haskell/hledger-web), [\*hledger\*](http://gpo.zugaina.org/Search?search=hledger) |
| Fedora                  | [hledger](https://apps.fedoraproject.org/packages/hledger), [\*hledger\*](https://apps.fedoraproject.org/packages/s/hledger), [hledger (package db)](https://admin.fedoraproject.org/pkgdb/package/hledger/), [Haskell SIG](http://fedoraproject.org/wiki/Haskell_SIG) |
| Void Linux              | [hledger\*](https://github.com/voidlinux/void-packages/search?utf8=✓&q=hledger) |
| Nix                     | [\*hledger\*](http://hydra.nixos.org/search?query=hledger) |
| Homebrew                | [hledger](https://github.com/Homebrew/homebrew-core/blob/master/Formula/hledger.rb) |
| Sandstorm               | [hledger web app & reviews](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90), [issues](https://github.com/simonmichael/hledger/issues?utf8=✓&q=label%3A%22platform%3A%20sandstorm%22%20)
| Reference               | [GHC Can I Use](http://damianfral.github.io/ghcaniuse/) |

<!-- list the debian packages for clarity:
3 source:
haskell-hledger-lib
haskell-hledger
haskell-hledger-web
8 binary:
hledger
hledger-web
libghc-hledger-dev
libghc-hledger-doc
libghc-hledger-prof
libghc-hledger-lib-dev
libghc-hledger-lib-doc
libghc-hledger-lib-prof
-->

<!-- old/future links -->
<!-- [haddock coverage](http://hledger.org/profs/haddock-coverage), -->
<!-- [unit test coverage](http://hledger.org/profs/coverage/hpc_index_fun.html), -->
<!-- [benchmark](http://hledger.org/profs/latest.bench) -->
<!-- [profile](http://hledger.org/profs/latest.prof), -->
<!-- [heap](http://hledger.org/profs/latest.ps) -->
<!-- [developer notes](http://github.com/simonmichael/hledger/NOTES.org)\ -->
<!-- [browse dev API docs](http://hledger.org/api/frames.html) -->
<!-- [How to clone it](contributing#set-up-for-development) -->
<!-- [hledger-web dev demo](http://demo.hledger.org:5001) -->

<!-- hoogle search form
<script type="text/javascript" src="http://haskell.org/hoogle/datadir/resources/jquery-1.4.2.js"></script>
<script type="text/javascript" src="http://haskell.org/hoogle/datadir/resources/hoogle.js"></script>
<form action="http://haskell.org/hoogle/" method="get" style="display:inline; margin:0; padding:0;">
<input type="hidden" name="prefix" value="+hledger +hledger-lib +hledger-web +hledger-vty +hledger-chart" />
<span style="white-space:nowrap;"
><input type="text" name="hoogle" id="hoogle" accesskey="1" size="30"
/><input type="submit" value="search API with hoogle"
/></span>
</form>
-->


<!-- <script async defer src="https://buttons.github.io/buttons.js"></script> -->
