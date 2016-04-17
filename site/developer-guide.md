<!-- hledger repo and http://hledger.org versions of this document are periodically bidirectionally synced -->

<style>
#toc > ol > li > a { display:none; }
#toc > ol > li > ol > li { padding-left:0; }
</style>
* toc

# Developer guide

Contributors of all levels are most welcome in the hledger project.
This guide is action-oriented: below you'll find useful links, then procedures, then general info.

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
| IRC                     | Join [#hledger](http://hledger.org/irc) ([chat log](http://ircbrowse.net/browse/hledger); see also [#ledger](http://webchat.freenode.net?channels=ledger&randomnick=1)) |
| Mail list               | [list.hledger.org](http://list.hledger.org) ([Gmane](http://dir.gmane.org/gmane.comp.finance.ledger.hledger)) |
| Twitter                 | [#hledger](https://twitter.com/search?q=%23hledger&src=typd&f=realtime), see also [#ledgercli](https://twitter.com/search?q=%23ledgercli&src=typd&f=realtime), [#plaintextaccounting](https://twitter.com/search?q=%23plaintextaccounting&src=typd&f=realtime), <a href="https://twitter.com/ledgertips">@LedgerTips</a> |
| hledger-web demo&nbsp;&nbsp;        | [demo.hledger.org](http://demo.hledger.org) |
| hledger-api demo        | [demo.hledger.org/api](http://demo.hledger.org/api/swagger.json), [in swagger editor](http://editor.swagger.io/#/?import=demo.hledger.org/api/swagger.json&no-proxy)
| Trello                  | [backlog/wishlist planning board](http://hledger.org/trello) |
| Github                  | [simonmichael/hledger](http://github.com/simonmichael/hledger) (code.hledger.org) <br> [commits](http://github.com/simonmichael/hledger/commits), <!-- [unreleased commits](https://github.com/simonmichael/hledger/compare/0.23...master), [release branch commits](https://github.com/simonmichael/hledger/compare/master...0.23), --> [COMMITS!](http://starlogs.net/#simonmichael/hledger), [travis build status](https://travis-ci.org/simonmichael/hledger) [![](https://img.shields.io/travis/simonmichael/hledger.svg?label=travis+build+status){width=164 height=20}](https://travis-ci.org/simonmichael/hledger) <br> [open bugs](https://github.com/simonmichael/hledger/issues?direction=desc&labels=BUG&page=1&sort=created&state=open), [open wishes (scheduled)](https://github.com/simonmichael/hledger/issues?utf8=%E2%9C%93&q=is%3Aopen+milestone%3A1.0+-label%3ABUG+sort%3Acreated-desc), [open  wishes (unscheduled)](https://github.com/simonmichael/hledger/issues?utf8=%E2%9C%93&q=is%3Aopen+sort%3Acreated-desc++no%3Amilestone+), <a href="https://github.com/simonmichael/hledger/issues?q=label:bounty">issues with bounties</a>, [all issues](https://github.com/simonmichael/hledger/issues?direction=desc&labels=&page=1&sort=created) <br> [most-starred haskell projects](https://github.com/search?utf8=✓&q=stars%3A%3E200+language%3AHaskell&type=Repositories&ref=searchresults) (hledger is #99 (of 47k haskell projects) as of 2016/04/05) |
| Hackage                 | packages: [hledger-lib](http://hackage.haskell.org/package/hledger-lib), [hledger](http://hackage.haskell.org/package/hledger), [hledger-ui](http://hackage.haskell.org/package/hledger-ui), [hledger-web](http://hackage.haskell.org/package/hledger-web), [hledger-diff](http://hackage.haskell.org/package/hledger-diff), [hledger-interest](http://hackage.haskell.org/package/hledger-interest), [hledger-irr](http://hackage.haskell.org/package/hledger-irr), [\*hledger\*](http://hackage.haskell.org/packages/search?terms=hledger) <!-- [![](https://img.shields.io/hackage/v/hledger.svg?label=current+release)](http://hackage.haskell.org/package/hledger) --> <br> GHC compatibility: [hledger-lib](http://matrix.hackage.haskell.org/package/hledger-lib), [hledger](http://matrix.hackage.haskell.org/package/hledger), [hledger-ui](http://matrix.hackage.haskell.org/package/hledger-ui), [hledger-web](http://matrix.hackage.haskell.org/package/hledger-web) <br> reverse deps: [hledger-lib](http://packdeps.haskellers.com/reverse/hledger-lib), [hledger](http://packdeps.haskellers.com/reverse/hledger), [hledger-ui](http://packdeps.haskellers.com/reverse/hledger-ui), [hledger-web](http://packdeps.haskellers.com/reverse/hledger-web) <br>  newer deps: [hledger-lib](http://packdeps.haskellers.com/feed/?needle=hledger-lib), [hledger](http://packdeps.haskellers.com/feed/?needle=hledger), [hledger-ui](http://packdeps.haskellers.com/feed/?needle=hledger-ui), [hledger-web](http://packdeps.haskellers.com/feed/?needle=hledger-web) [![](https://img.shields.io/hackage-deps/v/hledger-lib.svg?label=upper+bounds){width=158 height=20}](http://packdeps.haskellers.com/feed?needle=hledger-lib) [![](https://img.shields.io/hackage-deps/v/hledger.svg?label=upper+bounds){width=158 height=20}](http://packdeps.haskellers.com/feed?needle=hledger) [![](https://img.shields.io/hackage-deps/v/hledger-ui.svg?label=upper+bounds){width=158 height=20}](http://packdeps.haskellers.com/feed?needle=hledger-ui) [![](https://img.shields.io/hackage-deps/v/hledger-web.svg?label=upper+bounds){width=158 height=20}](http://packdeps.haskellers.com/feed?needle=hledger-web) |
| Stackage                | [hledger entry](https://github.com/fpco/stackage/blob/master/build-constraints.yaml#L626-629), [issues](https://github.com/fpco/stackage/search?q=hledger&ref=cmdform&type=Issues) <br> [jenkins build status](http://jenkins.stackage.org/job/Stackage/), last build output: [7.4](http://jenkins.stackage.org/job/Stackage/ghcversion=7.4.2/lastBuild/console), [7.6](http://jenkins.stackage.org/job/Stackage/ghcversion=7.6.3/lastBuild/console), [7.8](http://jenkins.stackage.org/job/Stackage/ghcversion=7.8.2/lastBuild/console), [7.10](http://jenkins.stackage.org/job/Stackage/ghcversion=7.10.1/lastBuild/console) |
| Debian                  | source packages: [haskell-hledger-lib](http://tracker.debian.org/pkg/haskell-hledger-lib), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-lib), [haskell-hledger](http://tracker.debian.org/pkg/haskell-hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger), [haskell-hledger-ui](http://tracker.debian.org/pkg/haskell-hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-ui), [haskell-hledger-web](http://tracker.debian.org/pkg/haskell-hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=haskell-hledger-web) <br> binary packages: <br>&nbsp;stable [hledger](https://packages.debian.org/stable/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=stable), <!-- [hledger-ui](https://packages.debian.org/stable/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=stable), --> [hledger-web](https://packages.debian.org/stable/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=stable) <br>&nbsp;testing [hledger](https://packages.debian.org/testing/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=testing), <!-- [hledger-ui](https://packages.debian.org/testing/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=testing), --> [hledger-web](https://packages.debian.org/testing/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=testing) <br>&nbsp;unstable [hledger](https://packages.debian.org/unstable/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=unstable), [hledger-ui](https://packages.debian.org/unstable/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=unstable), [hledger-web](https://packages.debian.org/unstable/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=unstable) <br>&nbsp;experimental [hledger](https://packages.debian.org/experimental/hledger), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger;dist=experimental), [hledger-ui](https://packages.debian.org/experimental/hledger-ui), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-ui;dist=experimental), [hledger-web](https://packages.debian.org/experimental/hledger-web), [bugs](https://bugs.debian.org/cgi-bin/pkgreport.cgi?package=hledger-web;dist=experimental) <br>&nbsp;all [\*hledger\*](https://packages.debian.org/search?searchon=names&keywords=hledger) <br> popularity stats: [hledger](https://qa.debian.org/popcon.php?package=haskell-hledger), [hledger-ui](https://qa.debian.org/popcon.php?package=haskell-hledger-ui), [hledger-web](https://qa.debian.org/popcon.php?package=haskell-hledger-web) <br> [PTS help](https://www.debian.org/doc/manuals/developers-reference/resources.html#pkg-tracking-system) |
| Ubuntu                  | source packages: [haskell-hledger-lib](https://launchpad.net/ubuntu/+source/haskell-hledger-lib), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-lib), [haskell-hledger](https://launchpad.net/ubuntu/+source/haskell-hledger), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger), [haskell-hledger-ui](https://launchpad.net/ubuntu/+source/haskell-hledger-ui), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-ui), [haskell-hledger-web](https://launchpad.net/ubuntu/+source/haskell-hledger-web), [bugs](https://bugs.launchpad.net/ubuntu/+source/haskell-hledger-web) <br> binary packages: [\*hledger\*](http://packages.ubuntu.com/search?suite=all&searchon=names&keywords=hledger) |
| Gentoo                  | [hledger](http://gpo.zugaina.org/dev-haskell/hledger), [hledger-web](http://gpo.zugaina.org/dev-haskell/hledger-web), [\*hledger\*](http://gpo.zugaina.org/Search?search=hledger) |
| Fedora                  | [hledger](https://apps.fedoraproject.org/packages/hledger), [\*hledger\*](https://apps.fedoraproject.org/packages/s/hledger), [hledger (package db)](https://admin.fedoraproject.org/pkgdb/package/hledger/), [Haskell SIG](http://fedoraproject.org/wiki/Haskell_SIG) |
| Nix                     | [hydra build status](http://hydra.nixos.org/search?query=hledger) |
| Homebrew                | [hledger](https://github.com/Homebrew/homebrew/blob/master/Library/Formula/hledger.rb) |
| Sandstorm               | [hledger-web](https://dev.thewordnerd.info/nolan/hledger), [app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90)
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
<!-- [How to clone it](developer-guide#set-up-for-development) -->
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


## How to..

### Do user testing

- review and critique our documentation and web presence
- test the procedures on [download](download) and on this page
- check that the hledger binaries run your platform, and `hledger test` reports no failures
- test the hledger tools' functionality, usability, browser compatibility, ui layout etc.
- discuss/report problems via irc/mail list/bug tracker

### Get help

- for quick help or if you're not sure about the problem,
  you can ask on the [#hledger](http://irc.hledger.org) (irc.hledger.org) IRC channel
  or the [mail list](http://list.hledger.org) (list.hledger.org).
  If #hledger does not respond quickly, you can leave the window open and check back later, or leave your email address.
- check the open and closed issues in the [bug tracker](http://bugs.hledger.org) (bugs.hledger.org). Sometimes the problem has been fixed in git but not yet released.

<!-- - test and share problem journal snippets at paste . hledger.org -->

### Suggest enhancements

Suggestions and feature requests are welcome, but we don't want them to pile up in the issue tracker obscuring higher-priority tasks.
So consider these alternatives:

1. The [#hledger](http://irc.hledger.org) (irc.hledger.org) IRC channel on freenode
   and the [mail list](http://list.hledger.org) (list.hledger.org) are excellent places for discussing and refining ideas.
   Both are archived and linkable, so the idea won't be lost. IRC is quick (if I'm not online, leave a comment anyway),
   while the mail list has the most readers.
2. The [trello board](http://trello.hledger.org) (trello.hledger.org) is a good place
   for storing and prioritising backlog and wishlist items of all kinds.
3. The [bug tracker](http:///bugs.hledger.org) (bugs.hledger.org) on github
   is mostly used for reporting problems with the existing product or docs.
   If you're not sure if it's a problem or not, it's fine to report it.
   When enhancement requests land in the bug tracker, they get the WISH label, and are excluded from the default view at [bugs.hledger.org](http://bugs.hledger.org).


### Report bugs

- get to know the bug tracker (on github) and its contents.
  Some convenient url shortcuts:\
  [`bugs.hledger.org`](http://bugs.hledger.org)   - show non-wishlist issues \
  `bugs.hledger.org/N`   - jump to issue #N
- research and update existing issues
- report a bug: [`bugs.hledger.org/new`](http://bugs.hledger.org/new)

### Install stack and git

[stack](https://github.com/commercialhaskell/stack/wiki/Downloads) is
the recommended tool for building hledger from source.
It builds haskell projects, installing required haskell libraries as needed.
It can also install GHC (the compiler) and (on windows) git, if needed.

You don't need to use stack, if you are already expert with the older
cabal tool, or even just GHC, but I won't attempt to document those
procedures; these docs assume you have downloaded and installed stack.

On Windows, you should choose the 64-bit stack download if you will be
processing >50,000 transactions at a time with hledger
([#275](https://github.com/simonmichael/hledger/issues/275)).

[git](http://git-scm.com) is the revision control tool you'll need to
fetch the latest hledger source and submit changes. On windows, stack
can install it for you. These docs assume you have installed git and
know a little about how to use it.

### Install other optional tools

Up-to-date `alex`, `happy`, and `haddock` tools are required, but `stack` should install those for you.

Here are some optional extra tools:

- `shelltestrunner` is useful for running functional tests.
- `hasktags` is an easy way to generate editor tag files for quick source code navigation.
- `profiteur` is for reporting stack profiles.
- `hpack` regenerates cabal files when package.yaml files have been updated.
- `hoogle` is for searching source code.

You can install them all with:

```shell
$ stack install shelltestrunner hasktags profiteur hpack hoogle
```

### Get the latest hledger source

```{.shell .bold}
$ git clone code.hledger.org hledger    # aka github.com/simonmichael/hledger.git
$ cd hledger
```

<!--
Old instructions:

1. Get [GHC](https://www.haskell.org/ghc/) and [cabal-install](http://hackage.haskell.org/package/cabal-install) installed.
   I recommend the [stackage.org install guide](http://www.stackage.org/install).
   You can see which GHC versions are officially supported in the `tested-with` field in
   [hledger.cabal](http://hackage.haskell.org/package/hledger/hledger.cabal),
   [hledger-ui.cabal](http://hackage.haskell.org/package/hledger-ui/hledger-ui.cabal),
   [hledger-web.cabal](http://hackage.haskell.org/package/hledger-web/hledger-web.cabal).
   Older versions may also work.
2. Get [git](http://git-scm.com) installed.
3. Get [GNU Make](http://www.gnu.org/software/make) installed (unless you don't care about the Makefile's conveniences).
   On some platforms the command will be eg `gmake` instead of `make`.
4. Get the hledger repo:

    ```shell
    $ git clone https://github.com/simonmichael/hledger.git
    ```

5. You might want to install or upgrade some of these haskell developer tools.
   If you're not sure, skip this step and return to it as needed.
   Be sure the cabal bin directory where these are installed (eg ~/.cabal/bin) is in your PATH.

    ```{.shell .bold}
    $ cabal update
    $ cabal install alex happy       # if you get alex/happy-related errors when building hledger
    $ cabal install haddock          # needed to build hledger API docs
    $ cabal install shelltestrunner  # needed to run hledger functional tests (may need latest git version)
    $ cabal install hoogle hlint     # maybe useful for searching API docs and checking code
    ```

    You'll also want a comfortable code editor, preferably with Haskell support.
    (I use emacs + [haskell-mode](https://github.com/haskell/haskell-mode),
    or occasionally [IntelliJ IDEA](https://www.jetbrains.com/idea/download) + one of the [plugins](https://www.google.com/search?hl=en&q=intellij+plugins+haskell)).

6. Install haskell libs required by hledger:

    ```{.shell .bold}
    $ cd hledger
    $ cabal sandbox init   # optional
    $ make installdeps     # or cabal install --only-dep ./hledger-lib ./hledger [./hledger-web]
    ```

    This will install the required dependencies from Hackage.
    If you're new to cabal, you can expect problems at this stage.
    The usual remedy is to ensure you start with a clean package db, eg by doing `cabal sandbox init`.
    You can simplify and speed up this step a lot by commenting out
    hledger-web in the `PACKAGES` list in the [Makefile](https://github.com/simonmichael/hledger/blob/master/Makefile#L41).

7. Build with cabal:

    ```shell
    $ make cabalbuild
    ```

    (Tip: `make cabalCMD` runs `cabal CMD` in each of the hledger packages).

8. Build with GHC:

    ```shell
    $ make bin/hledgerdev
    ```

    This builds hledger (and hledger-lib) with GHC directly, without using cabal,
    and as quickly as possible, without optimizations (the "dev" suffix is a reminder of this).
    I use and recommend this method for development, as it crosses package boundaries and ensures you are building the latest code.
    However it needs some files generated by cabal build, which is why we did that first.
-->

### Use the Makefile

A Makefile is provided to make common developer tasks easy to remember,
and to insulate us a little from the ever-evolving Haskell tools ecosystem.
Using it is entirely optional, but recommended.
You'll need [GNU Make](http://www.gnu.org/software/make) installed.

The Makefile is self-documenting. Run `make` to see a list of the main make rules:

```{.shell}
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

```{.shell}
$ make build -n
stack build
```
```{.shell}
$ make test -n
(stack test \
		&& echo pkgtest PASSED) || echo pkgtest FAILED
(stack exec hledger test \
		&& echo builtintest PASSED) || echo builtintest FAILED
(COLUMNS=80 PATH=`pwd`/bin:/home/simon/src/hledger/bin:/home/simon/src/hledger/extra:/home/simon/.local/bin:/home/simon/.cabal/bin:/opt/ghc/7.10.1/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/var/lib/gems/1.9.1/bin stack exec -- shelltest --execdir -- -j16 --hide-successes tests \
		&& echo functest PASSED) || echo functest FAILED
```

### Build or install hledger

Run `make` to see a list of build rules. You probably want `build` or `install`.

`make build` runs stack build, which downloads required haskell
dependencies and builds all hledger packages.
The resulting executables will be somewhere under .stack-work, eg in
`.stack-work/install/i386-linux/lts-3.0/7.10.2/bin/`.

```shell
$ make build
stack build
hledger-lib-0.27: configure
hledger-lib-0.27: build
hledger-lib-0.27: install
hledger-0.27: configure
hledger-0.27: build
Progress: 1/4
...
```

Note stack will install required haskell libraries, but not C
libraries such as curses or terminfo. If you get a build error, it is
likely because one of these is missing, in which case you must
identify and install it yourself using your system's package
manager. This is usually a bit harder on Windows.

`make install` runs stack install, which does everything stack build does and also
copies the executables to `~/.local/bin` or the Windows equivalent.
You should make sure this directory is in your `$PATH`, so that you can just type
`hledger` to run the latest.
```shell
$ make install
stack install
NOTE: the install command is functionally equivalent to 'build --copy-bins'
hledger-0.27: build
...
Copied executables to /Users/simon/.local/bin/:
- hledger-web
- hledger-ui
- hledger
```

You can save time and effort by building just the package(s) you're interested in.
To install just the hledger command-line tool, but not hledger-ui or (especially costly)
hledger-web, do:
```shell
$ stack install hledger
```

(This looks like the [download page](download) command for installing the latest hledger release from Stackage.
The difference is, here we are running it inside the hledger source tree, so the source version will be installed.)

### Run benchmarks

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

### Generate sample journal files

Synthetic data files like `data/100x100x10.journal` are useful for benchmarks and testing.
The numbers describe the number of transactions, number of accounts, and maximum account depth respectively.
They are generated by [`tools/generatejournal.hs`](https://github.com/simonmichael/hledger/blob/master/tools/generatejournal.hs).
They should be built as needed, if not you can use `make samplejournals` rule:

```shell
$ make samplejournals
ghc tools/generatejournal.hs
[1 of 1] Compiling Main             ( tools/generatejournal.hs, tools/generatejournal.o )
Linking tools/generatejournal ...
tools/generatejournal 100 100 10 >data/100x100x10.journal
tools/generatejournal 1000 1000 10 >data/1000x1000x10.journal
tools/generatejournal 1000 10000 10 >data/1000x10000x10.journal
tools/generatejournal 10000 1000 10 >data/10000x1000x10.journal
tools/generatejournal 10000 10000 10 >data/10000x10000x10.journal
tools/generatejournal 100000 1000 10 >data/100000x1000x10.journal
tools/generatejournal 3 5 5 >data/ascii.journal
tools/generatejournal 3 5 5 --chinese >data/chinese.journal
tools/generatejournal 3 5 5 --mixed >data/mixed.journal
```

### Run developer tests

This command will install haskell dependencies (you might need to
install additional system dependencies yourself) and run the package
test suites.  Currently these consist of hledger-lib's unit tests,
hledger's unit tests, and hledger-web's functional tests:

```shell
$ stack test [PKG]
```

Run the hledger-lib and hledger unit tests as a built-in hledger command:
```shell
$ [stack exec] hledger test
```

Run the hledger functional tests:
```{.shell .bold}
$ stack install shelltestrunner  # if not already done
$ make functest
```

Run both unit and functional tests:
```{.shell .bold}
$ make test
```

Test haddock doc generation:
```shell
$ make haddocktest
```

### Add a test

- identify what to test
- choose the test type: unit ? functional ? benchmark ?
- currently expected to pass or fail ?
- figure out where it goes
- write test, verify expected result
- get it committed

### Use the REPL (GHCI)

These all work from the main hledger source directory (at least).

First, ensure all required dependencies are installed with these
commands. (You might also need to install some system libs like
terminfo or curses.)

```{.shell .bold}
$ stack test
$ stack bench
```

Get a GHCI prompt for hledger-lib:
```{.shell .bold}
$ stack ghci hledger-lib
```

Get a GHCI prompt for hledger:
```{.shell .bold}
$ stack ghci hledger
```

Get a GHCI prompt for hledger-web:
```{.shell .bold}
$ stack ghci hledger-web
```
hledger-web also needs to find some things in its current directory (like the static/ directory).
This normally just works, if not please [send details](https://github.com/simonmichael/hledger/issues/274).

Get a GHCI prompt for hledger and hledger-lib:
```{.shell .bold}
$ make ghci
```

Get a GHCI prompt for hledger-web, hledger and hledger-lib:
```{.shell .bold}
$ make ghci-web
```

<!--
For the dev.hs developer script:
```{.shell .bold}
$ make ghci-dev
```
-->

### Improve the documentation

- get familiar with the website and documentation online, review and test
- get familiar with the site/doc source files (see Makefile)
- get the latest hledger source
- send patches with names prefixed with "doc: " (or "site: ")

### Fix a bug or add a feature

- research, discuss, validate the issue/feature on irc/list/bug tracker
- look for related tests, run the tests and check they are passing
- add a test ?
- develop a patch
- include any related issue numbers in the patch name, eg: "fix for blah blah (#NNN)"
- get it committed

### Get your changes accepted

Follow the usual github workflow:

- fork the main hledger repo on github,
- git clone it to your local machine,
- git commit, after (?) pulling and merging the latest upstream changes
- git push back to github,
- open a pull request on github,
- follow up on any discussion there.

If you're new to this process, [help.github.com](http://help.github.com) may be useful.

### Become a contributor

- after getting one or more patches committed, read and sign the [contributor list & agreement](contributors.html)
- or, [ask](#how-to-get-help) to be added

### Do code review

- review and discuss new pull requests and commits on github
- set up for development and test the latest changes in your own repo
- read the existing [code docs and source](#quick-links)
- send feedback or discuss via irc or list

### Help with packaging

- package hledger for linux distros, macports, etc.
- develop mac/windows installers
- find and assist distro packagers/installer developers

### Help with project management

- clarify/update goals and principles
- monitor, report on project progress and performance
- research, compare and report on successful projects, related projects
- identify collaboration opportunities
- marketing, communication, outreach
- release management, roadmap planning

### Do a major release

- review the release how-to in the developer guide
    - and update as needed
      (make site-preview, http://localhost:8000/developer-guide.html#do-a-major-release)

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
    - site/developer-guide.md
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


### Do a minor release

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


## Project overview

### Mission, principles, goals

The hledger project aims to produce:

- a practical, accessible, dependable tool for end users
- a useful library and toolbox for finance-minded haskell programmers
- a successful, time-and-money-solvent project within a thriving ecosystem of financial software projects.

### Roles and activities

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

### Documentation

Project documentation lives in a number of places:

- `site/*.md` is the hledger.org website content, which is generated with hakyll[-std] and pandoc
- haddock documentation in the code appears on Hackage
- short blurbs: cabal files, module headers, HCAR, GSOC project, ..
- `doc/notes.org` has some old developer notes
- developer reports (profiles, benchmarks, coverage..) in doc/profs, sometimes published at hledger.org/profs

### Code

The hledger repo is hosted on Github, at <http://github.com/simonmichael/hledger>.
You can also jump there via `code.hledger.org[/commits]`.

### Quality control

Relevant tools include:

- unit tests (HUnit, make unittest)
- functional tests (shelltestrunner, make functest)
- performance tests (simplebench, make bench)
- documentation tests (make haddocktest + manual)
- ui tests (manual)
- installation tests (manual)
- code reviews

### Code reviews

We have held one code review party, in July 2014, on the mail list and IRC channel.
Here's the original [proposal](http://article.gmane.org/gmane.comp.finance.ledger.hledger/1070) giving some motivation, and the discussion logs, note these are a good source of hledger development tips:

- 2014/7/21-25 **hledger-web code & UI**
  [mail thread](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1070),
  [IRC log](http://hledger.org/static/irc-20140725-code-review.html)

Dev sprint/party #2 was on 2015/10/10.

- [announcement, report](http://thread.gmane.org/gmane.comp.finance.ledger.hledger/1254)
- [chat](http://ircbrowse.net/day/hledger/2015/10/10)
  [log](http://ircbrowse.net/day/hledger/2015/10/11)



<!-- ### release process -->

<!-- ### roadmap -->

<!-- ### communication and collaboration -->

<!-- ### web presence and hosting setup -->

<!-- ### finances and other resources -->

<!-- ### licensing and legal issues -->

<!-- ### contributors and credits -->



## Implementation notes

### hledger

There are two core cabal packages:

**[hledger-lib](http://hackage.haskell.org/package/hledger-lib)** - data model, parsing, manipulation, standard reports
([github](https://github.com/simonmichael/hledger/tree/master/hledger-lib))\
**[hledger](http://hackage.haskell.org/package/hledger)** - command line interface, reusable cli options & helpers
([github](https://github.com/simonmichael/hledger/tree/master/hledger))

Most data types are defined in [hledger-lib:Hledger.Data.Types](http://hackage.haskell.org/package/hledger-lib/docs/Hledger-Data-Types.html),
while functions that operate on them are defined in
hledger-lib:Hledger.Data.TYPENAME.
Here's a diagram of the main data model:
<img src="images/data-model.png">
<!--
generated by the old wiki from:
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
Amount -- Commodity
Amount -- Quantity
Amount -- Price
Amount -- AmountStyle
</uml>
-->

hledger parses the journal file into a
[Journal](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Journal),
which contains a list of
[Transactions](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Transaction),
each containing multiple
[Postings](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Posting)
of some
[MixedAmount](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:MixedAmount)
(multiple
single-[Commodity](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Commodity)
[Amounts](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Amount))
to some
[AccountName](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:AccountName).
Commands get and render
[Reports](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Reports.html)
from the Journal, or sometimes from a
[Ledger](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Ledger),
which contains
[Accounts](http://hackage.haskell.org/package/hledger-lib-0.23.2/docs/Hledger-Data-Types.html#t:Account)
representing the summed balances and other details of each account.

After surveying the packages, modules, and data types, try tracing the execution of a hledger command:

1. CLI stuff is in [hledger:Hledger.Cli](https://github.com/simonmichael/hledger/tree/master/hledger/Hledger/Cli).
2. [hledger:Hledger.Cli.Main:main](https://github.com/simonmichael/hledger/blob/master/hledger/Hledger/Cli/Main.hs#L179)
parses the command line to select a command, then
3. gives it to
[hledger:Hledger.Cli.Utils:withJournalDo](https://github.com/simonmichael/hledger/blob/master/hledger/Hledger/Cli/Utils.hs#L61),
which runs it after doing all the initial parsing.
4. Parsing code is under
[hledger-lib:Hledger.Read](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read.hs),
eg the
[hledger-lib:Hledger.Read.JournalReader](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Read/JournalReader.hs).
5. Commands extract useful information from the parsed data model using
[hledger-lib:Hledger.Reports](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Reports),
and
6. render it to the console.
7. Everything uses the types and data
utilities under
[hledger-lib:Hledger.Data](https://github.com/simonmichael/hledger/tree/master/hledger-lib/Hledger/Data),
and the general helpers from
[hledger-lib:Hledger.Utils](https://github.com/simonmichael/hledger/blob/master/hledger-lib/Hledger/Utils.hs)
and below.

### hledger-web

hledger-web is in a third cabal package:

**[hledger-web](http://hackage.haskell.org/package/hledger-web)** - web interface
([github](https://github.com/simonmichael/hledger/tree/master/hledger-web))

It is a single-executable web application using the
[yesod](http://yesodweb.com) framework.  It runs a built-in web server
serving some views of the journal file, reading it at startup and
again whenever it changes. It can also append new transactions to the journal file.
There are two main views, which can be filtered with [query arguments](manual#query-arguments):

- [/journal](http://demo.hledger.org/journal), showing general journal entries (like `hledger print`)

- [/register](http://demo.hledger.org/register?q=inacct:Assets:Bank:Checking),
  showing transactions affecting an account (slightly different from
  `hledger register`, which shows postings).

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

Handler module and function names end with R, like the Yesod-generated route type they deal with.

Dynamically generated page content is mostly inline hamlet.
Lucius/Julius files and widgets generally are not used, except for the default layout.

The quickest way to test changes is `make ghciweb`, `:main --serve`, control-C, `:r`, repeat.
No linking is required, and changes to static files like hledger.js are visible after reloading a page.

Another way is `yesod devel`, which rebuilds automatically when files
change, including config files, templates and static files (but only in the hledger-web package).

A third way is `make autoweb`, if you can get it working (see the
makefile for instructions). This rebuilds automatically when haskell
files change in any of the hledger{-lib,,-web} packages.
