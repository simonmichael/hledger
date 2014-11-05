# Installation Guide

- [[#How to install]]
- [[#Troubleshooting]]

## How to install

hledger works on GNU/linux, mac and windows.
Here are several ways to install it:

### a. With your system package manager

If you have a system package manager that includes hledger,
this will be the quickest and easiest way to install,
if you don't need the very latest version.

^ On distro/packaging system: ^ Run: ^
| Debian & Ubuntu: | `apt-get install hledger [hledger-web]` |
| Red Hat, Fedora & CentOS (?): | `yum install hledger` |
| NixOS: | `nix-env -iA nixpkgs.haskellPackages.hledger` |

### b. Download binaries from hledger.org

Ready-to-run [[download|downloads]] for GNU/Linux, Mac OSX, and
Microsoft Windows are provided on a donation basis. These have not
been updated recently, but you can fix that by making a donation of
any size (see the page for more).

These are simple compressed executables (not installers), so after downloading
you may need to decompress, adjust permissions, and rename the file. Eg:

    $ gunzip hledger-web-0.18.2-mac-x86_64.gz
    $ chmod +x hledger-web-0.18.2-mac-x86_64
    $ mv hledger-web-0.18.2-mac-x86_64 /usr/local/bin/hledger-web
    $ /usr/local/bin/hledger-web --version

### c. Build released source from Hackage

You can download and build the latest release yourself using [cabal](http://www.haskell.org/cabal/users-guide/),
the standard installer for Haskell software.
This is the most common way to install hledger, but not always the easiest;
use the troubleshooting tips below if needed.

Ensure you have [GHC](http://haskell.org/ghc) or
the [Haskell Platform](http://haskell.org/platform) installed.
hledger requires GHC 7.2 or greater, and hledger-web requires GHC 7.4 or greater.

Also note that some Haskell packages depend on C packages, and cabal
currently isn't able to install or identify those for you. A common
issue is not having all the ncurses C libraries installed. A quick way
to ensure you have all required C libs is to
[install hledger once with your system package manager](#install-with-your-system-package-manager)
before installing the latest version with cabal.

Then install the hledger command-line tool:

    $ cabal update
    $ cabal install hledger [--dry-run]
    $ hledger --version

You should see the proper version reported.
If you get "could not resolve dependencies", "hledger not found",
or any other problem, see [troubleshooting](#troubleshooting).
Also note, to use non-ascii characters like £ in your data, you might need to [configure a suitable locale](MANUAL.html#locale).

#### Installing hledger-web

To also install the web interface, in theory just do:

    $ cabal install hledger-web

In practice, this is a real beast to keep working, so as of 2014/4/17 it's best to do it this way:

    $ cabal sandbox init             # start with a clean cabal package db, requires cabal 1.18
    $ cabal update
    $ cabal install -j alex happy    # update to alex 3.1 and happy 1.19
    $ cabal install -j hledger-web   # don't cabal install hledger first

This installs hledger and hledger-web. hledger-web will appear as the `web` command in hledger's commands list.

There are other [[start#add-on-commands|add-on packages]], hopefully compatible with the current hledger release and your platform.

### d. Build latest source from git

To download and build the latest development version of hledger, ensure you have
[git](http://git-scm.com) installed, then:

    $ git clone http://github.com/simonmichael/hledger.git
    $ cd hledger
    $ cabal update
    $ [optional: cabal sandbox init]
    $ cabal install ./hledger-lib ./hledger [./hledger-web]

The same [notes above](#install-from-hackage-with-cabal) about requirements and checking your installation apply. Note this time we mention `cabal sandbox`, a feature of cabal 1.18+ which can be used to reduce package dependency problems; it can be used when installing from Hackage as well.

## Troubleshooting

There are a lot of ways things can go wrong, especially if you are building from source.
Here are some known issues and things to try. Please also seek
[support](DEVELOPMENT.html#support) from the
[IRC channel](irc://irc.freenode.net/#ledger),
[mail list](http://hledger.org/list) or
[bug tracker](http://hledger.org/bugs).

Starting from the top, consider whether each of these might apply to
you. Tip: blindly reinstalling/upgrading everything in sight probably
won't work, it's better to go in small steps and understand the problem,
or get help.

### hledger not found ?
If cabal install succeeded but you get a message like "hledger not found" when you run hledger,
you should add cabal's bin directory to your PATH environment variable.
Eg on unix-like systems, something like:
```
$ echo 'export PATH=$PATH:~/cabal/bin' >> ~/.bash_profile
$ source ~/.bash_profile
```
On Ubuntu 14.04:
```
$ echo 'export PATH=~/.cabal/bin:$PATH' >> ~/.bashrc
$ source ~/.bashrc
```
Test your PATH-variable with:
```
$ $PATH
```
### hledger --version shows wrong version ?
Perhaps you have multiple versions of hledger in your PATH. Eg you installed with the system package manager 
(to get C libs) and then with cabal (to get the latest version), but cabal's bin directory appears too late
in the PATH. Move it closer to the front.

### Did you cabal update ?
If not, `cabal update` and try again.

### Do you have a new enough version of GHC ?
Run `ghc --version`. hledger requires GHC 7.0 or greater
(and on [some platforms](#5551), 7.2.1+ can be helpful).

### Do you have a new enough version of cabal ?
Avoid ancient versions, which are less capable and more confusing.
`cabal --version` should ideally report at least 1.16 (or if you want to
do sandboxed installs, 1.18). You may be able to upgrade it with:
```
$ cabal update
$ cabal install cabal-install
```

### haskeline fails to install, requires Cabal >=1.16

Related to the above. haskeline, one of hledger's dependencies, claims
to require cabal-install version 1.16+, which is a problem if, say,
you are on Debian Wheezy and only have cabal-install version 0.14.
You can relax haskeline's version constraint like so:

```
cabal unpack haskeline
cd haskeline-X.Y
(edit haskeline.cabal, comment out the `Cabal-Version:  >=1.16` line)
cabal install
(resume installing hledger)
```

### Are your installed GHC/cabal packages in good repair ?
Run `ghc-pkg check`. If it reports problems, some of your packages have
become inconsistent, and you should fix these first.
[ghc-pkg-clean](https://gist.github.com/1185421) is an easy way.

### cabal says "rejecting: system-fileio-0.3.11, 0.3.10 (conflict: blah blah blah.."
system-fileio does not yet allow text 1.x, making cabal sweat.
If your cabal is modern enough, adding `--max-backjumps=10000` should help.
([more](https://groups.google.com/d/topic/hledger/FdWGTSAVzYU/discussion)).

### cabal can't satisfy the new dependencies due to old installed packages
Cabal dependency failures become more likely as you install more
packages over time. If `cabal install hledger-web --dry` says it can't
satisfy dependencies, you have this problem. You can:

- (a) try to understand which packages to remove (with `ghc-pkg unregister`)
   or which constraints to add (with `--constraint 'PKG == ...'`) to help cabal
   find a solution

- (b) install into a fresh cabal sandbox, created with `cabal sandbox init`.
   ([virthualenv](http://hackage.haskell.org/package/virthualenv) or
   [cabal-dev](http://hackage.haskell.org/package/cabal-dev) also work).

- or (c<!-- -->) (easiest) erase your installed packages with
   [ghc-pkg-reset](https://gist.github.com/1185421) and try again.

For more detail, see [How to cabal install](https://www.fpcomplete.com/user/simonmichael/how-to-cabal-install).

### Dependency or compilation error in one of the new packages ?
 If cabal starts downloading and building packages and then terminates
 with an error, look at the output carefully and identify the problem
 package(s).  If necessary, add `-v2` or `-v3` for more verbose
 output. You can install the new packages one at a time to troubleshoot,
 but remember cabal is smarter when installing all packages at once.

 Often the problem is that you need to install a particular C library
 using your platform's package management system. Or the dependencies
 specified on a package may need updating. Or there may be a compilation
 error.  If you find an error in a hledger package, check the
 [recent commits](http://github.com/simonmichael/hledger/commits) to
 see if the [latest development version](#installing) might have a fix.

### ExitFailure 11
See
[http://hackage.haskell.org/trac/hackage/ticket/777](http://hackage.haskell.org/trac/hackage/ticket/777).
This means that a build process has been killed, usually because it grew
too large.  This is common on memory-limited VPS's and with GHC 7.4.1.
Look for some memory-hogging processes you can kill, increase your RAM,
or limit GHC's heap size by doing `cabal install ... --ghc-options='+RTS
-M400m'` (400 megabytes works well on my 1G VPS, adjust up or down..)

### Can't load .so/.DLL for: ncursesw (/usr/lib/libncursesw.so: file too short)
(or similar): cf [GHC bug #5551](http://hackage.haskell.org/trac/ghc/ticket/5551).
Upgrade GHC to 7.2.1, or try your luck with [this workaround](http://eclipsefp.github.com/faq.html).

### Undefined iconv symbols on OS X
This kind of error:

    Linking dist/build/hledger/hledger ...
    Undefined symbols:
      "_iconv_close", referenced from:
          _hs_iconv_close in libHSbase-4.2.0.2.a(iconv.o)
      "_iconv", referenced from:
          _hs_iconv in libHSbase-4.2.0.2.a(iconv.o)
      "_iconv_open", referenced from:
          _hs_iconv_open in libHSbase-4.2.0.2.a(iconv.o)

probably means you are on a mac with macports libraries installed, cf
[http://hackage.haskell.org/trac/ghc/ticket/4068](http://hackage.haskell.org/trac/ghc/ticket/4068).
To work around temporarily, add this --extra-lib-dirs flag:

    $ cabal install hledger --extra-lib-dirs=/usr/lib

or permanently, add this to ~/.cabal/config:

    extra-lib-dirs: /usr/lib

### "invalid preprocessing directive" on OS X

> "I'm trying to cabal install hledger-web on OS X, GHC 7.6, and getting error: invalid preprocessing directive #{mixedAmountAsHtml amt}".

[Example](https://gist.github.com/miikka/8886233)

Certain OS X and GHC versions do not work well together ([cabal #1496](https://github.com/haskell/cabal/issues/1496), [ghc #7678](https://ghc.haskell.org/trac/ghc/ticket/7678)).
There's a fix for this in hledger HEAD as of 2014/2/8 (it's not in 0.22.1).
If you find more cases, please report it.

### Many warnings about "missing terminating ' character" on OS X

Related to the above problem, can be ignored.

### hledger-vty requires curses-related libraries
On Ubuntu, eg, you'll need the `libncurses5-dev` package. On Windows,
these are not available (unless perhaps via Cygwin.)

### hledger-chart requires GTK-related libraries
On Ubuntu, eg, install the `libghc6-gtk-dev` package. See also [Gtk2Hs installation notes](http://code.haskell.org/gtk2hs/INSTALL).

### error based on missing ncurses C libs on Ubuntu 14.04 trusty
The following solved my dependency-problem with ncurses (this was required even when I had installed hledger 0.22 via apt-get)
     sudo apt-get install libghc-hscurses-dev libghc-ncurses-dev
