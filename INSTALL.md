---
title: hledger Installation Guide
---

# Installation Guide

hledger works on GNU/linux, mac and windows.
Here are several ways to install it.

## Install with your system package manager

If you have a system package manager that includes hledger,
this will be the quickest and easiest way to install,
if you don't need the very latest version.

Debian, Ubuntu:    `apt-get install hledger [hledger-web]`

Red Hat, Fedora:    `yum install hledger`


## Install binaries from hledger.org

[Ready-to-run binaries](DOWNLOAD.html) for GNU/Linux, Mac OSX, and Microsoft Windows can be downloaded from this site.
They are out of date, but you can fund new ones with a donation of any size (see the page for more).
This is probably the easiest way to install hledger on windows and mac.

The binaries do not currently include installers, so after downloading
you may need to decompress, make executable, and/or rename the file. Eg:

    $ gunzip hledger-web-0.18.2-mac-x86_64.gz
    $ chmod +x hledger-web-0.18.2-mac-x86_64
    $ mv hledger-web-0.18.2-mac-x86_64 /usr/local/bin/hledger-web
    $ /usr/local/bin/hledger-web --version

## Install from hackage with cabal

You can download and build the latest release yourself using cabal, the standard installer for Haskell software.
This is the most common way to install hledger, but not always the easiest;
use the troubleshooting tips below if needed.

Ensure you have [GHC](http://hackage.haskell.org/ghc/) or
the [Haskell Platform](http://hackage.haskell.org/platform/) installed.
hledger currently requires GHC 7.2 or greater, and hledger-web requires GHC 7.4 or greater.
Then install the hledger command-line tool:

    $ cabal update
    $ cabal install hledger [--dry-run]
    $ hledger --version

You should see the proper version reported.
If you get "could not resolve dependencies", "hledger not found",
or any other problem, see [troubleshooting](#troubleshooting).
Also note, to use non-ascii characters like £ in your data, you might need to [configure a suitable locale](MANUAL.html#locale).

To also install the web interface (slightly harder), do:

    $ cabal install hledger-web [--dry-run]
    $ hledger-web --version

This also installs hledger if not already installed, and the hledger-web command
will also be available as hledger's `web` subcommand.

Other add-on packages are available on Hackage, although some of these are
unmaintained or work only on certain platforms:

- [hledger-chart](http://hackage.haskell.org/package/hledger-chart)
- [hledger-interest](http://hackage.haskell.org/package/hledger-interest)
- [hledger-irr](http://hackage.haskell.org/package/hledger-irr)
- [hledger-vty](http://hackage.haskell.org/package/hledger-vty)

## Install the latest development version

To download and build the latest development version of hledger, ensure you have
[git](http://git-scm.com) installed, then:

    $ git clone http://github.com/simonmichael/hledger.git
    $ cd hledger
    $ cabal update
    $ cabal install ./hledger-lib ./hledger [./hledger-web]
    
## Troubleshooting

There are a lot of ways things can go wrong. Here are
some known issues and things to try. Please also seek
[support](DEVELOPMENT.html#support) from the
[IRC channel](irc://irc.freenode.net/#ledger),
[mail list](http://hledger.org/list) or
[bug tracker](http://hledger.org/bugs).

Starting from the top, consider whether each of these might apply to
you. Tip: blindly reinstalling/upgrading everything in sight probably
won't work, it's better to go in small steps and understand the problem,
or get help.

#. **hledger not found ?**  
  If cabal install succeeded but you get a message like "hledger not found" when you run hledger,
  you should add cabal's bin directory to your PATH environment variable.
  Eg on unix-like systems, something like:

      $ echo 'export PATH=$PATH:~/cabal/bin' >> ~/.bash_profile
      $ source ~/.bash_profile

#. **Did you cabal update ?**  
  If not, `cabal update` and try again.

#. **Do you have a new enough version of GHC ?**  
  Run `ghc --version`. hledger requires GHC 7.0 or greater
  (on [some platforms](#5551), 7.2.1 can be helpful).

#. **Do you have a new enough version of cabal ?**  
  Avoid ancient versions.  `cabal --version` should report at least
  0.10 (and 0.14 is much better). You may be able to upgrade it with:
  
        $ cabal update
        $ cabal install cabal-install-0.14

#. **Are your installed GHC/cabal packages in good repair ?**  
  Run `ghc-pkg check`. If it reports problems, some of your packages have
  become inconsistent, and you should fix these first. 
  [ghc-pkg-clean](https://gist.github.com/1185421) is an easy way.

#. <a name="cabaldeps" />**cabal can't satisfy the new dependencies due to old installed packages**  
  Cabal dependency failures become more likely as you install more
  packages over time. If `cabal install hledger-web --dry` says it can't
  satisfy dependencies, you have this problem. You can:
  
    a. try to understand which packages to remove (with `ghc-pkg unregister`)
       or which constraints to add (with `--constraint 'PKG == ...'`) to help cabal
       find a solution

    b. install into a fresh environment created with
       [virthualenv](http://hackage.haskell.org/package/virthualenv) or
       [cabal-dev](http://hackage.haskell.org/package/cabal-dev)

    c. or (easiest) erase your installed packages with
       [ghc-pkg-reset](https://gist.github.com/1185421) and try again.

#. **Dependency or compilation error in one of the new packages ?**  
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

#. **ExitFailure 11**  
  See
  [http://hackage.haskell.org/trac/hackage/ticket/777](http://hackage.haskell.org/trac/hackage/ticket/777).
  This means that a build process has been killed, usually because it grew
  too large.  This is common on memory-limited VPS's and with GHC 7.4.1.
  Look for some memory-hogging processes you can kill, increase your RAM,
  or limit GHC's heap size by doing `cabal install ... --ghc-options='+RTS
  -M400m'` (400 megabytes works well on my 1G VPS, adjust up or down..)

#. <a name="5551" />**Can't load .so/.DLL for: ncursesw (/usr/lib/libncursesw.so: file too short)**  
  (or similar): cf [GHC bug #5551](http://hackage.haskell.org/trac/ghc/ticket/5551).
  Upgrade GHC to 7.2.1, or try your luck with [this workaround](http://eclipsefp.github.com/faq.html).

#. <a name="iconv" />**Undefined iconv symbols on OS X**  
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

#. **hledger-vty requires curses-related libraries**  
  On Ubuntu, eg, you'll need the `libncurses5-dev` package. On Windows,
  these are not available (unless perhaps via Cygwin.)

#. **hledger-chart requires GTK-related libraries**  
  On Ubuntu, eg, install the `libghc6-gtk-dev` package. See also [Gtk2Hs installation notes](http://code.haskell.org/gtk2hs/INSTALL).

