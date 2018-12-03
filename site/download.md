# Download

There are several ways to install hledger:

a. [Download the binary or system package for your platform](#a.-download-a-binary-or-system-package) (quick install, often not up to date)
b. [Build the latest release with hledger-install](#b.-build-the-latest-release)
  or [with stack](#b2.-with-stack)
  or [with cabal](#b3.-with-cabal) (slow install, up to date)
c. [Build the development version with stack or cabal](#c.-build-the-development-version) (slow install, super-fresh)


<a name="a"></a>

## a. Download a binary or system package for your platform

<style>
table { margin-left:1em; }
tr { border-top:thin solid #ddd; border-bottom:thin solid #ddd; }
div > p > strong > code { margin-left:1em; } /* top-level code lines */
code { white-space:nowrap; }
tr { vertical-align:top; }
td { padding-bottom:.5em; padding-right:1em; }
td:first-of-type { 
  /* white-space:nowrap; */
  /* width:1%; */
}
a { white-space:nowrap; }
.warnings {
    display:inline-block;
    margin-left:1em;
    font-style:italic;
    font-size:small;
}
.warnings > a:before {
    content: " ⚠ ";
    color:red;
}
</style>

Binaries or system packages are quickest to install, but they can be outdated or incomplete.

**Available binaries / system packages:**
<div style="float:right; font-size:small;">
(please [update this page](https://github.com/simonmichael/hledger/edit/master/site/download.md) /
<br>report issues to packagers)
</div>

| Platform             | Command/Link           | Installs&nbsp;[version](https://repology.org/metapackage/hledger/badges)<br>(latest is [1.12](http://hledger.org/release-notes))
|----------------------|------------------------|----------------------------------------------------------------------------------------
| [Mac][]              | **`brew install hledger`** <br><span class=warnings>[only hledger CLI](https://github.com/simonmichael/hledger/issues/321#issuecomment-179920520)</span> | 1.11
| [Windows][]          | Developer binaries: **[1.11](https://ci.appveyor.com/api/buildjobs/rppu7oo05u283p0w/artifacts/hledger.zip)** <!-- or [latest nightly dev build](https://ci.appveyor.com/api/projects/simonmichael/hledger/artifacts/hledger.zip?branch=master) --> <br><span class=warnings> [no hledger-ui](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444),[doesn't work on old windows ?](https://github.com/simonmichael/hledger/issues/774),[many files in PATH causing hangs](https://github.com/simonmichael/hledger/issues/791)<!-- ,[appveyor builds failing](https://github.com/simonmichael/hledger/issues/832) --> </span> | 1.11
| &nbsp;               |
| [Arch&nbsp;Linux][]  | **`pacman -S hledger`** | 1.11
| CentOS               | <span class=warnings>?</span> | 
| [Debian][]           | **`sudo apt install hledger hledger-ui hledger-web`** | 1.0.1&nbsp;(Stable), 1.5&nbsp;(Testing), 1.10&nbsp;(Unstable)
| [Fedora][],&nbsp;RHEL | **`sudo dnf install hledger`** <br>or (more complete & current):<br>**`sudo dnf copr enable kefah/HLedger && sudo dnf install hledger`** | 1.2&nbsp;(27), 1.4&nbsp;(28), 1.5&nbsp;(Rawhide)<br>or 1.9.1
| [Gentoo][]           | **`sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`** | 1.11
| [Ubuntu][]           | **`sudo apt install hledger hledger-ui hledger-web`** | 0.26&nbsp;(16.04&nbsp;Xenial), 1.2&nbsp;(18.04&nbsp;Bionic), 1.5&nbsp;(18.10&nbsp;Cosmic)
| [Void&nbsp;Linux][]  | **`xbps-install -S hledger hledger-ui hledger-web hledger-api`** | 1.10
| &nbsp;               |
| FreeBSD              | <span class=warnings>?</span> | 
| NetBSD               | <span class=warnings>?</span> | 
| OpenBSD              | Ports: **[https://github.com/jasperla/openbsd-wip/pull/104](https://github.com/jasperla/openbsd-wip/pull/104)** <br>Third-party binaries: **[OpenBSD6.3/amd64](https://s3.amazonaws.com/openbsd-hledger/index.html)** | 1.10
| &nbsp;               |
| [Nix][]              | **<span style="font-size:small;">`nix-env -iA nixpkgs.haskellPackages.hledger nixpkgs.haskellPackages.hledger-ui nixpkgs.haskellPackages.hledger-web`</span>** <br><span class=warnings>[problems with hledger-ui on Mac ?](https://github.com/simonmichael/hledger/issues/613)</span> | 1.5&nbsp;(stable), 1.11&nbsp;(unstable)
| Sandstorm            | **[hledger-web Sandstorm app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90)** <br><span class=warnings>[features needed](https://github.com/simonmichael/hledger/issues/425)</span> | 1.9.2

[Mac]: https://formulae.brew.sh/formula/hledger
[Mac contact]: mailto:

[Windows]: https://ci.appveyor.com/project/simonmichael/hledger
[Windows contact]: mailto:simon@joyful.com

[Arch&nbsp;Linux]: https://www.archlinux.org/packages/?sort=&q=hledger
[Arch&nbsp;Linux contact]: mailto:

[Debian]: https://packages.debian.org/search?searchon=names&keywords=hledger
[Debian contact]: mailto:debian-haskell@lists.debian.org

[Fedora]: https://apps.fedoraproject.org/packages/s/hledger
[Fedora contact]: mailto:

[Gentoo]: https://gentoo.zugaina.org/Search?search=hledger
[Gentoo contact]: mailto:

[Ubuntu]: https://packages.ubuntu.com/search?suite=all&searchon=names&keywords=hledger
[Ubuntu contact]: mailto:

[Void&nbsp;Linux]: https://voidlinux.org/packages/?q=hledger
[Void&nbsp;Linux contact]: mailto:

[Nix]: http://hydra.nixos.org/search?query=hledger
[Nix contact]: mailto:


<a name="b"></a>

## b. Build the latest release

Good choice! You'll get the latest features and fixes mentioned in the [release notes](release-notes.html),
and you'll be in a good position to give feedback and get support.
Below are three ways to build the latest release, in order of preference.
But first, some tips:

- Building all hledger tools and dependencies for the first time could
  take as much as an hour, 1-2G of free memory, and 1-2G of disk
  space.  You can kill and restart the build without losing progress,
  and future builds will be much faster.

- If you get link errors (eg: "/bin/ld.gold: error: cannot find -ltinfo"), 
  you might need to install some extra system packages and try again.
  To find the right system package, check the list below, or do a web search for the error message
  (and please send updates for this list):

    |
    |-----------------|-------------------------------------------------------------------
    | CentOS:         | **`sudo yum install -y libstdc++-devel ncurses-devel zlib-devel`** <!-- https://github.com/simonmichael/hledger/issues/715 -->
    | Debian, Ubuntu: | **`sudo apt install -y libtinfo-dev`**
    | Fedora, RHEL:   | **`sudo dnf install -y gmp-devel ncurses-devel`**

- Here are some known build issues and workarounds on certain platforms:

    <span class=warnings>
    [arch: advice from Arch wiki](https://wiki.archlinux.org/index.php/Haskell)\
    [arch: No information found for ghc-8.4.2](https://github.com/commercialhaskell/stack/issues/3984)\
    <!-- [arch: some past problems](https://github.com/simonmichael/hledger/issues/668) -->
    [freebsd 12: no cabal file found](https://github.com/simonmichael/hledger/issues/709)\
    [openbsd 6: exec permission denied](https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html)\
    [openbsd: how to get stack](https://github.com/commercialhaskell/stack/issues/2822#issuecomment-318892816)\
    [windows: hledger-ui is not available](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444)\
    </span>

- If you have trouble, please send me a copy/paste of the output,
  including the commands you typed, at least up to the first error, via
      [paste](http://paste.hledger.org) + [IRC](http://irc.hledger.org),
  or the [issue tracker](http://bugs.hledger.org), 
  or [email](docs.html#helpfeedback).


<div style="margin-left:1em; margin-bottom:2em;">

<a name="b1"></a>

### b1. with hledger-install

On systems with bash installed (mac, linux, unix-like windows..),
if you don't already have stack or cabal, or if you are having trouble with them,
[hledger-install](https://github.com/simonmichael/hledger/tree/master/hledger-install)
is an easy and reliable way to get the latest hledger.
It automates the install process using stack or cabal, avoiding common pitfalls:

  **`curl -s https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh > hledger-install.sh`**\
  **`less hledger-install.sh`**  *# satisfy yourself that the script is safe*\
  **`bash hledger-install.sh`**

<a name="b2"></a>

### b2. with stack

[`stack`](http://haskell-lang.org/get-started) is the more reliable of Haskell's two build tools, for new users.
You need stack 1.7.1 or newer; the latest release is best.
On 64-bit Windows, use the 64-bit version of stack.
The following command installs the main hledger packages;
you can save some time by omitting hledger-ui, hledger-web and/or hledger-api (optional user interfaces).
On Windows, hledger-ui is not available.
To estimate the build time, add `--dry-run`:

  **`stack install --resolver=lts-12 megaparsec-7.0.4 cassava-megaparsec-2.0.0 config-ini-0.2.3.0 hledger-lib-1.12 hledger-1.12 hledger-ui-1.12 hledger-web-1.12 hledger-api-1.12`**\

Other [add-ons](/hledger.html#third-party-add-ons)
like
[hledger-diff](http://hackage.haskell.org/package/hledger-diff),
[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd),
or [hledger-interest](http://hackage.haskell.org/package/hledger-interest)
can be installed like so:

  **`stack install --resolver=lts-12 megaparsec-7.0.4 cassava-megaparsec-2.0.0 config-ini-0.2.3.0 hledger-lib-1.12 hledger-diff-0.2.0.14 hledger-iadd-1.3.7 hledger-interest-1.5.3`**\

<a name="b3"></a>

### b3. with cabal

[cabal](https://www.haskell.org/cabal/) is the other Haskell build tool. If you're a cabal expert, use it in the usual way, eg:

  **`cabal update`**\
  **`cabal install hledger-1.12 hledger-ui-1.12 hledger-web-1.12 hledger-api-1.12 hledger-diff-0.2.0.14 hledger-iadd-1.3.7 hledger-interest-1.5.3`**\

</div>

#### Set up PATH

You will probably see a message about where the executables were installed.
After installation, make sure this install directory is configured in your shell's \$PATH
(preferably near the start of it, to preempt older hledger system packages you may have installed).
The install directory is:

|                    | on non-Windows systems | on Windows 
|--------------------|------------------------|------------------------------------------
| If stack was used  | `$HOME/.local/bin`     | `%APPDATA%\local\bin` (eg&nbsp;`C:\Users\Joe\AppData\Roaming\local\bin`)
| If cabal was used  | `$HOME/.cabal/bin`     | `%APPDATA%\cabal\bin`

How to configure \$PATH is platform- and shell-specific.
If you are using bash, this should take care of it:

  **`echo "export PATH=~/.local/bin:~/.cabal/bin:$PATH" >> ~/.bashrc && source ~/.bashrc`**

\

#### Test the installation

You should now be able to run the hledger tools (whichever ones you installed) and see the expected versions:

  `$`**`hledger --version`**\
  `hledger 1.12`\
  `$`**`hledger-ui --version`**\
  `hledger-ui 1.12`\
  `$`**`hledger web --version`**\
  `hledger-web 1.12`\
  `$`**`hledger iadd --version`**\
  `This is hledger-iadd version 1.3.7`\

And you can check that the unit tests pass (just for fun):

  `$`**`hledger test`**\
  `...`\
  `✅  176 tests passed, no failures! 👍 🎉`\



<a name="c"></a>

## c. Build the development version

Also a good choice. Our master branch is stable enough for daily use,
and includes the [latest improvements](https://github.com/simonmichael/hledger/commits/master).
You'll need [git](https://en.wikipedia.org/wiki/Git) and 
[`stack`](http://haskell-lang.org/get-started) or [cabal](https://www.haskell.org/cabal/).
Eg, this will build and install all of the main hledger tools using stack:

  **`git clone https://github.com/simonmichael/hledger`**\
  **`cd hledger`**\
  **`stack install`**\

cabal users may find the `cabal-install.sh` or `cabal.project` files useful.

See the troubleshooting, PATH, and test tips [above](#b).
Note development builds will have a .99 suffix (eg 1.12.99 means "1.13-dev").

