# Download

Here are the ways to install hledger, organised by platform, with the usually most up-to-date methods at the top.
Please [send updates](index.html#helpfeedback) for 
[this page](https://github.com/simonmichael/hledger/blob/master/site/download.md),
and help packagers keep their platform packages up to date.
hledger's usual release window is the first day or two of the month.

<style>
table { margin-left:1em; }
tr { /*border-top:thin solid #ddd;*/ border-bottom:thin solid #ddd; }
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

|                                |                              | Latest&nbsp;release&nbsp;is&nbsp;1.14.1<br>([release notes](http://hledger.org/release-notes)) <!-- should be the latest release of the hledger package -->  <!-- [![latest version](https://repology.org/badge/latest-versions/hledger.svg)](http://hledger.org/release-notes)  -->
|--------------------------------|------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
|
| <br><big>**Multiplatform**</big>         | <br><small>*The first three build from source, which is slower. See [Building from source](#b).*</small> | <br><small>*This method installs:*</small>
| [hledger-install](#b1)<br><small>*Linux,&nbsp;Mac,&nbsp;WSL*</small><br><small>*Requires only bash.*</small> | <span style="font-size:small;">**`curl -sO https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh`**<br>**`less hledger-install.sh`**  *# satisfy yourself that the script is safe*<br>**`bash hledger-install.sh`**</span> | <small>Latest release</small>
| [stack install](#b2)<br><small>*Linux,&nbsp;Mac,&nbsp;Windows[*][]*</small>  | <span style="font-size:medium;">**`stack install --resolver=nightly-2019-03-09 hledger hledger-web hledger-ui`**</span> | <small>Latest release / any version</small>
| [cabal install](#b3)<br><small>*Linux,&nbsp;Mac,&nbsp;Windows[*][]*</small>  | <span style="font-size:medium;">**`cabal v2-update && cabal v2-install hledger-1.14.1 hledger-web-1.14 hledger-ui-1.14`**</span> | <small>Latest release / any version</small>
|
| [Docker][]<br><small>*Linux,&nbsp;Mac,&nbsp;Windows*</small>  | **`docker pull dastapov/hledger`** ([readme](https://hub.docker.com/r/dastapov/hledger), [more images](https://hub.docker.com/search?q=hledger&type=image&sort=updated_at&order=desc)) | 1.14.1
| [Nix][]<br><small>*Linux,&nbsp;Mac*</small>  | **`nix-channel --update && nix-env -i hledger hledger-ui hledger-web`**  | [![nixpkgs unstable](https://repology.org/badge/version-for-repo/nix_unstable/hledger.svg)](http://hydra.nixos.org/search?query=hledger) [![nixpkgs stable](https://repology.org/badge/version-for-repo/nix_stable/hledger.svg)](http://hydra.nixos.org/search?query=hledger) 
|
| <br><big>**Mac**</big>         |
| [Homebrew][]                   | **`brew install hledger`**   | [![Homebrew](https://repology.org/badge/version-for-repo/homebrew/hledger.svg)](https://repology.org/metapackage/hledger/versions)
|
| <br><big>**Windows**</big>     |
| [Windows&nbsp;binaries][]      | <code>**[hledger.zip][]**</code> <small>*(from Appveyor CI)*</small> <!-- or [latest nightly dev build](https://ci.appveyor.com/api/projects/simonmichael/hledger/artifacts/hledger.zip?branch=master) --><!-- <span class=warnings> [no hledger-ui](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444),[doesn't work on old windows ?](https://github.com/simonmichael/hledger/issues/774),[many files in PATH causing hangs](https://github.com/simonmichael/hledger/issues/791) --><!-- ,[appveyor builds failing](https://github.com/simonmichael/hledger/issues/832) </span>--> | 1.14.1+ 20190309
| [Linuxbrew][]                  | **`brew install hledger`**    | [![Linuxbrew](https://repology.org/badge/version-for-repo/linuxbrew/hledger.svg)](https://repology.org/metapackage/hledger/versions)
|
| <br><big>**GNU/Linux**</big>   |
| [Linuxbrew][]                  | **`brew install hledger`**    | [![Linuxbrew](https://repology.org/badge/version-for-repo/linuxbrew/hledger.svg)](https://repology.org/metapackage/hledger/versions)
| [Arch][]                       | **`pacman -S hledger hledger-ui hledger-web`** | [![Arch](https://repology.org/badge/version-for-repo/arch/hledger.svg)](https://repology.org/metapackage/hledger/versions)
| [Gentoo][]                     | **`sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`** | 1.14.1
| [Debian][]                     | **`sudo apt install hledger hledger-ui hledger-web`** | [![Debian Unstable](https://repology.org/badge/version-for-repo/debian_unstable/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Debian Testing](https://repology.org/badge/version-for-repo/debian_testing/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Debian Stable](https://repology.org/badge/version-for-repo/debian_stable/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Debian Oldstable](https://repology.org/badge/version-for-repo/debian_oldstable/hledger.svg)](https://repology.org/metapackage/hledger/versions) 
| [Ubuntu][]                     | **`sudo apt install hledger hledger-ui hledger-web`** | [![Ubuntu 19.04](https://repology.org/badge/version-for-repo/ubuntu_19_04/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Ubuntu 18.10](https://repology.org/badge/version-for-repo/ubuntu_18_10/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Ubuntu 18.04](https://repology.org/badge/version-for-repo/ubuntu_18_04/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Ubuntu 17.10](https://repology.org/badge/version-for-repo/ubuntu_17_10/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Ubuntu 16.04](https://repology.org/badge/version-for-repo/ubuntu_16_04/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Ubuntu 14.04](https://repology.org/badge/version-for-repo/ubuntu_14_04/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Ubuntu 12.04](https://repology.org/badge/version-for-repo/ubuntu_12_04/hledger.svg)](https://repology.org/metapackage/hledger/versions) 
| [Void][]                       | **`xbps-install -S hledger hledger-ui hledger-web`** | [![Void Linux x86_64](https://repology.org/badge/version-for-repo/void_x86_64/hledger.svg)](https://repology.org/metapackage/hledger/versions)
| [Fedora][]                     | **`sudo dnf install hledger`** <br><small>*or (more complete & current):*</small><br>**`sudo dnf copr enable kefah/HLedger && sudo dnf install hledger`** | [![Fedora Rawhide](https://repology.org/badge/version-for-repo/fedora_rawhide/hledger.svg)](https://repology.org/metapackage/hledger/versions)  [![Fedora 29](https://repology.org/badge/version-for-repo/fedora_29/hledger.svg)](https://repology.org/metapackage/hledger/versions)  [![Fedora 28](https://repology.org/badge/version-for-repo/fedora_28/hledger.svg)](https://repology.org/metapackage/hledger/versions)  [![Fedora 27](https://repology.org/badge/version-for-repo/fedora_27/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Fedora 26](https://repology.org/badge/version-for-repo/fedora_26/hledger.svg)](https://repology.org/metapackage/hledger/versions)
|
| <br><big>**BSD**</big>         |
| OpenBSD                        | Ports: **[https://github.com/jasperla/openbsd-wip/pull/104](https://github.com/jasperla/openbsd-wip/pull/104)** <br>Third-party binaries: **[OpenBSD6.3/amd64](https://s3.amazonaws.com/openbsd-hledger/index.html)** | 1.10
|
| <br><big>**Other**</big>       |
| [Sandstorm][]<br><small>*Community/private cloud platform*</small>                  | **[hledger-web Sandstorm app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90)** <!-- <br><span class=warnings>[features needed](https://github.com/simonmichael/hledger/issues/425)</span> --> | 1.9.2
|
<!--
| [Homebrew][]/[Linuxbrew][]<br><small>*Mac,&nbsp;Linux,&nbsp;Windows*</small> | **`brew install hledger`**   | [![Homebrew](https://repology.org/badge/version-for-repo/homebrew/hledger.svg)](https://repology.org/metapackage/hledger/versions) [![Linuxbrew](https://repology.org/badge/version-for-repo/linuxbrew/hledger.svg)](https://repology.org/metapackage/hledger/versions)
-->

[hledger.zip]:   https://ci.appveyor.com/api/buildjobs/gudfa3gv7pj94ab0/artifacts/hledger.zip
[hledger.zip.1]: https://ci.appveyor.com/api/buildjobs/xmgh6j3ywi125xhp/artifacts/hledger.zip

[Homebrew]: https://formulae.brew.sh/formula/hledger
[Homebrew contact]: @albins, simon@joyful.com
[Linuxbrew]: https://linuxbrew.sh
[Linuxbrew contact]: @albins, simon@joyful.com
[Windows&nbsp;binaries]: https://ci.appveyor.com/project/simonmichael/hledger
[Windows binaries contact]: simon@joyful.com
[*]: #build-issues
[Arch]: https://www.archlinux.org/packages/?sort=&q=hledger
[Arch contact]: ?
[Debian]: https://packages.debian.org/search?searchon=names&keywords=hledger
[Debian contact]: mailto:debian-haskell@lists.debian.org, Clint
[Fedora]: https://apps.fedoraproject.org/packages/s/hledger
[Fedora contact]: ?
[Gentoo]: https://gentoo.zugaina.org/Search?search=hledger
[Gentoo contact]: ?
[Ubuntu]: https://packages.ubuntu.com/search?suite=all&searchon=names&keywords=hledger
[Ubuntu contact]: ?
[Void]: https://voidlinux.org/packages/?q=hledger
[Void contact]: ?
[Docker]: https://www.docker.com/products/docker-desktop
[Docker contact]: @adept
[Nix]: https://nixos.org/nix
[Nix contact]: @peti
[Sandstorm]: https://sandstorm.io
[Sandstorm contact]: 


<a name="b"></a>

## Building from source

Building hledger from source takes a bit longer than using a prebuilt package,
but you can be sure of getting the latest release (or the latest dev version).

Below are three ways to build hledger. But first, some general tips:

- Building Haskell programs involves downloading and building and
  optimising a lot of dependencies. If you haven't built Haskell
  software before, your first hledger build could take up to an
  hour, 1-2G of free memory, and 1-2G of disk.

- Future builds will be much faster.

- You can kill a build and restart it later without losing progress.

- If you get link errors (eg: "/bin/ld.gold: error: cannot find -ltinfo"), 
  you might need to install some extra system packages 
  (C packages provided by your platform - not haskell packages)
  and try again. To find the right system package, check the list
  below, or do a web search for the error message (and please send
  updates for this list):

    |
    |-----------------|-------------------------------------------------------------------
    | CentOS:         | **`sudo yum install -y libstdc++-devel ncurses-devel zlib-devel`** <!-- https://github.com/simonmichael/hledger/issues/715 -->
    | Debian, Ubuntu: | **`sudo apt install -y libtinfo-dev`**
    | Fedora, RHEL:   | **`sudo dnf install -y gmp-devel ncurses-devel`**

- <a name="build-issues"></a>Here are some known build issues and workarounds on certain platforms:

    <span class=warnings>
    [windows: hledger-ui is not available](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444)\
    [windows: build hangs using GHC 8.6.3](https://github.com/well-typed/generics-sop/issues/93)\
    [arch: haskell build advice from Arch wiki](https://wiki.archlinux.org/index.php/Haskell)\
    [arch: No information found for ghc-8.4.2](https://github.com/commercialhaskell/stack/issues/3984)\
    <!-- [arch: some past problems](https://github.com/simonmichael/hledger/issues/668) -->
    [freebsd 12: no cabal file found](https://github.com/simonmichael/hledger/issues/709)\
    [openbsd 6: exec permission denied](https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html)\
    [openbsd: how to get stack](https://github.com/commercialhaskell/stack/issues/2822#issuecomment-318892816)\
    </span>

- If you have trouble, please send me a copy/paste of the output,
  including the commands you typed, at least up to the first error, via
      [paste](http://paste.hledger.org) + [IRC](http://irc.hledger.org),
  or the [issue tracker](http://bugs.hledger.org), 
  or [email](index.html#helpfeedback).


<a name="b1"></a>

### Building with hledger-install.sh

If you don't already have stack or cabal, or if you are having trouble with them,
[hledger-install.sh](https://github.com/simonmichael/hledger/tree/master/hledger-install)
is an easy and reliable way to get the latest hledger,
on systems which have the bash shell (and are supported by GHC: mac, linux, unix-like environments on windows..).
It automates the installation of stack or cabal and then building the hledger tools, avoiding common pitfalls.
Here's how to run it (or, some folks will prefer to pipe it directly into bash):

  **`curl -s https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh > hledger-install.sh`**\
  **`less hledger-install.sh`**  *# satisfy yourself that the script is safe*\
  **`bash hledger-install.sh`**

<a name="b2"></a>

### Building with stack

[`stack`](http://haskell-lang.org/get-started) is the more reliable of Haskell's two build tools for new users.
You need stack 1.7.1 or newer; the latest release (eg 1.9.3) is the most reliable.
You can often run `stack upgrade` to upgrade it.
64-bit Windows users should choose the 64-bit version of stack.
The following command installs the main hledger packages;

  **`stack install --resolver=lts-13 hledger-lib-1.14 hledger-1.14.1 hledger-web-1.14 \`**\
  &nbsp;&nbsp;**`hledger-ui-1.14 brick-0.46 text-zipper-0.10.1 config-ini-0.2.4.0 data-clist-0.1.2.2 word-wrap-0.4.1`**\

If needed, you can save some time by omitting the
[hledger-web](http://hackage.haskell.org/package/hledger-web) and
[hledger-ui](http://hackage.haskell.org/package/hledger-ui) packages
(and hledger-ui's extra dependencies: brick, text-zipper, config-ini etc.)
On Windows, hledger-ui is not available.
To estimate the build time remaining, add `--dry-run`.

Some other [add-on tools](/hledger.html#third-party-add-ons) like
[hledger-api](http://hackage.haskell.org/package/hledger-api),
[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd),
and [hledger-interest](http://hackage.haskell.org/package/hledger-interest)
can be installed like so:

  **`stack install --resolver=lts-13 hledger-lib-1.14 hledger-1.14.1 hledger-api-1.14 \`**\
  &nbsp;&nbsp;**`hledger-interest-1.5.3 hledger-iadd-1.3.9 brick-0.46 text-zipper-0.10.1 config-ini-0.2.4.0 data-clist-0.1.2.2 word-wrap-0.4.1`**\

<a name="b3"></a>

### Building with cabal

[cabal](https://www.haskell.org/cabal/) is the other Haskell build tool. If you're a cabal expert, use it in the usual way, eg:

  **`cabal v2-update`**\
  **`cabal v2-install hledger-1.14.1 hledger-ui-1.14 hledger-web-1.14 \`**\
  &nbsp;&nbsp;**`hledger-api-1.14 hledger-iadd-1.3.9 hledger-interest-1.5.3`**\

### Set up PATH

After building you may see a message about where the executables were installed.
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

### Test the installation

You should now be able to run the hledger tools (whichever ones you installed) and see the expected versions:

  `$`**`hledger --version`**\
  `hledger 1.14.1`\
  `$`**`hledger-ui --version`**\
  `hledger-ui 1.14`\
  `$`**`hledger web --version`**\
  `hledger-web 1.14`\
  `$`**`hledger iadd --version`**\
  `This is hledger-iadd version 1.3.9`\

And you can check that the unit tests pass (just for fun):

  `$`**`hledger test`**\
  `...`\
  `✅  198 tests passed, no failures! 👍 🎉`\

<a name="c"></a>

### Building the development version

The master branch in hledger's git repo is stable enough for daily use,
and includes the [latest improvements](https://github.com/simonmichael/hledger/commits/master).
You'll need [git](https://en.wikipedia.org/wiki/Git) and 
[`stack`](http://haskell-lang.org/get-started) or [cabal](https://www.haskell.org/cabal/).
This will build and install all of the main hledger tools using stack:

  **`git clone https://github.com/simonmichael/hledger`**\
  **`cd hledger`**\
  **`stack install`**\

cabal users may find the `cabal-install.sh` or `cabal.project` files useful.

The --version output of development builds has a .99 suffix meaning "dev".
So 1.14.99 means "1.15-dev", the in-development version of 1.15.

