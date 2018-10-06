# Download

There are a number of ways to install hledger, with different speed/freshness/security tradeoffs:

a. [Download the binary or system package for your platform](#a.-download-a-binary-or-system-package) (quick, often not the latest version)
b. [Build the latest release with hledger-install](#b.-build-the-latest-release)
  or [with stack](#b2.-with-stack)
  or [with cabal](#b3.-with-cabal) (slow, fresh)
c. [Build the development version with stack or cabal](#c.-build-the-development-version) (slow, super-fresh)


<a name="a"></a>

## a. Download a binary or system package

<style>
table { margin-left:1em; }
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
(Please help/report issues to packagers.)

**Available binaries and system packages:**

| Platform             | Command/Link           | Installs&nbsp;version<br>([as&nbsp;of&nbsp;20181006](https://repology.org/metapackage/hledger/badges), latest is 1.11)
|----------------------|------------------------|----------------------------------------------------------------------------------------
| Mac                  | **`brew install hledger`** <br><span class=warnings>[only hledger CLI](https://github.com/simonmichael/hledger/issues/321#issuecomment-179920520)</span> | 1.10
| Windows              | Developer binaries: **[1.10](https://ci.appveyor.com/api/buildjobs/5n63x22wvd4j24ee/artifacts/hledger.zip)** <!-- or [latest nightly dev build](https://ci.appveyor.com/api/projects/simonmichael/hledger/artifacts/hledger.zip?branch=master) --> <br><span class=warnings> [no hledger-ui](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444),[doesn't work on old windows ?](https://github.com/simonmichael/hledger/issues/774),[many files in PATH causing hangs](https://github.com/simonmichael/hledger/issues/791)<!-- ,[appveyor builds failing](https://github.com/simonmichael/hledger/issues/832) --> </span> | 1.10
| &nbsp;               |
| Arch&nbsp;Linux      | **`pacman -S hledger`** | 1.11
| Debian               | **`sudo apt install hledger hledger-ui hledger-web`** | 1.0.1&nbsp;(Stable), 1.5&nbsp;(Testing), 1.10&nbsp;(Unstable)
| Fedora               | **`sudo dnf install hledger`** | 1.2&nbsp;(27), 1.4&nbsp;(28), 1.5&nbsp;(Rawhide)
| Gentoo               | **`sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`** | 1.11
| RHEL                 | **`sudo dnf install hledger`** <span class=warnings>?</span> | <span class=warnings>?</span>
| Ubuntu               | **`sudo apt install hledger hledger-ui hledger-web`** | 0.26&nbsp;(16.04&nbsp;Xenial), 1.2&nbsp;(18.04&nbsp;Bionic), 1.5&nbsp;(18.10&nbsp;Cosmic)
| Void&nbsp;Linux      | **`xbps-install -S hledger hledger-ui hledger-web hledger-api`** | 1.10
| &nbsp;               |
| FreeBSD              | <span class=warnings>?</span> | 
| NetBSD               | <span class=warnings>?</span> | 
| OpenBSD              | Ports: **[https://github.com/jasperla/openbsd-wip/pull/104](https://github.com/jasperla/openbsd-wip/pull/104)** <br>Third-party binaries: **[OpenBSD6.3/amd64](https://s3.amazonaws.com/openbsd-hledger/index.html)** | 1.10
| &nbsp;               |
| NixOS                | **<span style="font-size:small;">`nix-env -iA nixpkgs.haskellPackages.hledger nixpkgs.haskellPackages.hledger-ui nixpkgs.haskellPackages.hledger-web`</span>** <br><span class=warnings>[problems with hledger-ui on MacOS ?](https://github.com/simonmichael/hledger/issues/613)</span> | 1.5&nbsp;(stable), 1.11&nbsp;(unstable)
| Sandstorm            | **[hledger-web Sandstorm app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90)** <br><span class=warnings>[features needed](https://github.com/simonmichael/hledger/issues/425)</span> | 1.9.2


<a name="b"></a>

## b. build the latest release

Good choice! You'll get the latest features mentioned in the [release notes](release-notes.html).
Below are three ways to build the latest release, in order of preference.

Note, building all hledger tools for the first time could take as much
as an hour, 1G of free memory, and 1G of disk space. 
(We're not bloated; we just sit atop a lot of fine Haskell engineering!)
You can kill and restart it without losing progress, and subsequent builds will be much faster.
Also, here are some known build issues and workarounds:\
<span class=warnings>
[arch: advice from Arch wiki](https://wiki.archlinux.org/index.php/Haskell)\
[arch: No information found for ghc-8.4.2](https://github.com/commercialhaskell/stack/issues/3984)\
<!-- [arch: some past problems](https://github.com/simonmichael/hledger/issues/668) -->
[freebsd 12: no cabal file found](https://github.com/simonmichael/hledger/issues/709)\
[openbsd 6: exec permission denied](https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html)\
[openbsd: how to get stack](https://github.com/commercialhaskell/stack/issues/2822#issuecomment-318892816)\
</span>

<a name="b1"></a>

### b1. with hledger-install

The latest version of our [hledger-install script](https://github.com/simonmichael/hledger/tree/master/hledger-install)
([changes](https://github.com/simonmichael/hledger/commits/master/hledger-install/hledger-install.sh))
is recommended as the easiest and most-likely-to-just-work build method,
on at least GNU/linux and mac (it requires /bin/bash):

- it requires only bash and curl/wget, and internet access
- it automates the install process using stack or cabal, avoiding common pitfalls
- it installs stack and GHC in ~/.stack, if needed
- it installs the latest release of hledger and addon tools in ~/.local/bin or ~/.cabal/bin

Here's the quick, non-secure way to run it:

 **`curl -s https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh | bash`**

And here's the more responsible way:

 **`curl -sO https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh`**\
 **`less hledger-install.sh`**  *# do security review*\
 **`bash hledger-install.sh`**

#### Link errors ?

If you see link errors (like "/bin/ld.gold: error: cannot find -ltinfo"), 
you might need to install some extra system packages, such as the below, and try again.
Please do a web search for the error and send corrections for this list:

 |
 |-----------------|-------------------------------------------------------
 | Centos:         | **`sudo yum install -y libstdc++-devel ncurses-devel zlib-devel`** *# [?](https://github.com/simonmichael/hledger/issues/715)*
 | Debian, Ubuntu: | **`sudo apt install -y libtinfo-dev`** *# ?*
 | Fedora, RHEL:   | **`sudo dnf install -y gmp-devel ncurses-devel`** *# ?*

#### Set up $PATH

You should
[extend your \$PATH with `~/.local/bin`](https://docs.haskellstack.org/en/stable/install_and_upgrade/#path) (for stack)
and/or `~/.cabal/bin` (for cabal).
Eg, if you use bash:

**`echo "export PATH=~/.local/bin:~/.cabal/bin:$PATH" >> ~/.bashrc && source ~/.bashrc`**

#### Test

Now you should be able to run the hledger tools and see the expected versions. Eg:
```shell
$ hledger --version
hledger 1.11
$ hledger-ui --version
hledger-ui 1.11
$ hledger web --version
hledger-web 1.11
$ hledger iadd --version
This is hledger-iadd version 1.3.6
```

#### Need help ?

If you are having trouble, please capture a debug log and send it to me via 
[paste](http://paste.hledger.org) & [IRC](http://irc.hledger.org),
the [issue tracker](http://bugs.hledger.org),
or [email](docs.html#helpfeedback):

 **`bash -x hledger-install.sh 2>&1 | tee hledger-install.log`**

<a name="b2"></a>

### b2. with stack

[`stack`](http://haskell-lang.org/get-started) is the newer and easier of the Haskell build tools.
If you prefer more control or if hledger-install failed, here's how to use stack yourself:

1. **Install or upgrade to the latest stack**\
   The latest version of stack (1.7.1) is recommended, for best avoidance of ecosystem breakages.
   If an older version fails to install hledger, you should install the latest release of stack and try again.
   If you can get at least stack 1.3 installed, eg from your system packages, you can usually run `stack upgrade` to quickly upgrade it to the latest.

    On Windows, the 64-bit version of stack is [preferred](https://github.com/simonmichael/hledger/issues/275#issuecomment-123834252).

2. **`stack install --resolver=lts-12 cassava-megaparsec-1.0.0 hledger-lib-1.11 hledger-1.11 hledger-ui-1.11 hledger-web-1.11 hledger-api-1.11`**\
    This installs the main hledger packages (and dependencies) from [Stackage](https://www.stackage.org) and/or [Hackage](http://hackage.haskell.org).
    You can save some time by omitting hledger-* packages you don't want.\
    <span class=warnings>([windows: hledger-ui is not available](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444))</span>

    You can kill and restart this without losing progress. 
    To estimate the build time, add `--dry-run`. 
    
    If you see "was generated with a newer version of hpack, please upgrade and try again" errors, you can ignore them.
    (Upgrade to the latest stack release to stop them.)

<!--
    If you need to build with an older GHC version for some reason, these commands should work
   (except on Mac Sierra which [requires at least GHC 8.0.2/lts-8](https://ghc.haskell.org/trac/ghc/ticket/12479)):\
   `stack install --resolver=lts-7 hledger-lib-1.3 hledger-1.3 hledger-ui-1.3 hledger-web-1.3 hledger-api-1.3 brick-0.19 vty-5.15.1 data-clist-0.1.2.0`  *# (GHC 8.0.1)* \
   `stack install --resolver=lts-6 hledger-lib-1.3 hledger-1.3 hledger-ui-1.3 hledger-web-1.3 hledger-api-1.3 megaparsec-5.3.1 brick-0.19 vty-5.15.1 data-clist-0.1.2.0 text-zipper-0.10`  *# (GHC 7.10.3)* \
--> <!-- keep synced with stack.yaml files -->

3. **[If you see link errors..](#link-errors)**

4. **[Set up \$PATH](#set-up-path)**

5. **Install addons ?**\
   Additional [add-on commands](/hledger.html#third-party-add-ons),
   such as
   [hledger-diff](http://hackage.haskell.org/package/hledger-diff),
   [hledger-iadd](http://hackage.haskell.org/package/hledger-iadd),
   [hledger-interest](http://hackage.haskell.org/package/hledger-interest),
   and [hledger-irr](http://hackage.haskell.org/package/hledger-irr)
   can be installed similarly to the above.

6. **[Test](#test)**


<a name="b3"></a>

### b3. with cabal

[cabal](https://www.haskell.org/cabal/) is the other Haskell build tool. If you're a cabal expert, feel free to use this in the usual way.

<a name="c"></a>

## c. build the development version

Also a good choice. Our master branch is stable enough for daily use,
and includes the [latest improvements](https://github.com/simonmichael/hledger/commits/master).

1. **Install [git](https://en.wikipedia.org/wiki/Git) and [`stack`](#b2)**
2. **`git clone https://github.com/simonmichael/hledger`**
3. **`cd hledger`**
4. **`stack install [hledger]`**  *# build all (or, just the command line UI)* 
5. **[If you see link errors..](#link-errors)**
6. **[Set up \$PATH](#set-up-path)**
7. **[Test](#test)**
   (Development versions will have a .99 suffix, 1.10.99 means 1.11-dev)

cabal users may find the `cabal-install.sh` or `cabal.project` files useful.
