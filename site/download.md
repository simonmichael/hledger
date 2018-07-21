\$toc\$

# Download

Adventurer, choose your path! No, really it's quite easy. Often. Do you want to..
<a name="a"></a>

## a. download a binary/system package

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
    font-style:italic;
}
.warnings > a:before {
    content: "⚠ ";
    color:red;
}
</style>

hledger binaries or system packages are quickest to install,
but they can be [out of date](https://repology.org/metapackage/hledger/badges) or incomplete.
(Please help your local packagers with this!).

|
|----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| Windows:             | [1.10](https://ci.appveyor.com/api/buildjobs/5n63x22wvd4j24ee/artifacts/hledger.zip) or [latest nightly dev build](https://ci.appveyor.com/api/projects/simonmichael/hledger/artifacts/hledger.zip?branch=master) (<span class=warnings>[no hledger-ui](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444), [doesn't work on old windows ?](https://github.com/simonmichael/hledger/issues/774), [unusually many files in PATH causing hangs](https://github.com/simonmichael/hledger/issues/791), [appveyor builds have been failing again](https://github.com/simonmichael/hledger/issues/832) )</span>
| Mac:                 | **`brew install hledger`** <span class=warnings>([only hledger CLI](https://github.com/simonmichael/hledger/issues/321#issuecomment-179920520))</span>
| Arch Linux:          | **`pacman -S hledger`**
| Debian,&nbsp;Ubuntu: | **`sudo apt install hledger hledger-ui hledger-web`**
| Fedora,&nbsp;RHEL:   | **`sudo dnf install hledger`**
| Gentoo:              | **`sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`**
| Void Linux:          | **`xbps-install -S hledger hledger-ui hledger-web hledger-api`**
| NixOS:               | **`nix-env -iA nixpkgs.haskellPackages.hledger nixpkgs.haskellPackages.hledger-ui nixpkgs.haskellPackages.hledger-web`** <span class=warnings>([problems with hledger-ui on MacOS ?](https://github.com/simonmichael/hledger/issues/613))</span>
| Sandstorm:           | **[hledger-web Sandstorm app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90) -> demo** - a hledger-web server in 3 clicks <span class=warnings>([features needed](https://github.com/simonmichael/hledger/issues/425))</span>


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
on GNU/linux, mac and freeBSD
(and possibly other BSDs if you install cabal-install first, or on unixlike environments on windows):

- it requires only bash and curl/wget, and internet access
- it automates the install process using stack or cabal, avoiding common pitfalls
- it installs stack and GHC in ~/.stack, if needed
- it installs the latest release of hledger and addon tools in ~/.local/bin or ~/.cabal/bin

Here's the quick, non-secure way to run it:

 **`curl https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh | bash`**

And here's the more responsible way:

 **`curl -O https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh`**\
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
hledger 1.10
$ hledger-ui --version
hledger-ui 1.10
$ hledger web --version
hledger-web 1.10
$ hledger iadd --version
This is hledger-iadd version 1.3.5
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

2. **`stack install --resolver=lts-12 hledger-lib-1.10 hledger-1.10 hledger-ui-1.10.1 hledger-web-1.10 hledger-api-1.10`**\
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
