# Download

Would you like to..
<a name="a"></a>

## A. download a packaged version ?

<style>
tr { vertical-align:top; }
td { padding-bottom:.5em; padding-right:1em; }
td:first-of-type { 
  /* white-space:nowrap; */
  /* width:1%; */
}
a { white-space:nowrap; }
</style>

Packaged versions are the quickest to install, 
but they can be [out of date](https://repology.org/metapackage/hledger/badges) or incomplete. 


|
|----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| Windows:             | [Latest developer builds](https://ci.appveyor.com/project/simonmichael/hledger/build/artifacts) (no hledger-ui, [please help](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444))
| Mac:                 | **`brew install hledger`** (CLI only, [please help ](https://github.com/simonmichael/hledger/issues/321#issuecomment-179920520))
| Arch Linux:          | **`pacman -S hledger`** (haskell problems are common on Arch at present, cf #668 [1](https://github.com/simonmichael/hledger/issues/668#issuecomment-352197500), [2](https://github.com/simonmichael/hledger/issues/668#issuecomment-355107667))
| Debian,&nbsp;Ubuntu: | **`sudo apt install hledger hledger-ui hledger-web`**
| Fedora,&nbsp;RHEL:   | **`sudo dnf install hledger`**
| Gentoo:              | **`sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`**
| Void Linux:          | **`xbps-install -S hledger hledger-ui hledger-web hledger-api`**
| NixOS:               | **`nix-env -iA nixpkgs.haskellPackages.hledger \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-ui \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-web`** (problems building hledger-ui on MacOS, [please help](https://github.com/simonmichael/hledger/issues/613))
| Sandstorm:           | **[hledger-web Sandstorm app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90) -> demo** (get a hledger-web server in 3 clicks. Features needed, [please help](https://github.com/simonmichael/hledger/issues/425))


<a name="b"></a>

## B. build the latest release ?

Good choice! The [release notes](release-notes.html) show what you'll get.

Just know that building haskell apps for the first time can take significant time (minutes to hours), 
memory (eg 1-2G), and disk space (eg 1G in $HOME/.stack or $HOME/.cabal, though you can delete much of that afterward).
Here are three methods:

### 1. hledger-install

On POSIX systems (mac/linux/freebsd..), the
[hledger-install.sh](https://github.com/simonmichael/hledger/tree/master/hledger-install) script is 
the build method most likely to just work, as the only prerequisite is bash and it avoids many common gotchas.
It uses stack or cabal, installing stack if needed, to build the latest release of hledger and all addon tools,
one at a time, installing everything in $HOME/.local/bin (or $HOME/.cabal/bin).

Here's the responsible way to run it:

 **`curl -O https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh`**\
 **`less hledger-install.sh`**  *# do security review*\
 **`bash hledger-install.sh`**  *# run it; add `-v` for more detail, or use `bash -x` to show commands being run* 

and here's the easy way:

 **`curl https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh | bash`**

You can kill and restart the installer without losing progress.
If it fails, please help us improve it by [reporting the full output](docs.html#helpfeedback).

### 2. stack

[`stack`](http://haskell-lang.org/get-started) is the newer and easier of the two Haskell build tools.
Here's how to use it directly to install hledger:

1. **Install or upgrade to the latest stack**\
   The latest version of stack is recommended, as it is the best at avoiding ecosystem breakages and most likely to just work.
   If you already have stack 1.3 or greater, you can usually run `stack upgrade` to quickly upgrade it.
   On Windows, the 64-bit version of stack is [recommended](https://github.com/simonmichael/hledger/issues/275#issuecomment-123834252).
   <!-- On Arch, you [may need to also install GHC manually](https://github.com/simonmichael/hledger/issues/434).\ -->
   Follow stack's advice to 
   [add `$HOME/.local/bin` to your \$PATH](https://docs.haskellstack.org/en/stable/install_and_upgrade/#path) (`%APPDATA%\local\bin` on Windows).
   Eg, if you're a bash user:\
   &nbsp;&nbsp;`echo "export PATH=$PATH:~/.local/bin" >> ~/.bashrc && source ~/.bashrc`

2. **`stack install --resolver=nightly hledger-lib-1.5 hledger-1.5 [hledger-ui-1.5] [hledger-web-1.5] [hledger-api-1.5]`**\   
   This installs the specified hledger packages (and required haskell libraries and tools) from [Stackage](https://www.stackage.org) (and if needed, [Hackage](http://hackage.haskell.org)).
   Specifying stackage's nightly resolver (snapshot) requires the most rebuilding, but is the most reliable;
   you can try reducing build time by specifying another resolver that you've installed from previously, or no --resolver option.\
   Add `--dry-run` to see the build plan. You can kill and restart this without losing progress.

    The bracketed packages are optional; if you include them, don't type the brackets, and do always 
    include the preceding hledger-lib and hledger packages in the command, otherwise stack may complain.
    hledger-ui is [not yet available on Windows](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444), alas.

    If you get errors due to missing C libraries like curses or terminfo, you'll need to find out the corresponding
    system packages and install those manually. Eg:

    |
    |-----------------|-----------------------------------
    | Debian, Ubuntu: | `sudo apt install libncurses5-dev` 
    | Fedora, RHEL:   | `sudo dnf install ncurses-devel`

<!--
    If you need to build with an older GHC version for some reason, these commands should work
   (except on Mac Sierra which [requires at least GHC 8.0.2/lts-8](https://ghc.haskell.org/trac/ghc/ticket/12479)):\
   `stack install --resolver lts-7 hledger-lib-1.3 hledger-1.3 hledger-ui-1.3 hledger-web-1.3 hledger-api-1.3 brick-0.19 vty-5.15.1 data-clist-0.1.2.0`  *# (GHC 8.0.1)* \
   `stack install --resolver lts-6 hledger-lib-1.3 hledger-1.3 hledger-ui-1.3 hledger-web-1.3 hledger-api-1.3 megaparsec-5.3.1 brick-0.19 vty-5.15.1 data-clist-0.1.2.0 text-zipper-0.10`  *# (GHC 7.10.3)* \
-->
   <!-- keep synced with stack.yaml files -->

Now you should be able to run `hledger --version` 
(and `hledger-ui --version`, `hledger-web --version` etc. if installed)
and see the latest version number.
Additional [add-on commands](/hledger.html#third-party-add-ons),
such as
[hledger-diff](http://hackage.haskell.org/package/hledger-diff),
[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd),
[hledger-interest](http://hackage.haskell.org/package/hledger-interest),
and [hledger-irr](http://hackage.haskell.org/package/hledger-irr)
can be installed similarly to the above. Eg:\
&nbsp;&nbsp;`stack install --resolver nightly hledger-lib-1.5 hledger-1.5 hledger-iadd-1.3.1`


### 3. cabal

[cabal](https://www.haskell.org/cabal/) is the other Haskell build tool. If you're a cabal expert, feel free to use this instead.

<a name="c"></a>

## C. build the development version ?

Also a good choice. Our master branch is kept stable enough for daily use,
and includes the very latest improvements ([commits](https://github.com/simonmichael/hledger/commits/master)).

1. **Install [`stack`](#stack) and [git](https://en.wikipedia.org/wiki/Git)**
2. **`git clone https://github.com/simonmichael/hledger`**
3. **`cd hledger`**
4. **`stack install [hledger]`**  *# optionally specify just the hledger package to build less* 

cabal users may find the `cabal-install.sh` or `cabal.project` files useful.

<!-- now covered by stack.yaml I think:
 Nix users taking advantage of Stack integration may need to use Stack's `--no-nix-pure` flag to build hledger. -->
