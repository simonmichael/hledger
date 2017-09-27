# Download

Choose a method:
<a name="a"></a>

## A. I want to download a packaged version

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
but they can be out of date or incomplete. 

|
|----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| Windows:             | [Latest developer builds](https://ci.appveyor.com/project/simonmichael/hledger/build/artifacts) (no hledger-ui, [please help](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444))
| Mac:                 | **`brew install hledger`** (CLI only, [please help ](https://github.com/simonmichael/hledger/issues/321#issuecomment-179920520))
| Arch Linux:          | **`pacman -S hledger`**
| Debian,&nbsp;Ubuntu: | **`sudo apt install hledger hledger-ui hledger-web`**
| Fedora,&nbsp;RHEL:   | **`sudo dnf install hledger`**
| Gentoo:              | **`sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`**
| Void Linux:          | **`xbps-install -S hledger hledger-ui hledger-web hledger-api`**
| NixOS:               | **`nix-env -iA nixpkgs.haskellPackages.hledger \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-ui \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-web`** (problems building hledger-ui on MacOS, [please help](https://github.com/simonmichael/hledger/issues/613))
| Sandstorm:           | **[hledger-web Sandstorm app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90) -> demo** (get a hledger-web server in 3 clicks. Features needed, [please help](https://github.com/simonmichael/hledger/issues/425))


<a name="b"></a>

## B. I want to build the latest release

The latest release (see [release notes](release-notes.html)) is a good choice.

On POSIX systems (mac/linux/freebsd..), use our new hassle-free installer
to get the current release of hledger and related tools.
Here's the more secure way:

 **`curl -sSLO http://hledger.org/hledger-install.sh`**  *# (or `wget -qO- ...`; or fetch it from [github](https://github.com/simonmichael/hledger/blob/master/hledger-install/hledger-install.sh))* \
 **`less hledger-install.sh`**                           *# (do security review)* \
 **`bash hledger-install.sh`**  *# (add `-v` for more detail; use `bash -x` to show commands being run)* 

and here's the less secure, more convenient way:

 **`curl -sSL http://hledger.org/hledger-install.sh | bash`**

Note this could take significant time (minutes to hours), memory (~2G),
and disk space (up to a gigabyte) depending on your connection, 
machine and past cabal/stack installations. You can kill and rerun it without losing progress.
If the installer fails, please help improve it by [reporting](docs.html#helpfeedback) the full output.

Or, you can install manually (all systems):

1. **Install [haskell `stack`](http://haskell-lang.org/get-started)**\
   \
   Avoid using versions older than 1.0, which give ["Invalid package ID" errors](https://github.com/simonmichael/hledger/issues/513);
   latest version is good.\
   On Windows, the 64-bit version is [recommended](https://github.com/simonmichael/hledger/issues/275#issuecomment-123834252).\
   On Arch, you [may need to also install GHC manually](https://github.com/simonmichael/hledger/issues/434).\
   Ensure [`~/.local/bin` (or Windows equivalent) is added to your \$PATH](https://docs.haskellstack.org/en/stable/install_and_upgrade/#path).
   Eg if you're a bash user:\
   &nbsp;&nbsp;`echo "export PATH=$PATH:~/.local/bin" >> ~/.bashrc && source ~/.bashrc`

2. **`stack install --install-ghc --resolver lts-8 hledger-lib-1.3 hledger-1.3 [hledger-ui-1.3] [hledger-web-1.3] [hledger-api-1.3]`**\   
   \
   This command installs the specified hledger packages (and required haskell libraries and tools) from [Stackage](https://www.stackage.org) (and if needed, [Hackage](http://hackage.haskell.org)).
   As noted above, it can take a while.
   You can add `--dry-run` to see what it plans to do, and kill/restart it without losing progress.\
   You can omit the bracketed packages to save time, and maybe install them later. If you include them, don't type the brackets.
   hledger-ui is [not yet available on Windows](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444).\
   If it gives "Invalid package ID" errors, get a newer version of stack.\
   If you're a [cabal](https://www.haskell.org/cabal/) expert, feel free to use that instead of stack.\
   If you get errors due to missing C libraries (like curses or terminfo), install those manually, eg:

    |
    |-----------------|-----------------------------------
    | Debian, Ubuntu: | `sudo apt install libncurses5-dev` 
    | Fedora, RHEL:   | `sudo dnf install ncurses-devel`

    If you need to build with an older GHC version for some reason, these commands should work
   (except on Mac Sierra which [requires at least GHC 8.0.2/lts-8](https://ghc.haskell.org/trac/ghc/ticket/12479)):\
   `stack install --resolver lts-7 hledger-lib-1.3 hledger-1.3 hledger-ui-1.3 hledger-web-1.3 hledger-api-1.3 brick-0.19 vty-5.15.1 data-clist-0.1.2.0`  *# (GHC 8.0.1)* \
   `stack install --resolver lts-6 hledger-lib-1.3 hledger-1.3 hledger-ui-1.3 hledger-web-1.3 hledger-api-1.3 megaparsec-5.3.1 brick-0.19 vty-5.15.1 data-clist-0.1.2.0 text-zipper-0.10`  *# (GHC 7.10.3)* \
   <!-- keep synced with stack.yaml files -->

Now you should be able to run `hledger --version` 
(and `hledger-ui --version`, `hledger-web --version` etc. if you installed those)
and see the version you just installed.


<a name="c"></a>

## C. I want to build the development version

The latest [master branch](https://github.com/simonmichael/hledger/commits/master) includes not-yet-released features and is stable enough for daily use.

1. **Install [`stack`](http://haskell-lang.org/get-started) and [git](https://en.wikipedia.org/wiki/Git)**
   (see notes in B above)
2. **`git clone http://code.hledger.org hledger`**
3. **`cd hledger`**
4. **`stack install`**

Cabal users can use the `cabal-install.sh` or `cabal.project` files instead.


<a name="d"></a>

## D. I want to install more commands

Additional [add-on commands](/hledger.html#third-party-add-ons)
can be installed. Eg:\
`stack install hledger-iadd-1.2.1` or `stack install --resolver nightly hledger-iadd`.

More, [experimental add-ons](/hledger.html#experimental-add-ons) are
included in the hledger source repo; to install these:

1. **Download the hledger source code** (as in C above)
2. **In the hledger directory, run `bin/compile.sh`** (installs dependencies & compiles for speed)
3. **Add the `hledger/bin/` directory to your `$PATH`** (as in B above) 

Now you should be able to run `hledger iadd --version`, `hledger check --help` etc.
