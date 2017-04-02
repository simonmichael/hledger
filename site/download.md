<div style="float:right; text-align:right; white-space:nowrap; ">
<a style="margin-left:3px;" href="https://flattr.com/submit/auto?user_id=simonmichael&amp;url=http%3A%2F%2Fhledger.org" target="_blank"><img src="//api.flattr.com/button/flattr-badge-large.png" alt="" title="Flattr this" border="0"></a> 
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&amp;hosted_button_id=5J33NLXYXCYAY"><img width=62 height=31 border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" alt=""></a> 
<div style="display:inline-block; position:relative; top:5px; width:62px; height:31px;">
<script data-gratipay-username="simonmichael" data-gratipay-widget="button" src="//grtp.co/v1.js"></script> 
</div>
<a href="https://www.bountysource.com/trackers/536505-simonmichael-hledger"><img border=0 src="https://www.bountysource.com/badge/tracker?tracker_id=536505" alt=""></a> &nbsp;
</div>
# Download
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

Packaged versions are the quickest to install, but they sometimes lag behind the
latest release, or provide only some of the hledger tools. (Packagers welcome!)

|
|----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| Windows:             | see B below <!-- Download and run the [latest development builds](developer-guide.html) (-> Appveyor CI) -->
| Mac:                 | **`brew install hledger`**
| Debian,&nbsp;Ubuntu: | **`sudo apt install hledger hledger-ui hledger-web`**
| Fedora,&nbsp;RHEL:   | **`sudo dnf install hledger`**
| Gentoo:              | **`sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`**
| NixOS:               | **`nix-env -iA nixpkgs.haskellPackages.hledger \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-ui \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-web`**
| Sandstorm:           | **[hledger-web Sandstorm app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90) -> demo**<br>(get your own private or public hledger-web instance in 3 clicks)


<a name="b"></a>

## B. I want to build the latest release

The latest release (see [release notes](release-notes.html)) is a good choice.
You have to build it, but normally that should be quite easy:

1. **Install [`stack`](http://haskell-lang.org/get-started)**\
   Note some packaged versions of stack are too old and will give ["Invalid package ID"](https://github.com/simonmichael/hledger/issues/513) in step 2
   (and their builtin "upgrade" command will be slow); in this case consider downloading the binary instead.\
   On Windows, the 64-bit version is [recommended](https://github.com/simonmichael/hledger/issues/275#issuecomment-123834252).\
   On Arch, you [may need to also install GHC manually](https://github.com/simonmichael/hledger/issues/434).\
   You should also add [~/.local/bin (or Windows equivalent)](https://docs.haskellstack.org/en/stable/install_and_upgrade/#path)
   to your \$PATH so that you can easily run stack-installed tools.
   Eg if you're a bash user:\
   `echo "export PATH=$PATH:~/.local/bin" >> ~/.bashrc && source ~/.bashrc`

2. **`stack install hledger-lib-1.2 hledger-1.2 [hledger-ui-1.2] [hledger-web-1.2] [hledger-api-1.2]`**\   
   Note specifying the version as above forces stack to select exactly this version, 
   which is sometimes necessary just after a release;
   at other times the following command may be preferable 
   [to](https://www.stackage.org/package/hledger-lib)
   [get](https://www.stackage.org/package/hledger)
   [any](https://www.stackage.org/package/hledger-ui)
   [newer](https://www.stackage.org/package/hledger-web)
   [versions](https://www.stackage.org/package/hledger-api):\
   `stack install --resolver=nightly hledger [hledger-ui] [hledger-web] [hledger-api]`\
   If stack says you need to run `stack setup`, do that first.\
   Don't type the square brackets; they mean that hledger-ui etc. are optional.\
   On Windows, hledger-ui is [not yet available](https://github.com/coreyoconnor/vty/pull/1).\
   If required C libraries (like curses or terminfo) are not installed, you might need to install those manually and try again.
   Eg:

    |
    |-----------------|-----------------------------------
    | Debian, Ubuntu: | `sudo apt install libncurses5-dev` 
    | Fedora, RHEL:   | `sudo dnf install ncurses-devel`
   
If you're a [`cabal`](https://www.haskell.org/cabal/) expert, feel free to use that instead of stack, adapting these instructions appropriately.

Now you should be able to run `hledger --version`, `hledger-ui --version` etc.


<!--(The exact steps depend on your OS, cabal version and expertise.)-->
<!--
Short version:\
`cabal update && cabal install hledger [hledger-ui] [hledger-web] [hledger-api]`
-->
<!--
If you're brand new to cabal, these steps should work on unix-like systems 
(on Windows, adjust commands and paths as needed):

1. Install [GHC](http://haskell.org/ghc) and [cabal](http://haskell.org/cabal/download.html) if needed,
   eg from [https://www.haskell.org/downloads](https://www.haskell.org/downloads)
2. Ensure `~/.cabal/bin` or the Windows equivalent is in your `$PATH`,
   eg `echo "export PATH=$PATH:~/.cabal/bin" >> ~/.bashrc && source ~/.bashrc`
3. `cabal update`
4. `cabal install alex happy`
5. `mkdir hledger-sandbox`
6. `cd hledger-sandbox`
7. `cabal sandbox init`
8. `cabal install hledger-1.0.1 [hledger-ui-1.0.2] [hledger-web-1.0.1] [hledger-api-1.0]`
9. `mv .cabal-sandbox/bin/hledger* ~/.cabal/bin`
10. `cd ..; rm -rf hledger-sandbox`
-->


<a name="c"></a>

## C. I want to build the development version

The [dev version](https://github.com/simonmichael/hledger/commits/master) includes not-yet-released features and is stable enough for daily use.

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
