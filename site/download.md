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

Packaged versions may lag behind the latest release, but are the quickest to install.

|
|---------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| **Windows:**        | Download and run the [latest development builds](developer-guide.html) (-> Appveyor CI)
| **Mac:**            | `brew install hledger`
| **Debian,&nbsp;Ubuntu:** | `sudo apt install hledger hledger-ui hledger-web`
| **Fedora,&nbsp;RHEL:**   | `sudo dnf install hledger`
| **Gentoo:**         | `sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`
| **NixOS:**          | `nix-env -iA nixpkgs.haskellPackages.hledger \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-ui \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-web`

Only some of the hledger tools may be available, eg Homebrew does not yet include 
hledger-ui, hledger-web or hledger-api (packagers welcome!).

<a name="b"></a>

## B. I want to build the latest release

The latest release (see [release notes](release-notes.html)) is a good choice. 
You have to build it, but that's pretty easy. 

1. Install [`stack`](http://haskell-lang.org/get-started).
   On Windows, the 64-bit version is [recommended](https://github.com/simonmichael/hledger/issues/275#issuecomment-123834252).

2. `stack install --resolver=nightly hledger [hledger-ui] [hledger-web] [hledger-api]`\
   On Windows, hledger-ui is [not yet available](https://github.com/coreyoconnor/vty/pull/1).\
   If stack says you need to run `stack setup`, do that first.\
   Warnings about "No packages found in snapshot which provide a "hsc2hs" executable" can be ignored.\
   If required C libraries (like curses) are not installed, you might need to install those manually and try again, eg:

    |
    |--------------------|-----------------------------------
    | **Debian, Ubuntu:** | `sudo apt install libncurses5-dev` 
    | **Fedora, RHEL:**   | `sudo dnf install ncurses-devel`

3. If stack warns that `~/.local/bin` (or the Windows equivalent) is not in `$PATH`, fix that.
   Eg if you're a bash user:\
   `echo "export PATH=$PATH:~/.local/bin" >> ~/.bashrc && source ~/.bashrc`\
   Now you can run `hledger --version`, `hledger-ui --version` etc.

Installing with [`cabal`](https://www.haskell.org/cabal/) is similar, if you're familiar with that. 
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

## C. I want to build the [development version](https://github.com/simonmichael/hledger/commits/master)

The dev version includes not-yet-released features and is stable enough for daily use.

1. Install [`stack`](http://haskell-lang.org/get-started) and [git](https://en.wikipedia.org/wiki/Git)
2. `git clone http://code.hledger.org hledger`
3. `cd hledger`
4. `stack install`\
   with the same caveats as mentioned above:
    
    - if stack says you need to run `stack setup`, do that first,
    - if building fails due to missing C libs, install those first 
    - if stack warns that `~/.local/bin` (or the Windows equivalent) is not in `$PATH`, fix that
    - warnings about "No packages found in snapshot which provide a hsc2hs executable" can be ignored

Cabal users can use the `cabal-install.sh` or `cabal.project` files instead.


<!--
VM instructions:

- if stack or cabal can't run on your OS, maybe this [vagrant image](https://github.com/sciurus/hledger-vagrant) can ?
-->
