<div style="float:right; text-align:right; white-space:nowrap; ">
<a style="margin-left:3px;" href="https://flattr.com/submit/auto?user_id=simonmichael&amp;url=http%3A%2F%2Fhledger.org" target="_blank"><img src="//api.flattr.com/button/flattr-badge-large.png" alt="" title="Flattr this" border="0"></a> 
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&amp;hosted_button_id=5J33NLXYXCYAY"><img width=62 height=31 border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" alt=""></a> 
<div style="display:inline-block; position:relative; top:5px; width:62px; height:31px;">
<script data-gratipay-username="simonmichael" data-gratipay-widget="button" src="//grtp.co/v1.js"></script> 
</div>
<a href="https://www.bountysource.com/trackers/536505-simonmichael-hledger"><img border=0 src="https://www.bountysource.com/badge/tracker?tracker_id=536505" alt=""></a> &nbsp;
</div>
# Download
<a name="packaged"></a>

## A. I want to download a packaged version
<!-- <sub>(If the download is out of date or doesn't run on my system, I might troubleshoot or donate to fund improvements)</sub> -->

<style>
tr { vertical-align:top; }
td { padding-bottom:.5em; padding-right:1em; }
td:first-of-type { 
  /* white-space:nowrap; */
  /* width:1%; */
}
a { white-space:nowrap; }
</style>

|
|--------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| **Windows**        | Download and run [hledger-0.26-win64.exe](http://hledger.org/downloads/hledger-0.26-win64.exe) <!-- (or the [32-bit build](http://hledger.org/downloads/hledger-0.26-win32.exe)) --> & [hledger-web-0.26-win64.exe](http://hledger.org/downloads/hledger-web-0.26-win64.exe) (old), or the [latest development builds](developer-guide.html). hledger-ui does not run on Windows.
| **Mac**            | `brew install hledger`<br>hledger-ui/hledger-web have not yet been added to homebrew.
| **Debian, Ubuntu** | `sudo apt install hledger hledger-ui hledger-web`
| **Fedora, RHEL**   | `sudo dnf install hledger`<br>hledger-ui/hledger-web have not yet been added to Fedora.
| **Gentoo**         | `sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`
| **NixOS**          | `nix-env -iA nixpkgs.haskellPackages.hledger \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-ui \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-web`

<!--
**on another GNU/Linux\<small>(or can run Linux binaries)</small>**
[hledger.linux-32.zip]()
[hledger-web.linux-32.zip]()
[hledger.linux-64.zip]()
[hledger-web.linux-64.zip]()
Use cabal
-->

<!--
Building and supporting Windows and Mac binaries is costly, so
it's demand-driven - you can indicate demand by making a project
donation of any size. Binaries funded in this way will be linked here.
This is a quick way to help the project and your fellow users!
-->

<a name="released"></a>
## B. Build/install a version
Using the [`stack`](https://docs.haskellstack.org/en/stable/README/) tool favored by hledger. Or use [`cabal`](https://www.haskell.org/cabal/) if you prefer and know how. 

1. Install [`stack`](http://haskellstack.org) to make building easier.
   On Windows, the 64-bit version is [recommended](https://github.com/simonmichael/hledger/issues/275#issuecomment-123834252).

If stack warns that ~/.local/bin or the Windows equivalent is not in your $PATH, configure that, so that commands like hledger will work. Eg if you're a bash user:

    echo "export PATH=$PATH:~/.local/bin" >> ~/.bashrc && source ~/.bashrc

Haskell builds can fail due to missing C libraries or headers, which stack/cabal can not install.
If you have this problem, here are some C libs you might need (please send updates):

|
|--------------------|-----------------------------------
| **Debian, Ubuntu** | `sudo apt install ... ?` 
| **Fedora, RHEL**   | `sudo dnf install ncurses-devel`

### B.1. I want to build the latest released version

The latest release may be newer than your OS's packaged version. 
Here are the latest [release notes](release-notes.html).

2. `stack setup 7.10.3`\
   to ensure you have a suitable version of [GHC](https://www.haskell.org/ghc).

3. `stack install hledger [hledger-ui] [hledger-web]`\
   hledger-ui and hledger-web take longer to build. hledger-ui is not buildable on Windows.

## B.2. I want to build the [latest development version](https://github.com/simonmichael/hledger/commits/master)

This includes the latest features and is normally stable enough for daily use (it's what I use).
<!-- See also the [Developer Guide](http://hledger.org/developer-guide.html). -->

1.a. Install [git](https://en.wikipedia.org/wiki/Git)
2. `git clone http://code.hledger.org hledger`
3. `cd hledger`
4. `stack setup`
5. `stack install [hledger] [hledger-ui] [hledger-web] [hledger-api]`

cabal users, try `./cabal-install.sh` instead.

<a name="unreleased"></a>

<!--
Detailed cabal instructions:
1. Install [GHC](http://haskell.org/ghc) and [cabal](http://haskell.org/cabal/download.html) if needed.
2. Ensure `~/.cabal/bin` or the Windows equivalent is in your `$PATH`.
3. `cabal update`
4. `cabal install alex happy`
5. `mkdir hledger-sandbox`
6. `cd hledger-sandbox`
7. `cabal sandbox init`
8. `cabal install hledger[-ui|-web]` (On Windows, hledger-ui is [not yet supported](https://github.com/coreyoconnor/vty/pull/1).)
9. Ensure this `.../hledger-sandbox/.cabal-sandbox/bin` is in your `$PATH` (or move its contents to ~/.cabal/bin).
-->
<!--
**in a VM**

- if stack or cabal can't run on your OS, maybe this [vagrant image](https://github.com/sciurus/hledger-vagrant) can ?
-->
