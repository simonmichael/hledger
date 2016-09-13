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

## I want to download a packaged version
<!-- <sub>(If the download is out of date or doesn't run on my system, I might troubleshoot or donate to fund improvements)</sub> -->

<style>
tr { vertical-align:top; }
td { padding-bottom:.5em; padding-right:1em; }
a { white-space:nowrap; }
</style>

<table>

<tr><td>
**on Windows**
</td><td>
[hledger-0.26-win64.exe](http://hledger.org/downloads/hledger-0.26-win64.exe)
<!-- (or the [32-bit build](http://hledger.org/downloads/hledger-0.26-win32.exe)) -->
and/or
[hledger-web-0.26-win64.exe](http://hledger.org/downloads/hledger-web-0.26-win64.exe)
<em>(not the latest release. You can get the latest development builds from Appveyor CI [here](developer-guide.html). Note hledger-ui is not supported on Windows.)</em>
</td></tr>

<tr><td>
**on Mac**
</td><td>
`brew install hledger`
</td></tr>

<tr><td style="white-space:nowrap;">
**on Debian or Ubuntu**
</td><td>
`sudo apt-get install hledger hledger-web`
</td></tr>

<tr><td>
**on Fedora or RHEL**
</td><td>
`sudo dnf install hledger`
</td></tr>

<tr><td>
**on Gentoo**
</td><td>
`sudo (layman -a haskell && emerge hledger hledger-web)`
</td></tr>

<tr><td>
**on NixOS**
</td><td>
`nix-env -iA nixpkgs.haskellPackages.hledger nixpkgs.haskellPackages.hledger-web`
</td></tr>

</table>

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

## I want to build the latest released version

The latest release may be newer than your OS's packaged version. 
(Here are the latest [release notes](release-notes.html).)

1. Install [`stack`](http://haskellstack.org).
   On Windows, the 64-bit version is [recommended](https://github.com/simonmichael/hledger/issues/275).

2. `stack setup` to ensure you have the required version of [GHC](https://www.haskell.org/ghc).

3. `stack install hledger [hledger-ui] [hledger-web]`\
   hledger-ui and hledger-web take longer to build. hledger-ui is not buildable on Windows.

4. If stack warns that `~/.local/bin` or the Windows equivalent is not in your `$PATH`,
   configure that, so that the command `hledger` will work.
   Eg if you're a bash user:\
   `echo "export PATH=$PATH:~/.local/bin" >> ~/.bashrc && source ~/.bashrc`

Or use [`cabal`](https://www.haskell.org/cabal/) if you prefer and know how. 
It's harder to use and to support, so these docs focus on stack.

**Note:** Haskell builds can fail due to missing C libraries or headers, which stack/cabal can not install.
If you have this problem, here are some C libs you might need (updates welcome):

|
|-------------------------|-----------------------------------
| **on Debian or Ubuntu** | 
| **on Fedora or RHEL**   | `sudo dnf install ncurses-devel`


<a name="unreleased"></a>

## I want to build the [latest development version](https://github.com/simonmichael/hledger/commits/master)

This includes the latest features and is normally stable enough for daily use; recommended.
<!-- See also the [Developer Guide](http://hledger.org/developer-guide.html). -->

1. `git clone http://code.hledger.org hledger`
2. `cd hledger`
3. `stack install [hledger] [hledger-ui] [hledger-web] [hledger-api]`

cabal users, try `./cabal-install.sh` instead.

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
