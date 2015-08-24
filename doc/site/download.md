# Download

<div style="float:right; text-align:right; white-space:nowrap; ">
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5J33NLXYXCYAY"><img border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" alt="paypal"></a> 
<a style="margin-left:3px;" href="https://flattr.com/submit/auto?user_id=simonmichael&url=http%3A%2F%2Fhledger.org" target="_blank"><img src="//api.flattr.com/button/flattr-badge-large.png" alt="Flattr this" title="Flattr this" border="0"></a> 
<div style="display:inline-block; position:relative; top:5px;">
<script data-gratipay-username="simonmichael" data-gratipay-widget="button" src="//grtp.co/v1.js"></script> 
</div>
</div>
## I want to download and run
<!-- <sub>(If the download is out of date or doesn't run on my system, I might troubleshoot or donate to fund improvements)</sub> -->

<style>
tr { vertical-align:top; }
td { padding-bottom:1em; padding-right:1em; }
</style>

<table>

<tr><td>
**on Windows**
</td><td>
<!-- [windows install guide](windows-install.html)\ -->
Download, unzip, and run
[hledger-0.26-win64.zip](http://hledger.org/downloads/hledger-0.26-win64.zip)
<!-- (or the [32-bit build](http://hledger.org/downloads/hledger-0.26-win32.zip)) -->
and/or
[hledger-web-0.26-win64.zip](http://hledger.org/downloads/hledger-web-0.26-win64.zip)
</td></tr>

<tr><td>
**on Mac**
</td><td>
<!-- [mac install guide](mac-install.html)\ -->
<!-- [hledger.mac.zip]()\ -->
<!-- [hledger-web.mac.zip]()\ -->
Use stack or cabal (see below)
</td></tr>

<tr><td>
**on Debian or Ubuntu**
</td><td>
`apt-get install hledger hledger-web`
</td></tr>

<tr><td>
**on Gentoo**
</td><td>
`emerge hledger hledger-web`
</td></tr>

<tr><td>
**on Fedora**
</td><td>
`yum install hledger`
</td></tr>

<tr><td>
**on NixOS**
</td><td style="white-space:nowrap;">
`nix-env -iA nixpkgs.haskellPackages.hledger nixpkgs.haskellPackages.hledgerWeb`
</td></tr>

<tr><td>
**in a Virtualbox VM**
</td><td>
Try <https://github.com/sciurus/hledger-vagrant>
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

**[Release notes](release-notes.html)**

## I want to build the latest release

**Using the stack tool** (easiest, recommended):

1. Install [`stack`](https://github.com/commercialhaskell/stack/wiki/Downloads)
    (on Windows, you should choose the 64-bit stack download if you will be
processing >50,000 transactions at a time with hledger, cf [#275](https://github.com/simonmichael/hledger/issues/275)).

2. `stack setup`
    (if you need GHC installed. If you're not sure, run the next command and it will tell you.)

3. `stack install hledger` (the command-line UI), or\
   `stack install hledger-ui` (the terminal UI; not available on Windows; includes the above), or\
   `stack install hledger-web` (the web UI; includes the above)

4. stack will report where it installed the binaries (`~/.local/bin` or the Windows equivalent).
   You should ensure this directory is in your `$PATH` (stack will let you know),
   so that you can just type `hledger` to run it.

**Without stack,** the process is much more variable; this is most likely to work:

1. Install [GHC](http://haskell.org/ghc) and [cabal](http://haskell.org/cabal/download.html) if needed
2. `cabal update`
3. `cabal install alex happy`
4. `cabal sandbox init`
5. `cabal install hledger[-ui|-web]`
6. Ensure `~/.cabal/bin` or the Windows equivalent is in your `$PATH`

## I want to build the latest [master branch](https://github.com/simonmichael/hledger/commits/master)

See the [Developer Guide](http://hledger.org/developer-guide.html), or just:

1. `git clone http://code.hledger.org hledger`
2. `cd hledger`
3. `stack install`

