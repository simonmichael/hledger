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
and
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

## I want to build the latest release

**[Release notes](release-notes.html)**

Using stack (easiest, recommended):

1. Download and install [stack](https://github.com/commercialhaskell/stack/wiki/Downloads)

    (on windows, choose the 64-bit version if you will be processing [>50k transactions](https://github.com/simonmichael/hledger/issues/275))

2. `stack --resolver nightly-2015-07-13 setup`

    (do this if you need GHC, eg on windows. If you're not sure, run the next command and it will tell you)

3. `stack --resolver nightly-2015-07-13 install hledger`

    (on windows, stack [can't](https://github.com/commercialhaskell/stack/issues/661) install the latest hledger-web yet)

Using cabal:

1. Install [GHC](http://haskell.org/ghc) and [cabal](http://haskell.org/cabal/download.html)
2. `cabal update`
3. `cabal install alex happy`    *(if these are not already in your PATH)*
4. `cabal sandbox init`
5. `cabal install hledger[-web]`

## I want to build the latest [master branch](https://github.com/simonmichael/hledger/commits/master)

1. `git clone https://github.com/simonmichael/hledger.git` (shortcut: `git clone code.hledger.org hledger`)
2. `cd hledger`
3. `stack install` (or `cabal sandbox init; cabal install ./hledger{-lib,,-web}`)

\
\
See also the old [Installation Guide](installing.html).

