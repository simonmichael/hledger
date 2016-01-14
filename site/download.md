<div style="float:right; text-align:right; white-space:nowrap; ">
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&amp;hosted_button_id=5J33NLXYXCYAY"><img border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" alt="paypal"></a> 
<a style="margin-left:3px;" href="https://flattr.com/submit/auto?user_id=simonmichael&amp;url=http%3A%2F%2Fhledger.org" target="_blank"><img src="//api.flattr.com/button/flattr-badge-large.png" alt="Flattr this" title="Flattr this" border="0"></a> 
<div style="display:inline-block; position:relative; top:5px;">
<script data-gratipay-username="simonmichael" data-gratipay-widget="button" src="//grtp.co/v1.js"></script> 
</div>
</div>
# Download

**[Release notes](release-notes.html)**

<a name="packaged"></a>

## I want to download a packaged version
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
<br><em>(The current release is 0.27. To contribute binaries for windows or mac, please <a href="mailto:simon@joyful.com">get in touch</a>.)</em>
</td></tr>

<tr><td>
**on Mac**
</td><td>
Build with stack or cabal as described below.
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
`nix-env -iA nixpkgs.haskellPackages.hledger nixpkgs.haskellPackages.hledger-web`
</td></tr>

<tr><td>
**in a VM**
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

<a name="released"></a>

## I want to build the latest release (0.27)

**with stack** (most reliable, recommended)

1. Install [`stack`](http://haskellstack.org)
    (On Windows, the 64-bit version is [recommended](https://github.com/simonmichael/hledger/issues/275).)
    <!-- needed if you will be processing >50,000 transactions at once -->
2. `stack setup`
    (if you need GHC installed. If you're not sure, run the next command and it will tell you.)
3. `stack --resolver lts-4 install hledger [hledger-ui] [hledger-web]`
4. Ensure `~/.local/bin` or the Windows equivalent is in your `$PATH`,
   so that you can just type `hledger` to run it.
   (stack will show the proper directory and will tell you if it is not in $PATH).

Eg, on a mac:
<script type="text/javascript" src="https://asciinema.org/a/29672.js" id="asciicast-29672" async></script>

**with cabal** (results will vary, recommended only if you can't install stack):

1. Install [GHC](http://haskell.org/ghc) and [cabal](http://haskell.org/cabal/download.html) if needed.
2. Ensure `~/.cabal/bin` or the Windows equivalent is in your `$PATH`.
3. `cabal update`
4. `cabal install alex happy`
5. `mkdir hledger-sandbox`
6. `cd hledger-sandbox`
7. `cabal sandbox init`
8. `cabal install hledger[-ui|-web]` (On Windows, hledger-ui is [not yet supported](https://github.com/coreyoconnor/vty/pull/1).)
9. Ensure this `.../hledger-sandbox/.cabal-sandbox/bin` is in your `$PATH` (or move its contents to ~/.cabal/bin).

<a name="unreleased"></a>

## I want to build the [unreleased git version](https://github.com/simonmichael/hledger/commits/master)

1. `git clone http://code.hledger.org hledger`
2. `cd hledger`
3. `stack install`

or see the [Developer Guide](http://hledger.org/developer-guide.html) for more help.

