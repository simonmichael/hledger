# Download

hledger is shipped as two executables: `hledger` (the command-line tool) and `hledger-web` (the web interface).
Generally (except on Windows), installing `hledger-web` automatically installs `hledger` as well.

[Release Notes](release-notes.html)

## I want to download and run
<!-- <sub>(If the download is out of date or doesn't run on my system, I might troubleshoot or donate to fund improvements)</sub> -->

<table>
<tr valign="top">
<td width="33%">
**I'm on Debian or Ubuntu**\
`apt-get install hledger[-web]`

**I'm on Gentoo**\
`emerge hledger[-web]`

**I'm on Red Hat/Fedora/CentOS**\
`yum install hledger`

**I'm on NixOS**\
`nix-env -iA nixpkgs.haskellPackages.hledger`

<!--
**I'm on another GNU/Linux\<small>(or can run Linux binaries)</small>**
[hledger.linux-32.zip]()
[hledger-web.linux-32.zip]()
[hledger.linux-64.zip]()
[hledger-web.linux-64.zip]()
Use cabal
-->

</td>
<td width="33%">

**I'm on Windows**\
<!-- [windows install guide](windows-install.html)\ -->
Download, unzip, and run:\
[hledger-0.23.3.win32.zip](http://hledger.org/downloads/hledger-0.23.3-windows-intel32.exe.zip)\
[hledger-web-0.23.3.win32.zip](http://hledger.org/downloads/hledger-web-0.23.3-windows-intel32.exe.zip)

</td>
<td width="33%">

**I'm on Mac**\
<!-- [mac install guide](mac-install.html)\ -->
<!-- [hledger.mac.zip]()\ -->
<!-- [hledger-web.mac.zip]()\ -->
Use cabal

</td>
</tr>
</table>

<div style="margin-left:1em; float:right;">
**[Gittip](https://www.gittip.com/simonmichael/)**,
<a href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5J33NLXYXCYAY"><img border=0 src="https://www.paypal.com/en_US/i/btn/x-click-but04.gif" alt="paypal"></a>
</div>
Building, testing and supporting Windows and Mac binaries is costly, so
it's demand-driven - you can indicate demand by making a project
donation of any size. Binaries funded in this way will be linked here.
This is a quick way to help the project and your fellow users!

## I want to build the [latest release](http://hackage.haskell.org/package/hledger-web) with [GHC](http://haskell.org/haskell) and [cabal](http://haskell.org/cabal/download.html)

`cabal sandbox init; cabal update; cabal install hledger[-web]`\
<!-- [cabal install guide](cabal-install.html) -->

The [Installation Guide](installing.html) describes how to install using cabal in more detail.

## I want to build the [latest development code](http://hledger.org/code)

`git clone https://github.com/simonmichael/hledger; cd hledger; make sandbox install`

