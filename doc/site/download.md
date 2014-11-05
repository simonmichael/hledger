# Download

\\
**I want to download and get started quickly !**\\
<!-- <sub>(If the download is out of date or doesn't run on my system, I might troubleshoot or donate to fund improvements)</sub> -->
| **I'm on Debian or Ubuntu**\\ `apt-get install hledger[-web]` \\ \\ **I'm on Gentoo**\\ `emerge hledger[-web]` <!-- \\ --> <!-- \\ --> <!-- **I'm on another GNU/Linux\\<small>(or can run Linux binaries)</small>**\\ --> <!-- <\\!-- [hledger.linux-32.zip]()\\ -\\-> --> <!-- <\\!-- [hledger-web.linux-32.zip]()\\ -\\-> --> <!-- <\\!-- [hledger.linux-64.zip]()\\ -\\-> --> <!-- <\\!-- [hledger-web.linux-64.zip]()\\ -\\-> --> <!-- Use cabal --> <!-- </td> --> \\ \\ **I'm on Red Hat/Fedora/CentOS**\\ `yum install hledger` \\ \\ **I'm on NixOS**\\ `nix-env -iA nixpkgs.haskellPackages.hledger` | **I'm on Windows**\\ <!-- [windows install guide](windows-install.html)\\ --> Download, unzip, and run:\\ [hledger-0.23.3.win32.zip](http://hledger.org/downloads/hledger-0.23.3-windows-intel32.exe.zip)\\ [hledger-web-0.23.3.win32.zip](http://hledger.org/downloads/hledger-web-0.23.3-windows-intel32.exe.zip)\\ | **I'm on Mac**\\ <!-- **I'm on Mac OSX 10.9+**\\ --> <!-- [mac install guide](mac-install.html)\\ --> <!-- [hledger.mac.zip]()\\ --> <!-- [hledger-web.mac.zip]()\\ --> Use cabal |

**I want to build the 
[latest release](http://hackage.haskell.org/package/hledger-web) with
[GHC](http://haskell.org/haskell) and
[cabal](http://haskell.org/cabal/download.html)...**
| `cabal sandbox init; cabal update; cabal install hledger[-web]`\\ |
<!-- [cabal install guide](cabal-install.html) -->

**I want to build the [latest development code](http://hledger.org/code)...**
| `git clone https://github.com/simonmichael/hledger; cd hledger; make sandbox install`\\ |

**I want more info...**

The [[installing|Installation Guide]] describes how to install using cabal in more detail.

hledger is shipped as two executables: `hledger` (the command-line
tool) and `hledger-web` (the web interface).  If you install
`hledger-web`, `hledger` will also be installed automatically (except on Windows).

Building, testing and supporting cross-platform binaries is costly, so
it's demand-driven - you can indicate demand by making a project
donation of any size. Binaries funded in this way will be linked here.
This is a quick way to help the project and your fellow users!

[[https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5J33NLXYXCYAY|{{https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif }}]]
