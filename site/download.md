# Download

Do you want to..
<a name="a"></a>

## A. download a binary/system package ?

<style>
tr { vertical-align:top; }
td { padding-bottom:.5em; padding-right:1em; }
td:first-of-type { 
  /* white-space:nowrap; */
  /* width:1%; */
}
a { white-space:nowrap; }
.warnings {
    font-style:italic;
}
.warnings > a:before {
    content: "⚠ ";
    color:red;
}
</style>

System packages are quickest to install, 
but they can be [out of date](https://repology.org/metapackage/hledger/badges) or incomplete. 


|
|----------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| Windows:             | [Latest developer builds](https://ci.appveyor.com/project/simonmichael/hledger/build/artifacts) <span class=warnings>([appveyor builds are not up to date](https://github.com/simonmichael/hledger/issues/694), [no hledger-ui](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444))</span>
| Mac:                 | **`brew install hledger`** <span class=warnings>([only hledger CLI is packaged](https://github.com/simonmichael/hledger/issues/321#issuecomment-179920520))</span>
| Arch Linux:          | **`pacman -S hledger`** <span class=warnings>([haskell problems are common on Arch at present](https://github.com/simonmichael/hledger/issues/668))</span>
| Debian,&nbsp;Ubuntu: | **`sudo apt install hledger hledger-ui hledger-web`**
| Fedora,&nbsp;RHEL:   | **`sudo dnf install hledger`**
| Gentoo:              | **`sudo layman -a haskell && sudo emerge hledger hledger-ui hledger-web`**
| Void Linux:          | **`xbps-install -S hledger hledger-ui hledger-web hledger-api`**
| NixOS:               | **`nix-env -iA nixpkgs.haskellPackages.hledger \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-ui \`<br>&nbsp;&nbsp;`nixpkgs.haskellPackages.hledger-web`** <span class=warnings>([problems building hledger-ui on MacOS](https://github.com/simonmichael/hledger/issues/613))</span>
| Sandstorm:           | **[hledger-web Sandstorm app](https://apps.sandstorm.io/app/8x12h6p0x0nrzk73hfq6zh2jxtgyzzcty7qsatkg7jfg2mzw5n90) -> demo** - a hledger-web server in 3 clicks. <span class=warnings>([features needed](https://github.com/simonmichael/hledger/issues/425))</span>


<a name="b"></a>

## B. build the latest release ?

Good choice! The [release notes](release-notes.html) show what you'll get.
Below are three ways to build hledger, in order of preference.

But first, a slight warning: the first build of a haskell application can take
significant time (minutes to 1 hour), memory (eg 1G+), and disk space
(eg 1G in ~/.stack or ~/.cabal).  On the upside, it can be left
unattended, you can kill and restart it without losing progress, and
subsequent builds will be quicker.

<span class=warnings>
Some known build issues and workarounds:\
[freebsd 12 no cabal file found](https://github.com/simonmichael/hledger/issues/709)\
[openbsd 6 exec permission denied](https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html)\
[openbsd how to get stack](https://github.com/commercialhaskell/stack/issues/2822#issuecomment-318892816)\
</span>

### 1. hledger-install

On supported POSIX systems - GNU/linux, mac, freeBSD (might work on other BSDs if you install cabal-install first), possibly unixlike environments on windows - 
our [hledger-install.sh](https://github.com/simonmichael/hledger/tree/master/hledger-install)
is the build method most likely to just work:

- it requires only that you have bash and curl (or wget) installed, and internet access.
- it uses haskell build tools like stack, cabal and GHC, installing stack/GHC if needed (in ~/.stack).
- it avoids common pitfalls, such as unreliable build plans and all-or-nothing builds
- it installs the latest release of hledger and the full suite of addon tools
  (in ~/.local/bin or ~/.cabal/bin).

Here's the quick, non-secure way to run it:

 **`curl https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh | bash`**

And here's the safer, more responsible way:

 **`curl -O https://raw.githubusercontent.com/simonmichael/hledger/master/hledger-install/hledger-install.sh`**\
 **`less hledger-install.sh`**  *# do security review*\
 **`bash hledger-install.sh`**  *# or bash -x, to log commands*

If you see link errors, you might need to manually install some extra C libraries and try again.
See "Install required C libraries" below.

If you have any other trouble, you can help greatly by capturing a debug log
and sending it to me via 
[paste](http://paste.hledger.org) & [IRC](http://irc.hledger.org),
an [issue](http://bugs.hledger.org),
or [email](docs.html#helpfeedback):

 **`bash -x hledger-install.sh 2>&1 | tee hledger-install.log`**

### 2. stack

[`stack`](http://haskell-lang.org/get-started) is the newer and easier of the two Haskell build tools.
Here's how to use it directly to install hledger:

1. **Install or upgrade to the latest stack**\
   The latest version of stack (1.6.3+) is recommended, as it is the best at avoiding ecosystem breakages and most likely to just work.
   If you can get at least stack 1.3 installed, eg from your system packages, you can usually run `stack upgrade` to quickly upgrade it to the latest.
   On Windows, the 64-bit version of stack is [recommended](https://github.com/simonmichael/hledger/issues/275#issuecomment-123834252).
   <!-- On Arch, you [may need to also install GHC manually](https://github.com/simonmichael/hledger/issues/434).\ -->
   Follow stack's advice to 
   [add `~/.local/bin` to your \$PATH](https://docs.haskellstack.org/en/stable/install_and_upgrade/#path) (`%APPDATA%\local\bin` on Windows).
   Eg, if you're a bash user:\
   &nbsp;&nbsp;`echo "export PATH=$PATH:~/.local/bin" >> ~/.bashrc && source ~/.bashrc`

2. **Install required C libraries**\
   You might need to manually install some extra C libraries, such as the below
   (please send updates for this list).
   If you're not sure about this, proceed with the next step and return here if you get link errors
   (eg: "/bin/ld.gold: error: cannot find -ltinfo").

    |
    |-----------------|-------------------------------------------------------
    | Centos:         | `sudo yum install -y libstdc++-devel ncurses-devel zlib-devel` *[?](https://github.com/simonmichael/hledger/issues/715)*
    | Debian, Ubuntu: | `sudo apt install -y libncurses5` *?*
    | Fedora, RHEL:   | `sudo dnf install -y ncurses-devel` *?*


3. **`stack install --resolver=nightly hledger-lib-1.5 hledger-1.5 [hledger-ui-1.5] [hledger-web-1.5] [hledger-api-1.5]`**\   
   This installs the specified hledger packages (and required haskell libraries and tools) from [Stackage](https://www.stackage.org) (and if needed, [Hackage](http://hackage.haskell.org)).
   Specifying stackage's nightly resolver (snapshot) requires the most rebuilding, but is the most reliable;
   you can try reducing build time by specifying another resolver that you've installed from previously, or no --resolver option.\
   Add `--dry-run` to see the build plan. You can kill and restart this without losing progress.

    The bracketed packages are optional; if you include them, don't type the brackets, and do always 
    include the preceding hledger-lib and hledger packages in the command, otherwise stack may complain.
    <span class=warnings>([hledger-ui is not yet available on Windows, alas](https://github.com/jtdaugherty/vty/pull/1#issuecomment-297143444).)</span>

<!--
    If you need to build with an older GHC version for some reason, these commands should work
   (except on Mac Sierra which [requires at least GHC 8.0.2/lts-8](https://ghc.haskell.org/trac/ghc/ticket/12479)):\
   `stack install --resolver lts-7 hledger-lib-1.3 hledger-1.3 hledger-ui-1.3 hledger-web-1.3 hledger-api-1.3 brick-0.19 vty-5.15.1 data-clist-0.1.2.0`  *# (GHC 8.0.1)* \
   `stack install --resolver lts-6 hledger-lib-1.3 hledger-1.3 hledger-ui-1.3 hledger-web-1.3 hledger-api-1.3 megaparsec-5.3.1 brick-0.19 vty-5.15.1 data-clist-0.1.2.0 text-zipper-0.10`  *# (GHC 7.10.3)* \
--> <!-- keep synced with stack.yaml files -->

Now you should be able to run `hledger --version` 
(and `hledger-ui --version`, `hledger-web --version` etc. if installed)
and see the latest version number.
Additional [add-on commands](/hledger.html#third-party-add-ons),
such as
[hledger-diff](http://hackage.haskell.org/package/hledger-diff),
[hledger-iadd](http://hackage.haskell.org/package/hledger-iadd),
[hledger-interest](http://hackage.haskell.org/package/hledger-interest),
and [hledger-irr](http://hackage.haskell.org/package/hledger-irr)
can be installed similarly to the above. Eg:\
&nbsp;&nbsp;`stack install --resolver nightly hledger-lib-1.5 hledger-1.5 hledger-iadd-1.3.1`


### 3. cabal

[cabal](https://www.haskell.org/cabal/) is the other Haskell build tool. If you're a cabal expert, feel free to use this instead.

<a name="c"></a>

## C. build the development version ?

Also a good choice. Our master branch is kept stable enough for daily use,
and includes the very latest improvements ([commits](https://github.com/simonmichael/hledger/commits/master)).

1. **Install [`stack`](#stack) and [git](https://en.wikipedia.org/wiki/Git)**
2. **`git clone https://github.com/simonmichael/hledger`**
3. **`cd hledger`**
4. **`stack install [hledger]`**  *# optionally specify just the hledger package to build less* 

cabal users may find the `cabal-install.sh` or `cabal.project` files useful.

<!-- now covered by stack.yaml I think:
 Nix users taking advantage of Stack integration may need to use Stack's `--no-nix-pure` flag to build hledger. -->
