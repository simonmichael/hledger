# Developer FAQ

<!-- toc -->

This is just getting started. It will absorb some of the other [Developer docs](dev.md).

<!-- ## Developing hledger -->

### How do I get/build the hledger source ?

```cli
$ git clone https://github.com/simonmichael/hledger
$ stack build
```
You can specify `hledger`, `hledger-ui` or `hledger-web` as an argument to build just that executable.
Please see [Install > Build from source](install.md#build-from-source) for more details and other build methods.

### What other repos are there ?

There are three official repos:
- <https://github.com/simonmichael/hledger> - the main hledger repo, for hledger, hledger-ui and hledger-web. Shortcut url: <https://code.hledger.org>
- <https://github.com/simonmichael/hledger_site> - the hledger.org website
- <https://github.com/simonmichael/hledger_finance> - the hledger project's financial ledger

And third-party add-ons and tools (hledger-iadd, hledger-utils, full fledged hledger, hledger-flow, etc.) have their own repos.

### How do I run a build in place ?

After building with stack,
```cli
$ stack exec -- hledger [ARGS]    # or hledger-ui, hledger-web
```

Or after building with cabal,
```cli
$ cabal exec -- hledger [ARGS]
```

### How do I install a build in PATH ?

```cli
$ stack install
```
This installs the hledger executables to `~/.local/bin`. You should have this directory configured in $PATH.
Or you can install to another directory with `--local-bin-path`.
It builds the executables first if needed; see [Install > Build from source](install.md#build-from-source) for more about building.
You can specify `hledger`, `hledger-ui` or `hledger-web` as an argument to build/install just that executable.

If you use cabal, it has a similar command; the argument is required.
It will install executables to `~/.cabal/bin`:
```cli
$ cabal install all:exes
```

### How do I build/run with ghc-debug support ?

You might need to stop background builders like HLS, to avoid a fight over the build flag
(in VS Code, run the command "Haskell: Stop Haskell LSP server").

Then build lib and executable(s) with the `ghcdebug` flag:
```cli
$ stack build --flag hledger-lib:ghcdebug --flag hledger:ghcdebug --flag hledger-ui:ghcdebug --flag hledger-web:ghcdebug
```

When the build is right, --version should mention ghc-debug:
```
$ stack exec -- hledger --version
... with ghc debug support
```

And when you run at debug level -1, -2 or -3 the output should mention ghc-debug:
```cli
$ hledger CMD --debug=-1    # run normally, and listen for ghc-debug commands
$ hledger CMD --debug=-2    # pause for ghc-debug commands at program start
$ hledger CMD --debug=-3    # pause for ghc-debug commands at program end
Starting ghc-debug on socket: ...
```

Now in another window, you can run [ghc-debug-brick](https://hackage.haskell.org/package/ghc-debug-brick) and it will show the hledger process (until it ends). Press enter to connect. This is not robust. Tips:
- you might need to clear out stale sockets: `rm -f ~/.local/share/ghc-debug/debuggee/sockets/*`
- you might need to kill stale hledger processes: `pkill -fl hledger`
- with --debug=-2 or -3 it might fail with this error, reasons unclear:\
  `rts_resume: called from a different OS thread than rts_pause`.\
  To work around, use --debug=-1.
  (This works best with hledger-ui or hledger-web; with hledger, you'll need to give it a big enough data file so that you can connect and pause it before it finishes.)
- once connected, if the program is running, press `p` to pause it

At this point, you can explore memory/profile information, save snapshots, resume execution, etc.

Or, instead of ghc-debug-brick you can write a [ghc-debug-client](https://hackage.haskell.org/package/ghc-debug-client) script to extract specific information.
