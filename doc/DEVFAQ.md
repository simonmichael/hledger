# Developer FAQ

<!-- toc -->

This is just getting started. It will absorb some of the other [Developer docs](dev.md).

## Developing hledger

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

### How do I install a build in $PATH ?

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
