# Developer workflows

<div class=pagetoc>
<!-- toc -->
</div>

## Get developer tools

Ensure [`stack`](https://haskell-lang.org/get-started) is installed
(or if you’re a [cabal](https://www.haskell.org/cabal/) expert, feel free to use that.)

Ensure [`git`](https://git-scm.com) is installed. On Windows, it comes with stack.

Here are some useful optional tools:

- [GNU Make](https://www.gnu.org/software/make): to use the convenient [Make rules](#make).
- [`entr`](https://www.entrproject.org/) runs arbitrary commands when files change.
- [`ghcid`](https://hackage.haskell.org/package/ghcid) gives real-time GHC feedback as you make code changes.
- [`shelltestrunner`](https://hackage.haskell.org/package/shelltestrunner) runs hledger's functional tests.
- [`quickbench`](https://hackage.haskell.org/package/quickbench) measures and reports time taken by commands.
- [`hasktags`](https://hackage.haskell.org/package/hasktags) generates tag files for quick code navigation in editors like Emacs and vim.
- For browsing and editing Haskell code, popular tools include: Emacs, Vim, IDEA, VS Code, Atom..

Eg:

    stack install ghcid shelltestrunner hasktags
    git clone https://github.com/simonmichael/quickbench; cd quickbench; stack install  # must run in source dir

## Get the code

    git clone https://github.com/simonmichael/hledger
    cd hledger

## Review code

- review and discuss new [pull requests](http://prs.hledger.org) and commits on github
- build hledger and test the latest changes in your own repo
- read the existing [code docs and source](#quick-links)
- send feedback or discuss via [IRC or mail list](support.html)

## Build in place

See also https://hledger.org/install.html#c.-build-the-development-version .

    stack build    # hledger hledger-ui ...

This fetches the required GHC version and haskell dependencies from the default stackage snapshot (configured in `stack.yaml`), 
then builds all hledger packages.
This can take a while! To save time, you can build individual packages, eg just the CLI and TUI.

Note stack does not fetch C libraries such as curses or terminfo, which you might need to install yourself, using your system's package manager.
In case of trouble, see [download](/install.html#link-errors).

If you want to use an older snapshot/GHC for some reason, specify one of the older stack-*.yaml files:

    stack --stack-yaml stack8.2.yaml build
    
## Run in place

    stack exec -- hledger     # ARGS...
    stack exec -- hledger-ui  # ARGS...
    stack exec -- which hledger

## Build and install

This builds and also copies the hledger executables to `~/.local/bin` or the Windows equivalent
(which you should  [add to your `$PATH`](/install.html#b)).

    stack install    # hledger hledger-ui ...

## Run package tests

Runs any HUnit/doctest/easytest tests defined by each hledger package.

    stack test    # hledger ...

## Run package benchmarks

Runs any performance reports defined by each hledger package.

    stack bench    # hledger ...

## Run quickbench benchmarks

Times the end-user commands in `bench.sh` using quickbench.

    make bench

## Run functional tests

Runs the shelltestrunner tests defined in hledger/test/, which test the hledger CLI.

    make functest

## Run haddock tests

Checks for anything that would break haddock doc generation.

    make haddocktest

Checks for the unit-tests embedded in documentation.

    make doctest

## Simulate Travis tests

Locally runs tests similar to what we run on Travis CI.

    make travistest

## Test with all supported GHC versions/stackage snapshots

    make allsnapshotstest

## Use GHCI

GHCI is GHC's [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop), 
useful for exploring and calling code interactively.

If you try to run GHCI (or things based on it, like ghcid)
right after cloning the hledger repo, you might see an error about CPP macros, eg like
[on #961](https://github.com/simonmichael/hledger/issues/961#issuecomment-459283412).
To fix this, build the hledger packages once, eg `stack build`.
 (Or `stack build hledger` might be enough, depending what you are doing.)

### Get a GHCI prompt for hledger-lib:

    cd hledger-lib; stack ghci hledger-lib

Changing into the package directory isn't actually needed, but it
enables a custom .ghci script which sets a more useful short prompt.

### Get a GHCI prompt for hledger:

    cd hledger; stack ghci hledger

### Get a GHCI prompt for hledger-ui:

    cd hledger-ui; stack ghci hledger-ui

### Get a GHCI prompt for hledger-web:

    cd hledger-web; stack ghci hledger-web

hledger-web also needs to find some things in its current directory (like the static/ directory).
This normally just works, if not please [send details](https://github.com/simonmichael/hledger/issues/274).

## Add a test

- identify what to test
- choose the test type: unit ? functional ? benchmark ?
- currently expected to pass or fail ?
- figure out where it goes
- write test, verify expected result
- get it committed

## Fix a bug or add a feature

- research, discuss, validate the issue/feature on irc/list/bug tracker
- look for related tests, run the tests and check they are passing
- add a test ?
- develop a patch
- include any related issue numbers in the patch name, eg: "fix for blah blah (#NNN)"
- get it committed

## Get your changes accepted

Follow the usual github workflow:

- fork the main hledger repo on github,
- git clone it to your local machine,
- git commit, after (?) pulling and merging the latest upstream changes
- git push back to github,
- open a pull request on github,
- follow up on any discussion there.

If you're new to this process, [help.github.com](https://help.github.com) may be useful.

## Add yourself to the contributor list

- after getting something into the master branch, read and sign the [contributor list & agreement](https://hledger.org/contributors.html). Or, [ask](support.html-feedback) to be added.
- give yourself a high five!

## Work on docs

Most docs tasks are handled by [Shake](#shake). 

### List Shake rules:

    ./Shake

### Generate man/info/txt manuals (in hledger*/) and embed in hledger executables:

    ./Shake manuals
    stack build

### Generate html manuals and the hledger website (in site/_site/):

    ./Shake website

### To remove all files generated by Shake:

    ./Shake Clean

## Use ghcid for watching GHC/GHCI

[ghcid](https://hackage.haskell.org/package/ghcid) is the most reliable and fastest way to see GHC's feedback, and optionally run tests or a GHCI command, as you edit. We run it via make, for convenience and to watch multiple packages rather than just one. Run `make help-ghcid` to list related rules.

### Watch for compile errors in hledger-lib and hledger:

    make ghcid

### Watch compile errors and the output of some hledger command:

    ghcid -c 'make ghci' -T ':main -f a.j bal --budget -N'

## Use --file-watch for watching stack

stack's --file-watch flag will re-run build/test/bench when source files or package.yaml/cabal files change. Eg:

    stack test hledger --file-watch

If you find that adding --fast makes this any faster, please update this.

## Use entr for watching arbitrary commands

[entr](https://entrproject.org/) is the most robust cross-platform tool for watching files and running a command when they change. Note its first argument must be an executable program, to run a shell command or multiple commands use `bash -c "..."`.

### Rerun a single functional test as you change it:

    ls hledger/test/budget/budget.test | entr bash -c 'clear; COLUMNS=80 stack exec -- shelltest --execdir hledger/test/budget/budget.test -i12'
