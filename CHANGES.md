General project-related changes and major/notable all-package releases.
For package-specific changes and minor releases, see the package changelogs.

# 1.21 2021-03-10

- roi has a new cookbook doc, and example files have been updated.
  (Dmitry Astapov)

- Example CSV rules for the Daedalus wallet have been added.

- The default stackage resolver/GHC version has been bumped to
  lts-17.4/ghc-8.10.4.

- tools/generatejournal now includes more commodities and prices in
  generated journals. (Stephen Morgan)

- Our functional tests now also run on BSD. (#1434, Felix Van der Jeugt)

- Addon scripts in bin/ have been updated for latest hledger API (Stephen Morgan).

- Addon scripts are now compiled as part of our CI tests, and always
  with the same version of hledger source they were shipped with. We
  now require script users to check out the hledger source tree and
  run the scripts (or, `bin/compile.sh`) from there. This keeps users
  and tests in sync, making things more reliable for everyone. (#1453)

- Last but not least, hledger's bash completions (provided in ./shell-completions/)
  have been thoroughly updated (#1404, #1410, Vladimir Zhelezov).

  "This was supposed to be just a fix for #1404 but upon visiting the source
   several issues became apparent and that is why the commit grew a bit more than
   expected. A complete list of changes below:
  
  - Fix #1404.
    No more orphaned temporary directories. Commands, options, etc. that used to be
    stored in there are included at build-time as here documents in the source.
  
  - Fix artifacts in /tmp after build.
    Upon fixing the above I became aware that the build itself was leaving behind a
    heap of artifacts in /tmp that were not taken care of with a make clean.
    Fixed by using temporary files and directories in the build directory. Makefile
    and build scripts adjusted.
  
  - Produce command aliases.
    Regular expressions in build scripts changed to produce all command aliases
    except single letter ones (see below)
  
  - Do not propose single letters completions.
    It is simply not useful and adds a lot of noise. It makes completion slower as
    well because you need to hit yes on the prompt:
    Display all 200 possibilities? (y or n)
    output-options.sh now excludes those.
  
  - Query filters simplified.
    Keep only the prefix of the filter with the colon in query-filters.txt. This
    change has two reasons:
  
  - Single letter completions are not useful (see above).
    It allows for completion suggestions specific to each
    Bonus reason: it's a completion engine, not a user manual.
    Fix completion impacts on global environment
    The completion script was making a couple of changes to the global environment
    which had an impact for the rest of the shell session.
  
  - set -o pipefail: the change is hidden from the user and could lead to subtle
    errors throughout the shell session.
    COMP_WORDBREAKS=" ": this affects subsequent completions for us and other
    programs too. I exclude the colon : from its value and use
    compopt -o filenames to handle escaping of special characters for us. I would
    like to find a solution without messing with COMP_WORDBREAKS but it is not
    straight forward.

  - Fix hiding of legit subcommands.
    Completion was hiding all possibilities if a subcommand happens to be the prefix
    of another. On typing balance, one should be proposed balancesheet and
    balancesheetequity as well.
  
  - Return early.
    Try to complete depending on the current context and return immediately if
    successful. Keep completion list relevant and as short as possible.
  
  - Context aware completion
  
    Add handlers for option parameter completion, see _hledger_compreply_optarg()
    Add handlers for query filters:, see _hledger_compreply_query()
    Use --file and --rules-file arguments when proposing completions for the
    above, see _hledger()
    Propose only top level accounts at first. Again, keep it short and focused.
    Custom compgen wrapper
    compgen is fairly complicated. There is no way to feed it a word list with
    literals. It will mangle your input in so many ways that we cannot trust it. To
    work around this several wrappers are used: _hledger_compgen() works with
    _hledger_quote_by_ref() to process and escape newline separated input which is
    then fed to compgen and finally in COMPREPLY through _hledger_compreply()
    and _hledger_compreply_append(). It sounds messy and I guess it is, I would like
    to find a more straight forward way to do it. I think it is still a way better
    and safer interface with readline than trying to grep our way through.
  
  - Replace declare with local.
    Again, this script is sourced by the shell -- keep variable scopes as narrow as
    possible. Oops, they are actually synonymous when used in a function but
    local declares our intentions explicitly.
  
  - Use compopt -o nosort.
    Often I resort to using it to keep different groups of completions together.
    Whether this is more ergonomic or not is subjective. But our input lists are
    already sorted at build-time so why not. Sort manually query-filters.txt when
    editing it.
  
  - Remove irrelevant comments.
    And add some new ones :)
  
  I think that is all. Give it a spin, try to abuse it, in and outside of quotes,
  with some funky accounts, payees, tags, whatever, and tell me where it breaks or
  behaves unexpectedly."

# 1.20.4 2021-01-29

# 1.20.3 2021-01-14

- The run/compile instructions for add-on scripts in bin/ have been
  updated. The scripts now use `stack runghc` and are tested (manually
  with `make functest` for now) with the corresponding hledger source,
  not the hledger on stackage.

# 1.20 2020-12-05

- examples: clean up & add more budgeting examples

- examples: stripe csv

- The functional tests in tests/ have been moved into the respective
  packages, eg hledger/test/ and hledger-ui/test/.

- Shake cabalfiles: now gives an error when it fails

- make bench: add some large tabular reports; 
  run just the slowest commands by default;
  run after make (func)test

- a hie.yaml file has been added, so hledger source loads
  easily in IDEs supporting haskell-language-server

# 2020-09-07

- Update shell completions (Jakob Schöttl)

# 1.19 2020-09-01

- example scripts:

  - stack scripts now use stack's script command consistently
  - stack scripts no longer have explicit --package lists, stack
    infers them from the imports
  - hledger-print-location: new script

- CI: 

  - always recompile all modules for robustness
  - generate optimised binaries, which can be downloaded
  - build a single different GHC version with each workflow, reducing
    total building and carbon footprint a bit
  - stop building with GHC 8.0

- the default stack file now uses lts 16.12 (ghc 8.8.4)

# 1.18.1 2020-06-21

- provide CI binaries for windows, mac & gnu/linux

# 1.18 2020-06-07

- new example scripts:

  - hledger-combine-balances.hs, hledger-balance-as-budget.hs  (Dmitry Astapov)
  - hledger-check-tag-files.hs, hledger-check-tag-files2.hs

- more CSV rule examples: coinbase, waveapp

- new CI (continuous integration) system using Github Actions.
  Thanks to Travis and Appveyor for their service to date.
  Improvements:

  - one CI service instead of several
  - more closely integrated with code repo
  - tests run on the three main platforms (linux, mac, windows)
  - harmless commits are ignored automatically ([ci skip] no longer needed for doc commits)
  - scheduled and on-demand testing (push to master, push to ci-* branches, pull request, weekly)
  - now tested: all GHC versions, doctests, haddock building
  - new shortcut url: http://ci.hledger.org

# 1.17 2020-03-01

- hledger-install: re-enable installation of hledger-iadd & hledger-interest.

- hledger-install: bump minimum stack version to 1.9.1
  1.7.1 fails with deps using newer cabal file syntax I believe.

- hledger-install: always do stack update, to help ensure we get the latest packages.
  https://github.com/commercialhaskell/stack/issues/5112

- examples: Add a basic example of rule parsing for the output of csb2format. (Evilham)
  csb2format deals with the CSB43/AEB43 format, which all banks operating in
  Spain must support.
  Having these example rules enables easens bootstraping for users with a
  Spanish bank account.

- doc: simpler, clearer structure in the manuals and hledger.org sidebar

- doc: a new [Quick Start](https://hledger.org/start.html) page

- doc: a new [Common Tasks](https://hledger.org/hledger.html#common-tasks) section in the hledger manual

- doc: a new invoicing how-to: https://hledger.org/invoicing.html

- doc: Fix dead pointer in contributing (Aleksandar Dimitrov)

- doc: Fix build badges for Travis and AppVeyor (Rui Chen)

# 1.16 2019-12-01

- add support for GHC 8.8, base-compat 0.11 (#1090)

- drop support for GHC 7.10

- add descriptions to most issue tracker labels

- matrix.hledger.org now redirects to a more readable/useful url

# 1.15 2019-09-01

- install: bump to lts-14.4, hledger 1.15, drop hledger-api

- bump versions to 1.15

- api: drop from Shake scripts

- new unified website: hledger.org now has its own git repo, has
  absorbed the github wiki, and is generated with Sphinx.

- hledger-api's functionality is now included in hledger-web,
  and the hledger-api package is mothballed.

- hledger-install.sh: updated, now also works on FreeBSD 12 (zieone)

- bin/ addon scripts: hledger-swap-dates added; hledger-check,
  hledger-smooth updated. (#1072)

- shell-completion/ scripts: updated (Jakob Schöttl)

- github: FUNDING.yml / sponsor button configured

- site: Wine option added to download page

- tools: generatejournal updates: vary amount, make reports with fewer
  zeroes, start from a fixed year to keep tests stable, also generate
  P records. (#999)

- tools: make, shake, CI: misc. updates

- doc: add a README for the functional tests, linked from contrib guide

- hledger-makeitso has been renamed to hledger-flow (Andreas Pauley)

- The hledger docker image is now based on the "haskell" image (Dmitry Astapov)


# 1.14 2019-03-01

- hledger.org website: now uses https, home page updates,
  download page improved package list with status badges.
  Also the github wiki pages are now rendered as part of hledger.org,
  like the main site pages (with pandoc markdown and tables of contents).
  Building the site now requires that a copy of the wiki is checked out
  under wiki/.

- bash completion support: removed duplicate options, added new
  options, stopped listing -h as a command, added some completion for
  external addon commands.

- release automation improvements

- makefile cleanups; make site-liverender helps with local site preview

# 1.13 (2019/01/02)

- packaging: A docker image providing the main hledger tools is now
  linked on the download page. This is another way to get up-to-date
  hledger tools without building them yourself (and, a way to run
  hledger-ui on windows ?) (Dmitry Astapov, Simon Michael)

- hledger-install.sh: fix installation of stack when .local/bin is not
  in PATH (Dmitry Astapov)

- doc: fixed pandoc typography conversion in web manuals. Eg `--` was
  being rendered as en-dash. (#954).

Developers:

- developer docs have moved from the wiki into CONTRIBUTING.md (#920)

- new streamlined changelog update process. Shake targets:
  
      ./Shake changelogs
      ./Shake CHANGES.md
      ./Shake CHANGES.md-dry
      ./Shake PKG/CHANGES.md
      ./Shake PKG/CHANGES.md-dry

  update the project-wide and/or package changelogs, inserting new
  commits (touching the respective directory, since the tag version or
  commit hash which is the first word in the changelog's previous top
  heading) at the top, formatted as changelog entries.

- ./Shake PKG - builds a package plus its embedded docs.
  ./Shake build - builds all the packages and their embedded docs.
  ("stack build PKG" does not notice changes in embedded doc files.)

- make ghci-shake - loads Shake.hs in ghci

- make tags - includes doc source files, hpack/cabal files, Shake.hs

- make site-livereload - opens a reloading browser view on the website html
  (requires `livereloadx`)

- added a Dockerfile and helper scripts (Dmitry Astapov)
  
- doc files and hpack/cabal files are included in TAGS again

# 1.12 (2018/12/02)
