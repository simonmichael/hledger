General project-related changes and major/notable all-package releases.
For package-specific changes and minor releases, see the package changelogs.

# ec4d131d2
- Merge branch 'bash-completion' (#1410)
  An extensive overhaul by @zhelezov of the bash programmable
  completions in shell-completions/.

  "This was supposed to be just a fix for #1404 but upon visiting the source
  several issues became apparent and that is why the commit grew a bit more than
  expected. A complete list of changes bellow:

  Fix #1404
  No more orphaned temporary directories. Commands, options, etc. that used to be
  stored in there are included at build-time as here documents in the source.

  Fix artifacts in /tmp after build
  Upon fixing the above I became aware that the build itself was leaving behind a
  heap of artifacts in /tmp that were not taken care of with a make clean.
  Fixed by using temporary files and directories in the build directory. Makefile
  and build scripts adjusted.

  Produce command aliases
  Regular expressions in build scripts changed to produce all command aliases
  except single letter ones (see below)

  Do not propose single letters completions
  It is simply not useful and adds a lot of noise. It makes completion slower as
  well because you need to hit yes on the prompt:
  Display all 200 possibilities? (y or n)
  output-options.sh now excludes those.

  Query filters simplified
  Keep only the prefix of the filter with the colon in query-filters.txt. This
  change has two reasons:

  Single letter completions are not useful (see above)
  It allows for completion suggestions specific to each
  Bonus reason: it's a completion engine, not a user manual.
  Fix completion impacts on global environment
  The completion script was making a couple of changes to the global environment
  which had an impact for the rest of the shell session.

  set -o pipefail: the change is hidden from the user and could lead to subtle
  errors throughout the shell session
  COMP_WORDBREAKS=" ": this affects subsequent completions for us and other
  programs too. I exclude the colon : from its value and use
  compopt -o filenames to handle escaping of special characters for us. I would
  like to find a solution without messing with COMP_WORDBREAKS but it is not
  straight forward.
  Fix hiding of legit subcommands
  Completion was hiding all possibilities if a subcommand happens to be the prefix
  of another. On typing balance, one should be proposed balancesheet and
  balancesheetequity as well.

  Return early
  Try to complete depending on the current context and return immediately if
  successful. Keep completion list relevant and as short as possible.

  Context aware completion

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

  Replace declare with local
  Again, this script is sourced by the shell -- keep variable scopes as narrow as
  possible. Oops, they are actually synonymous when used in a function but
  local declares our intentions explicitly.

  Use compopt -o nosort
  Often I resort to using it to keep different groups of completions together.
  Whether this is more ergonomic or not is subjective. But our input lists are
  already sorted at build-time so why not. Sort manually query-filters.txt when
  editing it.

  Remove irrelevant comments
  And add some new ones :)

  I think that is all. Give it a spin, try to abuse it, in and outside of quotes,
  with some funky accounts, payees, tags, whatever, and tell me where it breaks or
  behaves unexpectedly."

- stack: bump default to lts-17.4/ghc-8.10.4

- tools: Include more commodities and prices in generated journals. (Stephen Morgan)

- Rebuild completion after the rebase on upstream/master (Vladimir Zhelezov)
- `help` command's output is no longer listing help topics, so
  those completions are removed.
- `check` command supersedes `check-dates`, `check-dupes`, etc.

- Disable shell expansion in here-docs (Vladimir Zhelezov)
  Better be safe that sorry...

- Makefile: move all variable definitions to the top, before targets (Vladimir Zhelezov)

- README: fix syntax highlighting, quotes (Vladimir Zhelezov)

- Update README (Vladimir Zhelezov)

- BSDmakefile (Vladimir Zhelezov)
  Just print a reminder to use `gmake` instead of spurting a bunch
  of errors.

- Prevent unwanted m4 macro expansion + `-g` GNU compatibility flag (Vladimir Zhelezov)
  Most of the included files are meant to be output literally, without any
  macro processing. Using `undivert` in place of `include` achieves that.
  This is a safety net against unsanitized input generated during the
  build. Also, developers editing the shell code stub shouldn't be
  constantly alert about triggering accidental macro expansion.

  In order that `undivert` mimics GNU behaviour on BSDs, the `-g` flag is
  used for the m4 invocation.

- Portability: replace GNU extension `\s` with `[[:space:]]` (Vladimir Zhelezov)

- Revert "Do not use `set -e` in helper shell scripts" (Vladimir Zhelezov)
  This reverts commit 0aeb88e663b9b19e173de20b132bef2189c6d7c9.

- Install/uninstall completion for all extensions by default (Vladimir Zhelezov)
  Install the symlinks unconditionally. This way the user don't need
  to reinstall completion after adding an extension. Of course fine-
  grained control is possible with: `make install EXTENSIONS=web` e.g.

- Exit build with an error if unable to parse hledger sub-commands (Vladimir Zhelezov)

- Do not use `set -e` in helper shell scripts (Vladimir Zhelezov)
  It is not a substitute for proper error checking, it can easily cause
  more trouble than good and it would be a burden for contributors and a
  source for potential misbehavior. See this take on the topic for
  example: http://mywiki.wooledge.org/BashFAQ/105

- Add phony clean-all build target (Vladimir Zhelezov)
  A `make clean` before commit removes hledger-completion.bash and
  it is supposed to be in the repository. `make clean` removes build
  artifacts while keeping the latter. Do a `make clean-all` to purge
  everything.

- Remove `api` from hledger extensions (Vladimir Zhelezov)
  Superseded by hledger-web

- Update .gitignore (Vladimir Zhelezov)
  Options/commands parsing no longer creates any temporary files

- Add basic editor configuration to shell scripts (Vladimir Zhelezov)

- Move back commands/options parsing to separate shell scripts (Vladimir Zhelezov)
  It made the Makefile more difficult to read and this way we can
  take advantage of `set -e`, `-o pipefail` and friends.

  Clean up debugging targets

- Avoid hard-coded reference to `commands.tmp` (Vladimir Zhelezov)

- Add example usage comment for debug targets (Vladimir Zhelezov)

- Modify .gitignore (Vladimir Zhelezov)
  query-filters.txt is a build dependency and shouldn't be ignored.

- Add install and uninstall build targets (Vladimir Zhelezov)

- Move commands and options parsing into the Makefile (Vladimir Zhelezov)
  It is just a pipe of sed regex filters and all can be viewed/edited
  in one place. Add a couple of debug targets to see easily the
  effects of regex changes.

- Add stub file to m4 build target prerequisites (Vladimir Zhelezov)
  And as it becomes unwieldy, put all the dependencies in a variable

- Eliminate dependency on ‘paste’ and ‘parallel’ (Caleb Maclennan)

- Isolate shell code in a stub file included by m4 (Vladimir Zhelezov)
  This way we can easily edit m4 in m4-mode and the shell script stub
  in sh-mode and prevent subtle errors coming from accidental quoting
  issues or macros (mis)interpreted by m4.

- Fix build race condition (Vladimir Zhelezov)
  Make `command-options` a prerequisite of `hledger-completion.bash`.
  Currently the build succeeds only because the former takes less time
  to finish than all the prerequisites of the latter. If you run a
  `make clean && make -j 4`, the build would fail as they are built
  in parallel.

- Amend installation comment (Vladimir Zhelezov)

- Add a comment with example installation instructions (Vladimir Zhelezov)

- Remove _function from function names (Vladimir Zhelezov)

- Fix _hledger_extension_completion when called by path... (Vladimir Zhelezov)
  Could break if called with ./funky-path/with-dashes/hledger-ui

- return 0 (Vladimir Zhelezov)
  Shame on me...

- Fix a7dc62d: set $subcommandOptions unconditionally (Vladimir Zhelezov)

- Add an automatic check for required option argument (Vladimir Zhelezov)
  Get rid of manually listing unhandled long options:

  Instead of listing options requiring an argument one by one in the
  case statement in _hledger_compreply_optarg(), the option completion
  lists are searched and if the option does require an argument an
  empty COMPREPLY is send. Short options are uncounted for and still
  need a manual entry.

  This necessitated setting $subcommandOptions beforehand, so it is
  moved back where it was -- in the sub-command loop in main().

- Remove `compopt -o nospace` from --{close,open}-acct handler (Vladimir Zhelezov)

- Delay setting `compopt -o filenames` (Vladimir Zhelezov)
  ...until just before query completion. It is not desired for
  commands and options completion and is better used selectively
  only when really needed.

- Use compopt -o nospace only for a query prefix (Vladimir Zhelezov)
  Similarly to long options treatment, suspend space only when
  completing what looks like a query or account prefix i.e. something
  ending with a colon (:).

- White space (Vladimir Zhelezov)

- White space, fix alignment (Vladimir Zhelezov)

- Edit comments (Vladimir Zhelezov)

- Remove redundant call of _get_comp_words_by_ref() (Vladimir Zhelezov)

- Break overly long lines for better readability (Vladimir Zhelezov)
  Hopefully differences are aligned and easily discernible this way

- Remove _hledger_quote(), unused. (Vladimir Zhelezov)

- Remove unnecessary quoting, a matter of consistency... (Vladimir Zhelezov)

- Add an early return from option argument completion (Vladimir Zhelezov)
  Do not even enter the case statement if preceding words don't look
  like an option

- White space (Vladimir Zhelezov)

- Simplify sub-command loop logic (Vladimir Zhelezov)
  If the cursor is on the sub-command, just offer sub-command
  completions and be done with it. It was way over complicated.

- Move sub-command options reply out of the for-loop (Vladimir Zhelezov)
  It was just before the `break` statement anyways

- _hledger_compreply_query: minor refactor (Vladimir Zhelezov)

- Add a couple of sub-commands to the no-query list (Vladimir Zhelezov)

- Further refinement of option extraction regex (Vladimir Zhelezov)

- Improve option extraction (Vladimir Zhelezov)
  Do not consider lines starting with anything other than white space
  followed by a dash.

- Fix regular expression in output-options.sh (Vladimir Zhelezov)
  Match only lines starting with white space because there are a lot
  of option like strings in the discussion section that follows that
  are not necessarily what we are after.

- Adjust comment obsoleted by previous commit: 4d2a4b0 (Vladimir Zhelezov)

- Append `=` to long options requiring an argument (Vladimir Zhelezov)
  Make it obvious that the option expects an argument by appending
  the equal sign on completion. Suspend space in this case. The
  regular expression in `output-options.sh` is adjusted to take into
  account this on processing option strings.

- f656ff8 made another early return possible, so do it (Vladimir Zhelezov)
  Also it made a couple of statements redundant, cleaned up.

- Postpone options display until after entering a dash (Vladimir Zhelezov)
  I was looking at how other programs that have an overwhelming
  number of sub-commands and options deal with completion, namely
  how git does it, and I liked the clean workflow not spitting every
  available option until asked for. Hledger's main workflow is:

  > hledger COMMAND QUERY

  so I have tried to reproduce this with this change. Options are of
  course still there, but not shown until you ask for them by entering
  a dash on the command line. Also, the `acct:` filter proposes only top
  level accounts until there is some input from the user because accounts
  tend to be numerous as well.

- Rename $wordlist to $complist in _hledger_compgen() (Vladimir Zhelezov)
  Keep to established conventions

- Typo in comments (Vladimir Zhelezov)

- Make sure `=` is present in COMP_WORDBREAKS (Vladimir Zhelezov)
  Currently option processing logic is based on the assumption that `=` is
  a word-breaking character, so make sure it is present in COMP_WORDBREAKS

- A non-recursive version of _hledger_compreply_optarg() (Vladimir Zhelezov)
  Feels more streamlined and readable

- Make hard-coded completion word lists easy to find (Vladimir Zhelezov)
  A quick search for `wordlist=` should be enough to find hard-coded
  completions if we need to change them

- Simplify condition (Vladimir Zhelezov)
  Knowing the cursor position is enough to decide whether to complete
  subcommand or not, and subcommand is already known.

- Minor refactor of main function (Vladimir Zhelezov)
  No functional changes except special treatment of `help` subcommand

- Revert "Refactor _hledger_compreply_optarg()" (Vladimir Zhelezov)
  This reverts commit 2fd01d8ef51b897a63a2590556dbb3af7c13ffc9.

  Well, I was so wrong...

- Refactor _hledger_compreply_optarg() (Vladimir Zhelezov)
  It used to do a lot more work parsing the command line that is
  no longer necessary. Clean up redundant code.

- Clarify comment in extension completion function (Vladimir Zhelezov)

- Refactor _hledger_extension_completion_function() (Vladimir Zhelezov)
  Reduce number of instructions, remove variables used only once

- Fix extension completion (Vladimir Zhelezov)
  When inserting $extensionName in $COMP_WORDS manually, $COMP_CWORD lags
  behind by one. Needs a manual adjustment too.

- Fix duplicate call of _hledger_compreply_optarg() (Vladimir Zhelezov)
  It was called in both conditional branches so it's moved out before
  the conditional.

- Helper _hledger_debug() now accepts any number of arguments (Vladimir Zhelezov)

- Fix --long-opt= completion (Vladimir Zhelezov)
  Commit #64282f3f broke that somehow so I need to force the match on
  an empty string after the equal sign, not the equal sign itself.

  I think that _init_completion() does a bit more than I need...

- Style: unset compopt filenames if completing command or option (Vladimir Zhelezov)
  This is about the looks of the completion list -- if we have a
  directory with the name of a subcommand it will be presented with
  a trailing slash. This avoids that.

- Style: replace $cur with $subcommand (Vladimir Zhelezov)
  It changes nothing but spells out intention more clearly

- More option argument completions (Vladimir Zhelezov)
  Added handlers for:
  --output-format
  --close-acct
  --open-acct
  --debug

  Added --drop to blocking list

- Add optional arguments $prefix and $match to _hledger_compgen() (Vladimir Zhelezov)
  This allows more flexibility when generating completion candidates
  and we don't need to resort to external help from `sed` or others.
  _hledger_compgen's arguments become:

  $1 -> $wordlist:
     a newline separated wordlist with completion cadidates
  $2 -> $prefix:
     (optional) a prefix string to add to generated completions
  $3 -> $match:
     (optional) a word to match instead of $cur, the default.

  If $match is null and $prefix is defined the match is done against $cur
  stripped of $prefix. If both $prefix and $match are null we match against
  $cur and no prefix is added to completions. Of course you can also pass
  an empty string as $prefix and set $match to whatever you wish.

- Use _init_completion() (Vladimir Zhelezov)
  This handles a lot that we have to do manually otherwise. Without
  this we need to handle e.g. redirections to get completion for say:
  > hledger payees > <TAB>

  Also because this function assumes that we use `cur`, `prev`, `words`
  and `cword` and sets them up for us, `wordToComplete`, `COMP_WORDS`
  and `COMP_CWORD` are renamed accordingly. Those names are pretty much
  hard-coded in bash completion so it is easier to follow the lead than
  go with custom variable names.

- Fix or silence shellcheck warnings (Vladimir Zhelezov)
  There are a couple of places where (un)quoting is intentionally
  skipped so make those explicit.

- Query filter completion for amt, real and status; clean whitespace (Vladimir Zhelezov)

- Basic settings for major editors: prevent mixing tabs and spaces (Vladimir Zhelezov)

- Fix: pass *all* accounts and rules files to hledger calls (Vladimir Zhelezov)
  _hledger_optarg() is used to parse the options on the command line
  and provide their arguments for context aware completion suggestions

- Fix quoted/escaped file path handling in _hledger() (Vladimir Zhelezov)
  When reading hledger (rules-)file from COMP_WORDS we need to unescape it
  first. For once compgen is doing exactly what we need :)

- Fix #1404, and more... (Vladimir Zhelezov)
  This was supposed to be just a fix for #1404 but upon visiting the source
  several issues became apparent and that is why the commit grew a bit more than
  expected. A complete list of changes bellow:

- Fix #1404
  No more orphaned temporary directories. Commands, options, etc. that used to be
  stored in there are included at build-time as here documents in the source.

- Fix artifacts in =/tmp= after build
  Upon fixing the above I became aware that the build itself was leaving behind a
  heap of artifacts in =/tmp= that were not taken care of with a ~make clean~.
  Fixed by using temporary files and directories in the build directory. Makefile
  and build scripts adjusted.

- Produce command aliases
  Regular expressions in build scripts changed to produce all command aliases
  except single letter ones (see below)

- Do not propose single letters completions
  It is simply not useful and adds a lot of noise. It makes completion slower as
  well because you need to hit yes on the prompt:
  > Display all 200 possibilities? (y or n)
  =output-options.sh= now excludes those.

- Query filters simplified
  Keep only the prefix of the filter with the colon in =query-filters.txt=. This
  change has two reasons:
  - Single letter completions are not useful (see above)
  - It allows for completion suggestions specific to each
  - Bonus reason: it's a completion engine, not a user manual.

- Fix completion impacts on global environment
  The completion script was making a couple of changes to the global environment
  which had an impact for the rest of the shell session.

  ~set -o pipefail~: the change is hidden from the user and could lead to subtle
  errors throughout the shell session

  COMP_WORDBREAKS=" ": this affects subsequent completions for us and other
  programs too. I exclude the colon =:= from its value and use
  ~compopt -o filenames~ to handle escaping of special characters for us. I would
  like to find a solution without messing with COMP_WORDBREAKS but it is not
  straight forward.

- Fix hiding of legit subcommands
  Completion was hiding all possibilities if a subcommand happens to be the prefix
  of another. On typing ~balance~, one should be proposed ~balancesheet~ and
  ~balancesheetequity~ as well.

- Return early
  Try to complete depending on the current context and return immediately if
  successful. Keep completion list relevant and as short as possible.

- Context aware completion
  - Add handlers for option parameter completion, see _hledger_compreply_optarg()
  - Add handlers for query filters:, see _hledger_compreply_query()
  - Use --file and --rules-file arguments when proposing completions for the
    above, see _hledger()
  - Propose only top level accounts at first. Again, keep it short and focused.

- Custom ~compgen~ wrapper
  ~compgen~ is fairly complicated. There is no way to feed it a word list with
  literals. It will mangle your input in so many ways that we cannot trust it. To
  work around this several wrappers are used: _hledger_compgen() works with
  _hledger_quote_by_ref() to process and escape newline separated input which is
  then fed to ~compgen~ and finally in ~COMPREPLY~ through _hledger_compreply()
  and _hledger_compreply_append(). It sounds messy and I guess it is, I would like
  to find a more straight forward way to do it. I think it is still a way better
  and safer interface with ~readline~ than trying to ~grep~ our way through.

- Replace ~declare~ with ~local~
  Again, this script is sourced by the shell -- keep variable scopes as narrow as
  possible.

- Use ~compopt -o nosort~
  Often I resort to using it to keep different groups of completions together.
  Whether this is more ergonomic or not is subjective. But our input lists are
  already sorted at build-time so why not. Sort manually =query-filters.txt= when
  changing it.

- Remove irrelevant comments
  And add some new ones :)

  I think that is all. Give it a spin, try to abuse it, in and outside of quotes,
  with some funky accounts, payees, tags, whatever, and tell me where it breaks or
  behaves unexpectedly.

- doc: explain roi-unrealised.ledger, reference to cookbook (Dmitry Astapov)

- roi: roi-unrealised.example now works based on price directives (Dmitry Astapov)

- bin: more robust --package syntax, comma form sometimes fails

- examples: csv: daedalus wallet csv rules

- cli: rename --infer-value to --infer-market-price
  For clarity; infer-value was too vague. The old spelling remains
  supported for compatibility, but is now deprecated.
  When typing, --infer-market or even --infer (for now) is sufficient.

- install: 1.20.4

- install: 1.20.3

- bin: switch to "runghc", drop "env -S" (#1453)
  env -S isn't a thing on linux of course. Go back to using standard
  env, which means using a stack options line, which means not using
  "ghc". This new setup is probably simpler anyway. I've just had to
  give up on the goal of having each script's required packages being
  defined in one place; now (to they extent they are required) they
  must be defined both in the script header and in compile.sh.

- bin: switch scripts to "stack ghc" and "env -S" (#1453)
  Using stack's script command meant that the scripts needed to be
  compatible, and regularly tested, with a hledger release in stackage,
  rather than the latest hledger source. This created hassles for
  maintainers, contributors and sometimes for users.

  To simplify things overall, we now require script users to check out
  the hledger source tree and run the scripts (or, bin/compile.sh) from
  there once so they compile themselves. Some notes on alternative
  setups are included (in one of the scripts, and referenced by the
  others). This ensures that users and our CI tests are building scripts
  the same way.

  Current stack does not allow a stack options line to be used with the
  "stack ghc" command, unfortunately, so instead we are using env's -S
  flag, which hopefully has sufficiently wide support by now, and
  putting all arguments in the shebang line.

  This method will probably require complete explicit --package options,
  unlike "stack script", so more testing and tweaking is expected.
  Probably we're going to end up with some long shebang lines.

  This isn't pretty but seems like a possible way to keep things
  manageable.

- ci: really add addon-building tests to CI (#1453)

- stack: 8.10: bump to latest nightly and ghc 8.10.3

- bin: Update bin scripts for new API. (Stephen Morgan)

- bin: Update bin scripts for current hledger-lib. (Stephen Morgan)
  (cherry picked from commit bc4aef17b7fa13ec0754b93325e1c5e5ee04f1e7)

- ci: also test compilation of bin/ add-on scripts

- doc: merge file format manuals into the hledger manual
  Also flatten the journal manual topics a bit.

- doc: rewrite commands intro

- bin: switch to "runghc", drop "env -S" (#1453)
  env -S isn't a thing on linux of course. Go back to using standard
  env, which means using a stack options line, which means not using
  "ghc". This new setup is probably simpler anyway. I've just had to
  give up on the goal of having each script's required packages being
  defined in one place; now (to they extent they are required) they
  must be defined both in the script header and in compile.sh.

  (cherry picked from commit 32ccbba8050a26d09eb0fa8fdbc2c4b7ffe4f44c)

- bin: switch scripts to "stack ghc" and "env -S" (#1453)
  Using stack's script command meant that the scripts needed to be
  compatible, and regularly tested, with a hledger release in stackage,
  rather than the latest hledger source. This created hassles for
  maintainers, contributors and sometimes for users.

  To simplify things overall, we now require script users to check out
  the hledger source tree and run the scripts (or, bin/compile.sh) from
  there once so they compile themselves. Some notes on alternative
  setups are included (in one of the scripts, and referenced by the
  others). This ensures that users and our CI tests are building scripts
  the same way.

  Current stack does not allow a stack options line to be used with the
  "stack ghc" command, unfortunately, so instead we are using env's -S
  flag, which hopefully has sufficiently wide support by now, and
  putting all arguments in the shebang line.

  This method will probably require complete explicit --package options,
  unlike "stack script", so more testing and tweaking is expected.
  Probably we're going to end up with some long shebang lines.

  This isn't pretty but seems like a possible way to keep things
  manageable.

  (cherry picked from commit 2db87333d702d27ee45d8089ad4ad189bcb50cf2)

- ci: really add addon-building tests to CI (#1453)
  (cherry picked from commit 3ae6cf3200fad46cfbfa15c89e6d06dac309d76c)

- ci: also test compilation of bin/ add-on scripts
  (cherry picked from commit 06b466d847c46dc384cdef95658dade68111b173)

- bin: Update bin scripts for current hledger-lib. (Stephen Morgan)
  (cherry picked from commit bc4aef17b7fa13ec0754b93325e1c5e5ee04f1e7)
  (cherry picked from commit a64d1aa6d0bcaf643bbe2607238026b4d26a3637)

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
