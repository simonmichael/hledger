General/project-related changes in the hledger project. 
For package-specific changes, see the package changelogs.

# cc9d7f8a

- stop building hledger-api

- bump default stack.yaml to lts-14.1 / ghc 8.6.5

- bin: add hledger-swap-dates; update hledger-check, hledger-smooth (#1072)

- Update shell completions (Jakob Schöttl)

- travis: skip hledger-api at least for now to stay under time limit

- Create FUNDING.yml / github sponsor button

- examples: fix incompatible syntax in bcexample.hledger

- make tag: include all hledger-web hs files in tags

- shake: be more verbose when updating changelogs

- shake: show htmlmanuals target in help

- make site-watch: site-liverender & site-livereload in one command

- site: download: wine

- tools: generatejournal: vary amount, make reports with fewer zeroes

- make samplejournals: more; don't remake ones used for tests

- tools: generatejournal: start from a fixed year, not last year
  So regenerating sample journals doesn't require updating tests.

- tools: generatejournal: also generate P records (#999)

- doc: add a README for the functional tests, linked from contrib guide

- Rename hledger-makeitso to hledger-flow (Andreas Pauley)

- docker: use "haskell" image as base (Dmitry Astapov)

- appveyor: enable web
  [skip ci]

- travis: enable web, api

- bump default stack.yaml to nightly-2019-03-09 / ghc 8.6.4

- shake site: also commit & push home page when wiki links change

- tools: move site.sh into shake; stop running just-pushed shakefile
  Running the just pushed Shake.hs was too insecure.

- make: rename sub-makefiles; start moving site script to make/shake

- tools: commit missing pandoc filters (#981)

- site: download: more cleanup

- site: download: improve nix/docker/sandstorm links/descriptions



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
