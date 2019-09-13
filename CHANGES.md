General/project-related changes in the hledger project. 
For package-specific changes, see the package changelogs.

# 4db14ef7

- add descriptions to most issue tracker labels

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
