General/project-related changes in the hledger project. 
For package-specific changes, see the package changelogs.

# afd7cae1

- install: fix "stack" installation when .local/bin is not in PATH (Dmitry Astapov)

- package: added helper scripts in docker/ (Dmitry Astapov)

- cli: command help: reduce width, line wrapping
  cmdargs wraps any lines longer than 78 characters.  To (mostly) avoid
  this, we now display verbatim blocks unindented, and some of
  register's examples have been altered to make them fit.

- shake: Shake PKG (or Shake build) builds packages plus their docs
  "stack build hledger" will not notice changes in documentation source
  files (like hledger/Hledger/Cli/Commands/Add.md or
  hledger-lib/hledger_journal.m4.md), but "./Shake hledger" will.

  "./Shake build" builds all the packages, like a doc-aware "stack build".

- make: ghci-shake

- package: added Dockerfile (Dmitry Astapov)

- site: download: link adept's & other docker images

- site: download: link sandstorm

- site: download: platform headings

- site: download: fix the TOC links

- site: home: link download page
  I accidentally the link.

- make: include Shake.hs in tags

- fixed pandoc typography conversion in web manuals (#954).
  Eg `--` was being rendered as en-dash.

- developer docs have moved from the wiki into CONTRIBUTING.md (#920)

- new streamlined changelog update process.
  "make changelogs" updates the project-wide and package changelogs,
  inserting new commits at the top, formatted as changelog entries.
  New commits means commits touching that directory since the tag
  version or commit hash which is the first word in the changelog's
  previous top-most heading.

- new command documentation process.
  Commands' CLI help and manual section are now generated from the same
  source (just the `close` command so far).

- doc files and hpack/cabal files are included in TAGS again

- make site-livereload - opens a reloading browser view on the website html
  (requires `livereloadx`)

- site: home: focus on "robust"

- site: remove the wasteful top nav bar, mostly

# 1.12 (2018/12/02)
