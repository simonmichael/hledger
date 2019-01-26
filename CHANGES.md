General/project-related changes in the hledger project. 
For package-specific changes, see the package changelogs.

# 0a31ef84

# 9bc88727

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
