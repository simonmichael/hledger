General/project-related changes in the hledger project. 
For package-specific changes, see the package changelogs.

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
