General/project-related changes in the hledger project. 
For package-specific changes, see the package changelogs.

# 8c9d8052

- ; site: download: drop unpinned/failing nix command (#980); cleanups

- ; site: download: consistent hledger-install.sh naming

- ; site: download: drop nix badges

- ; site: download: add nix pinned command (#980); drop double borders

- ; site: download: drop linuxbrew links, that was an old name (#321)

- ; site: download: latest nix wording (#980)

- ; site: download: fix hydra links (#980)

- ; site: home: wording tweaks

- ; site: download: drop unnecessary link brackets
  http://pandoc.org/MANUAL.html#reference-links -> shortcut_reference_links

- ; site: download: update nix command (#980); links cleanup

- doc: mention dockerized source build (Dmitry Astapov)

- docker: use "haskell" image as base (Dmitry Astapov)

- ;site: download: refine openbsd, sandstorm links

- ;site: download: drop third-party packages that seem out of date

- ;site: download: consistent install tool/package links on left/right

- ; site: download: add manual badges where needed

- ; site: home: reorder intro links, add offsite arrow

- ; site: home: rewrite top content

- ;site: update cookbook links

- ; site: home: streamline left column content

- ; site: home: more prominent install, github buttons

- ; site: download: mention windows unicode display issue (#961)

- ; site: download: enlarge font

- ; site: download: stack: can simplify that further, why not

- ; site: download: simplify stack command, ensure GHC 8.6.4

- ; site: download: latest windows binary; windows tweaks

- ; site: download: links to windows build issues; wording

- ; site: download: order linux distros by freshness

- ; site: download: update windows binary

- appveyor: note 7z docs
  [skip ci]

- appveyor: enable web
  [skip ci]

- travis: enable web, api

- travis: build fewer packages, refilling cache after resolver bump

- bump default stack.yaml to nightly-2019-03-09 / ghc 8.6.4

- ;site: download: bump gentoo version

- ;shake hledgerorg: tweak home page commit message

- ;site: home: wiki links

- ;site: home: wiki links

- ;fix pull request template link; comment the help text

- ;site: home: wiki links

- make: cleanups

- make site: remove a bashism

- ;site: home: wiki links

- make site: log to site.log again

- shake site: also commit & push home page when wiki links change

- tools: move site.sh into shake; stop running just-pushed shakefile
  Running the just pushed Shake.hs was too insecure.

- tools: make site.sh mac compatible

- shake: don't echo commands by default

- shake: clean up docs

- make: rename sub-makefiles; start moving site script to make/shake
  [ci skip

- site: download: note ghc 8.6.3 hang on windows

- site: download: drop hledger-diff addon

- install: stop installing hledger-diff addon (#981)

- shake commandhelp: don't generate a txt for the README (#981)

- site: download: ensure nix install gets latest available (#980)

- tools: commit missing pandoc filters (#981)

- site: download: more cleanup

- site: download: tweaks

- site: simplify download page

- site: download: bump docker version

- site: download: improve nix/docker/sandstorm links/descriptions

- site: download: simplify nix instructions (#980)

- install: don't forget to bump own version

- download page, hledger-install: bump hledger-iadd version

- site: download: reduce popping of "latest release is"

- site: relnotes: show minor release, link announcent

- download page, hledger-install: bump hledger version

- shake: add 1.14 to web manual versions


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
