# Files

<div class=pagetoc>

<!-- toc -->
</div>

Some views and explanations of files in the hledger project, as of 2022-12.

<br clear=all>

## hledger working copy

A full working copy of the official hledger [repos](REPOS.html)
is best laid out like this (manually; we currently don't use git submodules):
<pre>
src/hledger/ - git clone https://github.com/simonmichael/hledger; cd hledger
  site/      - git clone https://github.com/simonmichael/hledger_site site
  finance/   - git clone https://github.com/simonmichael/hledger_finance finance
</pre>

You don't need to clone all of these repos unless you are working in all of those areas.

The next two listings show the directories in the main and site repos:

## main repo directories

The main repo contains
the hledger-lib, hledger, hledger-ui, and hledger-web haskell packages,
the hledger-install script,
a collection of example data,
some documentation
and other support files.

<!-- commands are starting points, manual fixups applied -->
<!-- $ (cd ~/src/hledger; tools/gtree -d) -->
<pre>
src/hledger/
  .github/
    ISSUE_TEMPLATE/
    workflows/
      old/
  .sandstorm/
  bin/
  checks/
  doc/
    haskellerz/
    hcar/
    mockups/
    profs/
  docker-static-arm32v7/
  docker/
  examples/
    budgeting/
    csv/
    investing/
    invoicing/
      invoice-script/
      makefile/
    reports/
    systemd/
    templates/
      basic/
  hledger-install/
  hledger-lib/
    Hledger/
      Data/
        JournalChecks/
      Read/
      Reports/
      Utils/
    Text/
      Megaparsec/
      Tabular/
    other/
      ledger-parse/
        Ledger/
          Parser/
    test/
  hledger-ui/
    Hledger/
      UI/
    test/
  hledger-web/
    Hledger/
      Web/
        Handler/
        Settings/
        Widget/
    app/
    config/
    deploy/
    static/
      css/
      fonts/
      js/
    templates/
    test/
  hledger/
    Hledger/
      Cli/
        Commands/
    app/
    bench/
    embeddedfiles/
    shell-completion/
    test/
      addons/
      balance/
      cli/
      errors/
      i18n/
      import/
      journal/
        account-display-order/
          1/
      print/
      register/
  tools/
</pre>

## site repo directories

The site repo contains the website infrastructure, 
versioned snapshots of the user manuals,
home page, cookbook and other docs.

<!-- $ (cd ~/src/hledger/site; ../tools/gtree -d) -->
<pre>
src/hledger/site/
  css/
  js/
  src/
    1.0/
    1.1/
    1.10/
    1.11/
    1.12/
    1.13/
    1.14/
    1.15/
    1.16/
    1.17/
    1.18/
    1.19/
    1.2/
    1.20/
    1.21/
    1.22/
    1.23/
    1.24/
    1.25/
    1.26/
    1.27/
    1.28/
    1.3/
    1.4/
    1.5/
    1.9/
    dev/
    fonts/
    highslide/
      graphics/
        outlines/
    images/
      hledger-ui/
      hledger-web/
        normal/
        small/
  theme/
    css/
</pre>

## finance repo

The finance repo contains transaction journals and financial reports.

<!-- $ (cd ~/src/hledger/finance; ../tools/gtree) -->
<pre>
src/hledger/finance/
  Makefile
  README.md
  oc.accounts
  oc.csv
  oc.csv.rules
  oc.journal
</pre>

## hledger user scripts

[Scripts](scripts.html) for users are in bin/:

<!-- $ gtree ^bin -->
<pre>
src/hledger/bin/
  README.md
  _hledger-chart.hs
  bashrc
  compile.sh
  csv.mk
  hledger-addon-example.hs
  hledger-balance-as-budget.hs
  hledger-check-fancyassertions.hs
  hledger-check-postable.hs
  hledger-check-tagfiles.cabal.hs
  hledger-check-tagfiles.hs
  hledger-combine-balances.hs
  hledger-git
  hledger-move.hs
  hledger-pijul
  hledger-print-location.hs
  hledger-simplebal
  hledger-smooth.hs
  hledger-swap-dates.hs
  paypaljson
  paypaljson2csv
  scripts.test
  watchaccounts
</pre>

## hledger developer tools

Scripts used by developers and maintainers tend to be in tools/:

<!-- $ gtree ^tools -->
<pre>
src/hledger/tools/
  README
  changelog.hs
  commitlint
  criterionbench.hs
  dayssincetag.hs
  docshelltest.hs
  generatejournal.hs
  generatetimeclock.hs
  gtree
  hackageupload
  listbydeps.hs
  pandoc-dedent-code-blocks.lua
  pandoc-demote-headers.lua
  pandoc-drop-html-blocks.lua
  pandoc-drop-html-inlines.lua
  pandoc-drop-links.lua
  pandoc-drop-toc.lua
  pandoc-toc.lua
  pandoc-wiki-links.lua
  progressionbench.hs
  regressiontest.py
  release
  runhledgercov
  simplifyprof.hs
  trhsx
</pre>

## Sh/makefiles

Many developer tasks are automated with Make and/or Shake also.
Run `make` or `./Shake` (after `make Shake`) to see help.

<!-- $ gtree '(^|/)((bsd)?m|sh)ake' -->
<pre>
src/hledger/
  Makefile
  Makefile.helpsys
  Shake.hs
  doc/
    MAKE.md
    SHAKE.md
    haskellerz/
      Makefile
  examples/
    invoicing/
      makefile/
        Makefile
    reports/
      Makefile
  hledger/
    shell-completion/
      BSDmakefile
      Makefile
    test/
      errors/
        Makefile
</pre>

## YAML files

Developer configuration often happens in .yaml or .yml files.

<!-- $ gtree ya?ml -->
<pre>
src/hledger/
  .hlint.yaml
  azure-pipelines.yml
  hie-other.yaml
  stack.yaml
  stack8.10.yaml
  stack9.0.yaml
  stack9.4.yaml
  .github/
    FUNDING.yml
    ISSUE_TEMPLATE/
      config.yml
    workflows/
      binaries-linux-arm32v7-static.yml
      binaries-linux-x64-static.yml
      binaries-mac-x64.yml
      binaries-windows-x64.yml
      test-linux-x64.yml
      old/
        release.yml
  hledger-lib/
    package.yaml
  hledger-ui/
    package.yaml
  hledger-web/
    package.yaml
    config/
      keter.yaml
      settings.yml
  hledger/
    package.yaml
</pre>

## Core docs

Core documentation which should stay closely synced with hledger's implementation
(changelogs, user manuals, developer docs) is kept in the main repo.

- Many directories have a README.md explaining their purpose and content.

- Each hledger package, and the project itself, has a CHANGES.md changelog file.

- hledger/hledger.m4.md, hledger-ui/hledger-ui.m4.md, hledger-web/hledger-web.m4.md
  are the user manuals, which get rendered as html, info, man and plain text.
  They are processed first with m4 for extra flexibility.

- The hledger manual imports the subcommand docs from hledger/Hledger/Cli/Commands/*.md.

- doc/ contains other developer docs.

<!-- $ gtree md -->
<pre>
src/hledger/
  CHANGES.md
  README.md
  .github/
    pull_request_template.md
    ISSUE_TEMPLATE/
      a-bug.md
      a-wish.md
  .sandstorm/
    README.md
    changelog.md
    description.md
  bin/
    README.md
  doc/
    ACHIEVEMENTS.md
    BENCHMARKS.md
    CHANGELOGS.md
    CODE.md
    COMMITS.md
    CONTRIBUTING.md
    CREDITS.md
    DOCS.md
    EXAMPLES.md
    FILES.md
    FINANCE.md
    ISSUES.md
    LINKS.md
    MAKE.md
    PULLREQUESTS.md
    RELEASING.md
    REPOS.md
    SHAKE.md
    TESTS.md
    VERSIONNUMBERS.md
    WORKFLOWS.md
    github-release-doc.tmpl.md
    haskellerz/
      haskellerz.md
    hcar/
      HCAR-hledger-201611.md
  docker/
    README.md
  examples/
    invoicing/
      README.md
      invoice-script/
        README.md
        abinvoice.tmpl.md
      makefile/
        202001ab.md
        README.md
    systemd/
      hledger-web.service
      hledger.nginx
      readme.md
    templates/
      README.md
  hledger-install/
    README.md
  hledger-lib/
    CHANGES.md
    README.md
  hledger-ui/
    CHANGES.md
    README.md
    hledger-ui.m4.md
    test/
      uitest.md
  hledger-web/
    CHANGES.md
    README.md
    hledger-web.m4.md
  hledger/
    CHANGES.md
    README.md
    hledger.m4.md
    Hledger/
      Cli/
        Commands/
          Accounts.md
          Activity.md
          Add.md
          Aregister.md
          Balance.md
          Balancesheet.md
          Balancesheetequity.md
          Cashflow.md
          Check.md
          Close.md
          Codes.md
          Commodities.md
          Descriptions.md
          Diff.md
          Files.md
          Help.md
          Import.md
          Incomestatement.md
          Notes.md
          Payees.md
          Prices.md
          Print.md
          README.md
          Register.md
          Rewrite.md
          Roi.md
          Stats.md
          Tags.md
          Test.md
    shell-completion/
      README.md
    test/
      README.md
      errors/
        README.md
</pre>

## Additional docs

Additional docs (intro pages, faqs, tutorials, cookbook..)
are kept in the site repo's src/ directory.
index.md is the home page.
SUMMARY.md defines the site structure.

Developer docs (UPPERCASE) and the latest dev manuals (dev/*.md) from
the main repo are also symbolically linked there, so that they too
appear on the website.

<!-- $ cd site; gtree md -->
<pre>
src/hledger/site/src/
  ACHIEVEMENTS.md
  BACKLOG.md
  BENCHMARKS.md
  CHANGELOGS.md
  CODE.md
  COMMITS.md
  CONTRIBUTING.md
  CREDITS.md
  DOCS.md
  ERRORS.md
  EXAMPLES.md
  FILES.md
  FINANCE.md
  ISSUES.md
  LINKS.md
  MAKE.md
  PULLREQUESTS.md
  RELEASING.md
  REPOS.md
  ROADMAP.md
  SHAKE.md
  SUMMARY.md
  TESTS.md
  VERSIONNUMBERS.md
  WORKFLOWS.md
  accounting.md
  add.md
  balancing-the-accounting-equation.md
  beancount.md
  budgeting-and-forecasting.md
  budgeting.md
  change-account-name-separator.md
  charts.md
  checking-for-errors.md
  command-line-completion.md
  common-journal-entries.md
  common-workflows.md
  conversion2.md
  cookbook.md
  create-a-journal.md
  currency-conversion.md
  dev-README.md
  dev.md
  dsq.md
  editors.md
  export.md
  faq.md
  features.md
  financerepo.md
  finfaq.md
  forecasting.md
  foreign-trip-expenses.md
  gain.md
  gnucash.md
  hledger-web-tips.md
  import-csv.md
  index.md
  install.md
  inventory.md
  investments.md
  invoicing.md
  ledger.md
  loans.md
  manuals.md
  mobile.md
  mockups.md
  multicurrency-tutorial.md
  project-accounting.md
  quicken.md
  2-minute-quick-start.md
  5-minute-quick-start.md
  release-notes.md
  report-examples.md
  rewrite-account-names.md
  rewrite-commodity-symbols.md
  roi.md
  save-frequently-used-options.md
  scripting.md
  scripts.md
  simons-setup.md
  snippets.md
  sponsor.md
  sqlite.md
  start.md
  support.md
  tags-tutorial.md
  time-planning.md
  time-to-money.md
  track-changes-with-version-control.md
  track-investments.md
  ui.md
  videos.md
  web.md
  1.0/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.1/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.10/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.11/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.12/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.13/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.14/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.15/
    csv.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.16/
    csv.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.17/
    csv.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.18/
    csv.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.19/
    csv.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.2/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.20/
    csv.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.21/
    hledger-ui.md
    hledger-web.md
    hledger.md
  1.22/
    hledger-ui.md
    hledger-web.md
    hledger.md
  1.23/
    hledger-ui.md
    hledger-web.md
    hledger.md
  1.24/
    hledger-ui.md
    hledger-web.md
    hledger.md
  1.25/
    hledger-ui.md
    hledger-web.md
    hledger.md
  1.26/
    hledger-ui.md
    hledger-web.md
    hledger.md
  1.27/
    hledger-ui.md
    hledger-web.md
    hledger.md
  1.28/
    hledger-ui.md
    hledger-web.md
    hledger.md
  1.3/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.4/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.5/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  1.9/
    csv.md
    hledger-api.md
    hledger-ui.md
    hledger-web.md
    hledger.md
    journal.md
    timeclock.md
    timedot.md
  dev/
    hledger-ui.md
    hledger-web.md
    hledger.md
</pre>

## Site config files

These help configure the website.

- [book.toml](https://github.com/simonmichael/hledger_site/blob/master/book.toml) is the main config file for mdbook.

- [src/SUMMARY.md](https://github.com/simonmichael/hledger_site/blob/master/src/SUMMARY.md)
  defines the site's pages and which ones appear in the sidebar
  (except for old manual versions; those are rendered separately).

<!-- $ gtree 'Makefile|toml|css|js|theme|SUMMARY' -->
<pre>
src/hledger/site/
  Makefile
  book.toml
  css/
    site.css
  js/
    bootstrap.min.js
    jquery-1.11.0.min.js
    site.js
  src/
    SUMMARY.md
    highslide/
      highslide-ie6.css
      highslide.css
      highslide.js
  theme/
    book.js
    favicon.png
    favicon.svg
    highlight.css
    highlight.js
    index.hbs
    css/
      chrome.css
      general.css
      print.css
      variables.css
</pre>

<!-- template: -->
<!-- ## title -->
<!-- <\!-- $ gtree ... -\-> -->
<!-- <pre> -->
<!-- </pre> -->
<!--  -->
