<details>
<summary>

## Release notes (https://hledger.org/release-notes.html#hledger-1-32)

</summary>

**More precision control, beancount output, TSV output, --summary-only,
strict/idempotent import, CSV rule enhancements, timedot letters, fixes.**

### hledger 1.32

Breaking changes

- Display styles and display precision are now managed more carefully
  during calculations and output, fixing a number of issues (#2111,
  "Precisiongeddon").  In brief:

  - Cost and value reports, such as `print -V`, now (1) consistently
    apply commodity display styles, and (2) do not add or discard
    decimal digits unnecessarily.  (#2105)
  
  - When "infinite decimals" arise during calculations (eg in value
    reports, or in `prices` or `roi` output), these are now shown
    limited to 8 decimal digits rather than 255.
  
  - Non-print-like reports no longer add trailing decimal marks to
    disambiguate digit group marks (this was an unintended regression
    in 1.31). (#2115)
    
  - We now document number formatting adjustments made in certain
    reports and output formats (hledger manual > REPORTING CONCEPTS >
    Amount formatting, parseability).


Features

- Timedot format supports a new letters syntax for easier tagged time logging.
  (#2116)

- `print` has a new `beancount` output format for exporting to Beancount.
  This prints journal output more likely (though not guaranteed) to
  be readable by Beancount.

- In CSV rules, matchers using regular expressions can now interpolate
  their matched texts into the values they assign to fields (field
  assignment values can reference match groups).
  (#2009) (Jonathan Dowland)
  
- In CSV rules, matchers can be negated by prepending `!`.
  (#2088) (bobobo1618)

- Multi-column balance reports (from `bal`, `bs`, `is` etc.) can use
  the new `--summary-only` flag (`--summary` also works) to display
  just the Total and Average columns (if enabled by `--row-total` and
  `-A/--average`) and hide the rest.
  (#1012) (Stephen Morgan)

- All commands that suport csv output now also support `tsv`
  (tab-separated values) output. The data is identical, but the fields
  are separated by tab characters and there is no quoting or
  escaping. Tab, carriage return, and newline characters in data are
  converted to spaces (this should rarely if ever happen in practice).
  (#869) (Peter Sagerson).


Improvements

- Journal format no longer fails to parse Ledger-style lot costs with spaces
  after the `{`, improving Ledger compatibility.

- `import` now does not update any .latest files until it has run
  without error (no failing strict checks, no failure while writing
  the journal file). This makes it more idempotent, so you can run it
  again after fixing problems.

- `print` now shows zeros with a commodity symbol and decimal digits
  when possible, preserving more information.

- `print` has a new option for controlling amount rounding (#2085):
  
  - `--round=none` - show amounts with original precisions (default;
    like 1.31; avoids implying less or more precision than was
    recorded)

  - `--round=soft` - add/remove decimal zeros in non-cost amounts
    (like 1.30 but also affects balance assertion amounts)

  - `--round=hard` - round non-cost amounts (can hide significant digits)

  - `--round=all`  - round all amounts and costs

  For the record:
  `print` shows four kinds of amount: posting amounts,
  balance assertion amounts, and costs for each of those.
  Past hledger versions styled and rounded these inconsistently.
  Since 1.31 they are all styled, and since 1.32 they are rounded as follows:
  
  | hledger-1.32 print | amt  | cost | bal  | balcost |
  |--------------------|------|------|------|---------|
  | (default)          | none | none | none | none    |
  | --round=soft       | soft | none | soft | none    |
  | --round=hard       | hard | none | hard | none    |
  | --round=all        | hard | hard | hard | hard    |

- The `prices` command has had a number of fixes and improvements (#2111):

  - It now more accurately lists the prices that hledger would use
    when calculating value reports (similar to what you'd see with
    `hledger bal -V --debug=2`).
    
  - The --infer-reverse-prices flag was confusing, since we always
    infer and use reverse prices; it has been renamed to `--show-reverse`.
    
  - `--show-reverse` and `--infer-market-prices` flags now combine properly.
    
  - `--show-reverse` now ignores zero prices rather than giving an error.
    
  - Price amounts are now shown styled.
  
  - Price amounts are now shown with all their decimal digits; or with
    8 decimal digits if they appear to be infinite decimals (which can
    arise with reverse prices).
    
  - Filtering prices with `cur:` or `amt:` now works properly.
    

Fixes

- `print` now styles balance assertion costs consistently, like other
  amounts.

- `import` now works with `-s/--strict`.
  And more generally, when reading multiple input files, eg with
  multiple `-f` options, strict checks are done only for the overall
  combined journal (not for each individual file).
  (#2113)

- `tag:` queries now work when reading CSV files. (#2114)

- Using a `.json` or `.sql` file extension with `-o`/`--outputfile`
  now properly selects those output formats.

- Auto postings no longer break redundant equity/cost detection and
  transaction balancing. (#2110)
  
- Amounts set by balance assignment now affect commodity styles again.
  (#2091, a regression in 1.30)

- Timedot quantities with units are parsed more accurately.
  Eg a quantity like "15m" was evaluated as 0.249999999 not 0.25,
  and since hledger 1.21, it was printed that way also.
  Now we round such quantities to two places during parsing to get
  exact quarter-hour amounts. (#2096)

- The `demo` command no longer triggers a JSON decode error in asciinema
  2.3.0. It now also shows a better error message if asciinema fails
  (#2094).

- Failing balance assertions with a cost now show correct markers in
  the error message. (#2083)


Docs

- New:
  
  - Amount formatting, parseability
  - Started new code docs for developers, based in the Hledger module's haddock

- Updated:
  
  - aregister
  - commodity directive
  - Commodity display style
  - if table
  - Decimal marks, digit group marks
  - Regular expressions
  - Timedot

  
### hledger-ui 1.32

Fixes

- The V key now preserves the valuation mode specified at the command
  line, if any. (#2084)

- The hledger-ui package no longer wastefully builds its modules
  twice.


### hledger-web 1.32

Features

- The hledger-web app on the Sandstorm cloud platform has been updated to
  a recent version (Jacob Weisz, #2102), and now uses Sandstorm's access
  control. (Jakub Zárybnický, #821)

Improvements

- The --capabilities and --capabilities-header options have been replaced
  with an easier `--allow=view|add|edit|sandstorm` option.
  `add` is the default access level, while `sandstorm` is for use on Sandstorm.
  UI and docs now speak of "permissions" rather than "capabilities".
  (#834)

- The Sandstorm app's permissions and roles have been renamed for clarity. (#834)

- Permissions are now checked earlier, before the web app is started,
  producing clearer command line errors when appropriate.

- Account's `adeclarationinfo` field is now included in JSON output. (#2097) (S. Zeid)

Fixes

- The app can now serve on address 0.0.0.0 (exposing it on all interfaces),
  which previously didn't work.
  (#2099) (Philipp Klocke)

- The broken "File format help" link in the edit form has been fixed. (#2103)


### project changes 1.32

Scripts/addons

- hledger-install.sh: replaced hledger-stockquotes with pricehist

- added gsheet-csv.hs: fetch a google sheet as CSV

- added hledger-report1: an example custom compound report, with haskell and bash versions

- justfile: updated import, time report scripts


Examples

- New:

  - Fidelity CSV rules

- Updated:

  - roi-unrealised.ledger (Charlie Ambrose)

Docs

- New:

  - Started a weekly This Week In Hledger news post, inspired by Matrix.
  - There's now a News page, for This Week In Hledger etc.
  - hledgermatic, an up-to-date, simple journal-first workflow
  - How to record journal entries: added
  - Reporting version control stats: added
  - Moved regression bounty info from the issue tracker to Developer docs > REGRESSIONS.

- Updated:

  - Checking for errors
  - Common workflows
  - Ledger
  - Simon's old setup
  - Videos
  - All docs now use the `cli` class instead of `shell` for command-line examples,
    avoiding inaccurate highlighting.


Infrastructure

- hledger.org website:

  - Fixed the webhook that was not updating the site on git push.
  
  - Fixed a problem with cloudflare authentication that was preventing
    automatic TLS certificate renewal on hledger.org.
  
  - Updated and committed hledger.org's caddy config and short urls (redirects)
  
  - Enabled https for code.hledger.org and site.hledger.org short urls.

  - Updated the stars.hledger.org redirect
    (we have reached the top 30 github-starred Haskell projects 🌟 🎉).
  
  - Set up a self-hosted Sandstorm server, and a public hledger-web
    instance (sandbox.hledger.org) in it that is fully writable (until
    spammers find it). Use it as a pastebin for examples, eg.

- Github CI (continuous integration) workflows have been optimised somewhat:

  - Scheduled weekly builds have been disabled, as they were propagating
    to forks and running wastefully there in some cases.

  - Some repeated rebuilding of the hledger-lib and hledger packages
    that seems unnecessary has been stopped.

  - hledger-ui no longer builds its modules twice.

  - Haddock testing now done only at release time.
  
  - renamed main CI workflow and branch to "ci"

- Tools:

  - .ghci: added an :rmain alias, which is like :main but reloads first -
  saves typing, and is useful eg when changing --debug level.

  - make haddock-watch is now fast


Finance

- Updated project finance scripts, regenerated the journal with consistent precisions.
  
- Updated reports with the last few months of data from Open Collective.


### credits 1.32

Simon Michael,
Jonathan Dowland,
S. Zeid,
Charlie Ambrose,
Jacob Weisz,
Peter Sagerson,
Philipp Klocke,
Stephen Morgan,
bobobo1618.


</details>

## Install

At <https://hledger.org/install>, binary packages should be available for this release within a few days (look for green badges). 

Or, you can build from source as described there, after cloning at tag `1.32`:
`git clone https://github.com/simonmichael/hledger --depth 1 -b 1.32`

Or, if under "Assets" below there are release binaries suitable for your OS and hardware, you can use those.
<!--
Note: release binaries have been updated:
- YYYY-MM-DD: description. [#NNNN](https://github.com/simonmichael/hledger/issues/NNNN)
-->
Here are platform-specific instructions for the release binaries.
(You can copy & paste each block of commands as a unit to save time.):

<details>
<summary>

### GNU/Linux on 64-bit Intel

</summary>

At the command line,

```
cd /usr/local/bin
curl -LOC- https://github.com/simonmichael/hledger/releases/download/1.32/hledger-linux-x64.zip   # can rerun if interrupted
unzip hledger-linux-x64.zip; tar xvf hledger-linux-x64.tar; rm hledger-linux-x64.{zip,tar}        # github workaround, preserves permissions
cd -
hledger --version  # should show the new version
touch $HOME/.hledger.journal   # ensure a default journal file exists
```

</details>

<details>
<summary>

### Mac on 64-bit Intel

</summary>

In a terminal window,

```
cd /usr/local/bin
curl -LOC- https://github.com/simonmichael/hledger/releases/download/1.32/hledger-mac-x64.zip
unzip hledger-mac-x64.zip; tar xvf hledger-mac-x64.tar; rm hledger-mac-x64.{zip,tar}              # github workaround, preserves permissions
open .
# for the hledger, hledger-ui, hledger-web icons: right-click, Open, confirm it's ok to run
cd -
hledger --version  # should show the new version
touch $HOME/.hledger.journal   # ensure a default journal file exists
```

</details>

<details>
<summary>

### Windows 64-bit Intel (or ARM, using emulation)

</summary>

In a powershell window (press Windows-r, type powershell, press enter),

1. Make a place to keep hledger binaries, and add it to your PATH; this makes running hledger easier. You only need to do this once, not for every release:
```
mkdir -force $HOME\bin >$null
$ENV:PATH += ";"+$HOME+"\bin"
[Environment]::SetEnvironmentVariable("Path", [Environment]::GetEnvironmentVariable("Path", [EnvironmentVariableTarget]::User)+";"+$HOME+"\bin", [EnvironmentVariableTarget]::User)
```

2. Download and install the release binaries:
```
cd $HOME\bin
curl https://github.com/simonmichael/hledger/releases/download/1.32/hledger-windows-x64.zip -OutFile hledger-windows-x64.zip
Expand-Archive hledger-windows-x64.zip -DestinationPath .
rm hledger-windows-x64.zip
cd $HOME
hledger --version           # should show the new version
```

3. Ensure a default journal file exists, and without a problematic encoding. 
(Not sure why "ascii" is needed here - hledger likes utf8 and understands utf8 BOM headers..
but the state of [our unicode support on Windows](https://github.com/simonmichael/hledger/issues?q=is%3Aissue+label%3A%22platform%3A+windows%22+label%3Ai18n)
is really unknown, your input welcome.)
```
out-file -append -encoding ascii $HOME/.hledger.journal
```

Once that journal file exists, you can start hledger-web by double-clicking on the icon if you wish.

</details>

<details>
<summary>

### Windows 7 on 64-bit Intel

</summary>

- click hledger-windows-x64.zip below
- choose Open with Windows Explorer, OK
- click Extract all files
- choose a destination folder - ideally one that appears in `echo %PATH%`, like `C:\Windows` (though that one will require administrator permission); otherwise, your home directory (`C:\Users\YOURNAME`)
- check "Show extracted files when complete"
- click Extract, wait for the destination folder to open
- find the hledger, hledger-web icons (if you extracted to `\Windows`, you'll need to scroll down)
- for each icon: double-click, uncheck "Always ask before opening this file", click Run
- close those Explorer windows
- open a command window (press Windows-r, type CMD, press enter)
- `hledger --version` should show the new version
- `echo # >> .hledger.journal` to ensure a default journal file exists. (Important: the doubled **>>** is needed to avoid overwriting existing data.)

Problems:
- Starting hledger by double-clicking its icon won't work because it needs arguments; run it from the command window instead.
- Starting hledger-web by double-clicking its icon may fail eg because Explorer's command window is too small;
  configure that to be larger, or run hledger-web from a command window instead.
- hledger or hledger-web may fail to run if there is not enough memory available.

</details>

## Next steps

- https://hledger.org/#quick-start

<!-- ## Updates -->
<!-- 2022-06-08: windows-x64 binaries fixed. [#1869](https://github.com/simonmichael/hledger/issues/1869) -->
