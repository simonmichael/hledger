;<details>
<summary>

## Release notes (https://hledger.org/release-notes.html#hledger-1-30)

</summary>

**Boolean queries, easier CSV file management, built-in demos, hledger-ui cash accounts screen, fixes.**

### hledger 1.30

Breaking changes

- The CSV reader now properly skips all empty lines, as specified by docs.
  Previously, inner empty lines were not being skipped automatically.
  You might need to adjust the `skip` count in some CSV rules files.
  (#2024)

- Certain reporting flags now toggle on/off if repeated. (Experimental)
  This can be useful for overriding defaults in command line scripts,
  since you can now turn a flag off as well as on. The flags which
  can be toggled are:

      --invert
      --transpose
      -r/--related
      -%/--percent
      -E/--empty
      -N/--no-total
      -T/--row-total
      -A/--average
      -S/--sort-amount

- Timedot format now generates a single multi-posting transaction per
  date line, and supports comments and tags on all lines.
  (#1754)

- Timeclock format now supports comments and tags.
  Descriptions can no longer contain semicolons.
  (#1220)

Features

- CSV rules files can now be read directly, as in
  `hledger -f foo.csv.rules CMD`. By default this will read data
  from foo.csv in the same directory.
  
- CSV rules files can use a new `source FILE` rule to specify the data file,
  with some convenience features:

  - If the data file does not exist, it is treated as empty, not an
    error.

  - If FILE is a relative path, it is relative to the rules file's
    directory. If it is just a file name with no path, it is relative
    to `~/Downloads/`.

  - If FILE is a glob pattern, the most recently modified matched file
    is used.

  This helps remove some of the busywork of managing CSV downloads.
  Most of your financial institutions's default CSV filenames are
  different and can be recognised by a glob pattern.  So you can put a
  rule like `source Checking1*.csv` in foo-checking.csv.rules,
  periodically download CSV from Foo's website accepting your browser's
  defaults, and then run `hledger import checking.csv.rules` to import
  any new transactions. The next time, if you have done no cleanup, your
  browser will probably save it as something like Checking1-2.csv, and
  hledger will still see that because of the * wild card. You can choose
  whether to delete CSVs after import, or keep them for a while as
  temporary backups, or archive them somewhere.
  (Experimental)

- The balance command has a new --count report type
  which reports posting counts instead of amounts.

- Full boolean queries, allowing arbitrary use of AND, OR, NOT
  (case insensitive) and parentheses for grouping, are now supported.
  For backward compatibility, these require an `expr:` prefix.
  Existing queries work as before, and you can mix and match the
  old and new styles if you like.
  (Chris Lemaire)
 
- demo: This new command plays brief asciinema screencasts explaining
  various features and use cases. We will add more of these over time.
  (Experimental)

Improvements

- Add-on commands can now have `.js`, `.lua`, or `.php` file extensions.

- Generated and modified transactions and postings have the same hidden
  tags (beginning with underscore) as before, but no longer have visible
  tags added by default. Use `--verbose-tags` if you want them added.

- We now try harder to ensure `less` (and its `more` mode) show our
  ANSI formatting properly in help output.
  If you use some other $PAGER, you may have to configure it yourself
  to show ANSI (or disable ANSI entirely, eg by setting NO_COLOR=1).
  This is now documented in hledger manual > Paging.
  (#2015)

- The print command's `--match` mode has been refined.
  Previously, similarity completely outweighed recency, so a
  slightly-more-similar transaction would always be selected no matter
  how old it was. Now similarity and recency are more balanced,
  and it should produce the desired transaction more often.
  There is also new debug output (at debug level 1) for troubleshooting.

- Miscellaneous commands list updates.
  Help has been added for all published add-on commands (like hledger-lots).

- The help command's documentation now mentions an issue caused by
  a too-old `info` program, as on mac.
  (#1770)

Fixes

- Unbalanced virtual postings with no amount always infer a zero amount.
  This is fixing and clarifying the status quo; they always did this,
  but print always showed them with no amount, even with -x, and
  the behaviour was undocumented.

- On windows systems with multiple drive letters, the commands list
  could fail to show all installed add-ons.
  (#2040)

- Balancing a transaction with a balance assignment now properly respects costs.
  (#2039)

- The commands list no longer lists non-installed addons.
  (#2034)

- Since hledger 1.25, "every Nth day of month" period rules with N > 28 could
  be calculated wrongly by a couple of days when given certain forecast start dates.
  Eg `~ every 31st day of month` with `--forecast='2023-03-30..'`.
  This is now fixed.
  (#2032)

- Postings are now processed in correct date order when inferring balance assignments.
  (#2025)

- Posting comment lines no longer disrupt the underline position in error messages.
  (#1927)

- Debug output is now formatted to fit the terminal width.

Docs

- Miscellaneous manual cleanups.

- Rewrite introductory sections,
  Date adjustment,
  Directives,
  Forecasting,
  etc.

- Add Paging section.

- Remove archaic mentions of `setenv`.

API

- Renamed: Hledger.Cli.Commands: findCommand -> findBuiltinCommand

### hledger-ui 1.30

Features

- A "Cash accounts" screen has been added, showing
  accounts of the `Cash` type.

Improvements

- The top-level menu screen is now the default screen.
  Power users can use the `--cash`/`--bs`/`--is`/`--all`
  flags to start up in another screen.

- "All accounts" screen has been moved to the bottom of the list.

- Screens' help footers have been improved.

Docs

- The transaction screen's inability to update is now noted.

- Miscellaneous manual cleanups.

### hledger-web 1.30

Fixes

- A command line depth limit now works properly.
  (#1763)

Docs

- Miscellaneous manual cleanups.

### project changes 1.30

Scripts/addons

- hledger-bar: new script for making simple bar charts in the terminal

- hledger-install: also list cabal, stack, pip tool versions

Examples

- examples/csv: added a more up-to-date CSV makefile

- examples/i18: Added sample top level account and type declarations in several languages

Docs

- A shorter, more example-heavy home page on the website.

- Simplified website and FAQ structure.

### credits 1.30

Simon Michael,
Chris Lemaire,
Yehoshua Pesach Wallach.

</details>

<details>
<summary>

## Install

</summary>

At <https://hledger.org/install>, binary packages should be available for this release within a few days (look for green badges). 

Or, you can build from source as described there, after cloning at tag `1.30`:
`git clone https://github.com/simonmichael/hledger --depth 1 -b 1.30`

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
curl -LOC- https://github.com/simonmichael/hledger/releases/download/1.30/hledger-linux-x64.zip   # can rerun if interrupted
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
curl -LOC- https://github.com/simonmichael/hledger/releases/download/1.30/hledger-mac-x64.zip
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
curl https://github.com/simonmichael/hledger/releases/download/1.30/hledger-windows-x64.zip -OutFile hledger-windows-x64.zip
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

</details>

<details>
<summary>

## Next steps

</summary>

- [Get Started](https://hledger.org/start.html)

<!-- ## Updates -->
<!-- 2022-06-08: windows-x64 binaries fixed. [#1869](https://github.com/simonmichael/hledger/issues/1869) -->

</details>
