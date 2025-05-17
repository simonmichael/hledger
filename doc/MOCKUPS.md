# MOCKUPS

<div class=pagetoc>

<!-- toc -->
</div>

Old mockups, draft docs and notes exploring possible future features.
See also <https://github.com/simonmichael/hledger/tree/master/doc/mockups>

## Ease of getting started

What could make getting started substantially easier ?

- Official CI-generated binaries for all major platforms
- Builtin access to docs in web format

## Web docs

Provide the embedded user manuals as HTML also. Eg:

- hledger help --html   # temporary static html files
- hledger help --web    # serve from local hledger-web instance if installed
- hledger help --site   # on hledger.org
- hledger-ui ? h/w/s    # same as above
- hledger-web -> help   # served from hledger-web

## Config file

Name: hledger.conf (and possibly ~/.hledger.conf as well).

- easy to say and spell
- good highlighting support in editors

Format: toml/ini-ish format, but customised for our needs (if necessary).

Example:
```
# hledger.conf

[defaults]
# Set options/arguments to be always used with hledger commands.
# Each line is: HLEDGERCMD ARGS, or: hledger ARGS
hledger -f hledger.journal
bal -M --flat -b lastmonth
ui --watch
web -V
help --html

[commands]
# Define aliases for custom hledger commands.
# Each line is: CMDALIAS = HLEDGERCMD ARGS
assets = bal -M ^assets\b
liab   = bal -M ^liabilities\b

# Or use colon, like make ?
bs2:   bs --no-total date:thisyear

# Or just whitespace, like hledger csv rules ?
smui   ui ^sm\b

# Allow arbitrary shell commands ?
2019:    hledger -f 2019.journal
jstatus: git status -sb -- *.journal

# Allow multi-command shell scripts, with optional help string ?
bsis:
  "Show monthly balance sheet and income statement"
  hledger bs -M
  echo
  hledger is -M
  echo
```

Loaded: 

- at startup
and ideally:
- hledger-web: on each page load if changed, like journals
- hledger-ui --watch: on change, like journals

Location: 

Search a number of locations in order.
Values from multiple files are combined, with later files taking precedence.

User config file: should it be "modern"  ~/.config/hledger.conf or "old/simple" ~/.hledger.conf ? 
One or the other may be preferred/easier/more portable.
If we support both, should it be one or the other, or both ?

Parent directory config files: we'd probably like to recognise config files in parent directories.
How far up should we look - 
to the root dir ? 
to the user's home dir ? and if not under the user's home dir, don't look up at all ?
to the nearest VCS working directory root ?

This would be the simplest comprehensive scheme: use all of

1. ~/.config/hledger.conf
2. ~/.hledger.conf
3. hledger.conf in all directories from / down to the current directory

Eg: running hledger in /home/simon/project/finance would combine any of the following which exist:

- ~/.config/hledger.conf
- ~/.hledger.conf
- /hledger.conf
- /home/hledger.conf
- /home/simon/hledger.conf
- /home/simon/project/hledger.conf
- /home/simon/project/finance/hledger.conf

<style>
.wy-table-responsive { 
  overflow:visible;
}
</style>

