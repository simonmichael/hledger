% hledger(1) hledger 0.27.98
%
% April 2016

<div class="web">
_versions_({{hledger}})

* toc
</div>
<div class="man">

# NAME

hledger - a command-line accounting tool

# SYNOPSIS

`hledger [-f FILE] COMMAND [OPTIONS] [CMDARGS]`\
`hledger [-f FILE] ADDONCMD -- [OPTIONS] [CMDARGS]`

# DESCRIPTION

hledger is a cross-platform program for tracking money, time, or any
other commodity, using double-entry accounting and a simple, editable
file format. It is inspired by and largely compatible with ledger(1).
Tested on unix, mac, windows, hledger aims to be a reliable, practical
tool for daily use.

</div>

_include_(description.m4.md)
_include_(examples.m4.md)
_include_(options.m4.md)
_include_(queries.m4.md)
_include_(commands.m4.md)
_include_(troubleshooting.m4.md)

<div class="man">

# ENVIRONMENT

**LEDGER_FILE**
sets the default journal file path. If not set, it is `~/.hledger.journal`.

**COLUMNS**
sets the default width used by the register command (normally the full terminal width).

# FILES

Reads data from a hledger journal file (`$LEDGER_FILE` or
`~/.hledger.journal` by default), or a CSV file plus associated CSV
rules file.

# BUGS

The need to precede options with `--` when invoked from hledger is awkward.

hledger can't render non-ascii characters when run from a Windows command prompt (up to Windows 7 at least).

When input data contains non-ascii characters, a suitable system locale must be configured (or there will be an unhelpful error).
Eg on POSIX, set LANG to something other than C.

</div>
