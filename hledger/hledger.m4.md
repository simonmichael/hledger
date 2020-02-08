% hledger(1) hledger _version_
% _author_
% _monthyear_

m4_dnl Quick hledger m4 intro:
m4_dnl  .m4.md are hledger docs source, processed with m4 to generate markdown.
m4_dnl  Lines beginning with m4_dnl are comments.
m4_dnl  Words enclosed in underscores are macros, defined in doc/common.m4.
m4_dnl  Macro arguments are enclosed in (). Text literals are enclosed in {{}}.
m4_dnl  Macros may depend on command line flags, configured in Shake.hs.
m4_dnl
m4_dnl This file includes others:
m4_dnl hledger.1.m4.md
m4_dnl  hledger_examples.m4.md
m4_dnl  hledger_options.m4.md
m4_dnl  hledger_commands.m4.md

m4_dnl Show these first headings only in man pages:
_man_({{
# NAME
}})

hledger - a command-line accounting tool

_man_({{
# SYNOPSIS
}})

`hledger [-f FILE] COMMAND [OPTIONS] [ARGS]`\
`hledger [-f FILE] ADDONCMD -- [OPTIONS] [ARGS]`\
`hledger`

_man_({{
# DESCRIPTION
}})

m4_dnl Include the standard description:
_hledgerdescription_

This is hledger’s command-line interface (there are also terminal and web
interfaces). Its basic function is to read a plain text file describing
financial transactions (in accounting terms, a general journal) and
print useful reports on standard output, or export them as CSV. hledger
can also read some other file formats such as CSV files, translating
them to journal format. Additionally, hledger lists other hledger-\* 
executables found in the user’s \$PATH and can invoke them as subcommands.

hledger reads _files_ 
If using `$LEDGER_FILE`, note this must be a real environment variable, 
not a shell variable.
You can specify standard input with `-f-`.

Transactions are dated movements of money between two (or more) named
accounts, and are recorded with journal entries like this:

m4_dnl Format as a journal snippet:
_journal_({{
2015/10/16 bought food
 expenses:food          $10
 assets:cash
}})

For more about this format, see hledger_journal(5).

Most users use a text editor to edit the journal, usually with an editor
mode such as ledger-mode for added convenience. hledger’s interactive
add command is another way to record new transactions. hledger never
changes existing transactions.

To get started, you can either save some entries like the above in
`~/.hledger.journal`, or run `hledger add` and follow the prompts. Then
try some commands like `hledger print` or `hledger balance`.
Run `hledger` with no arguments for a list of commands.
 
m4_dnl Include these subfiles:
_include_(hledger_examples.m4.md)
_include_(hledger_options.m4.md)
_include_(hledger_commands.m4.md)

# ENVIRONMENT

**COLUMNS**
The screen width used by the register command. 
Default: the full terminal width.

m4_dnl Standard LEDGER_FILE description:
_LEDGER_FILE_

# FILES

m4_dnl Standard input files description:
Reads _files_

# LIMITATIONS

The need to precede addon command options with `--` when invoked from hledger is awkward.

When input data contains non-ascii characters, a suitable system locale must be configured (or there will be an unhelpful error).
Eg on POSIX, set LANG to something other than C.

In a Microsoft Windows CMD window, non-ascii characters and colours are not supported.

On Windows, non-ascii characters may not display correctly when running a hledger built
in CMD in MSYS/CYGWIN, or vice-versa.

In a Cygwin/MSYS/Mintty window, the tab key is not supported in hledger add.

Not all of Ledger's journal file syntax is supported. See [file format differences](faq.html#file-format-differences).

On large data files, hledger is slower and uses more memory than Ledger.

# TROUBLESHOOTING

Here are some issues you might encounter when you run hledger
(and remember you can also seek help from the
[IRC channel](http://irc.hledger.org),
[mail list](http://list.hledger.org) or
[bug tracker](http://bugs.hledger.org)):

**Successfully installed, but "No command 'hledger' found"**  
stack and cabal install binaries into a special directory, which
should be added to your PATH environment variable.  Eg on unix-like
systems, that is ~/.local/bin and ~/.cabal/bin respectively.

**I set a custom LEDGER_FILE, but hledger is still using the default file**  
`LEDGER_FILE` should be a real environment variable, not just a shell variable.
The command `env | grep LEDGER_FILE` should show it.
You may need to use `export`. Here's an [explanation](http://stackoverflow.com/a/7411509).

**"Illegal byte sequence" or "Invalid or incomplete multibyte or wide character" errors**  
In order to handle non-ascii letters and symbols (like £), hledger needs
an appropriate locale. This is usually configured system-wide; you can
also configure it temporarily.  The locale may need to be one that
supports UTF-8, if you built hledger with GHC < 7.2 (or possibly always,
I'm not sure yet).

Here's an example of setting the locale temporarily, on ubuntu gnu/linux:

```shell
$ file my.journal
my.journal: UTF-8 Unicode text                 # <- the file is UTF8-encoded
$ locale -a
C
en_US.utf8                             # <- a UTF8-aware locale is available
POSIX
$ LANG=en_US.utf8 hledger -f my.journal print   # <- use it for this command
```

Here's one way to set it permanently, there are probably better ways:

```shell
$ echo "export LANG=en_US.UTF-8" >>~/.bash_profile
$ bash --login
```

If we preferred to use eg `fr_FR.utf8`, we might have to install that first:

```shell
$ apt-get install language-pack-fr
$ locale -a
C
en_US.utf8
fr_BE.utf8
fr_CA.utf8
fr_CH.utf8
fr_FR.utf8
fr_LU.utf8
POSIX
$ LANG=fr_FR.utf8 hledger -f my.journal print
```

Note some platforms allow variant locale spellings, but not all (ubuntu
accepts `fr_FR.UTF8`, mac osx requires exactly `fr_FR.UTF-8`).

