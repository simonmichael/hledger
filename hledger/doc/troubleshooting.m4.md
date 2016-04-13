# TROUBLESHOOTING

## Run-time problems

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

```{.shell}
$ file my.journal
my.journal: UTF-8 Unicode text                 # <- the file is UTF8-encoded
$ locale -a
C
en_US.utf8                             # <- a UTF8-aware locale is available
POSIX
$ LANG=en_US.utf8 hledger -f my.journal print   # <- use it for this command
```

Here's one way to set it permanently, there are probably better ways:

```{.shell}
$ echo "export LANG=en_US.UTF-8" >>~/.bash_profile
$ bash --login
```

If we preferred to use eg `fr_FR.utf8`, we might have to install that first:

```{.shell}
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


## Known limitations

**Command line interface**

Add-on command options, unless they are also understood by the main
hledger executable, must be written after `--`, like this:
`hledger web -- --server`

**Differences from Ledger**

Not all of Ledger's journal file syntax is supported. See [file format differences](faq#file-format-differences).

hledger is slower than Ledger, and uses more memory, on large data files.

**Windows limitations**

In a windows CMD window, non-ascii characters and colours are not supported.

In a windows Cygwin/MSYS/Mintty window, the tab key is not supported in hledger add.
