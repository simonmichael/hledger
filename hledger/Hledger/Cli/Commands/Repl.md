## repl

Start an interactive prompt for running hledger commands.
Input files are reloaded only when they change, so commands run faster.

```flags
Flags:
no command-specific flags
```

This command is experimental and could change in the future.

`hledger repl` starts a read-eval-print loop (REPL) where you can enter commands interactively.
As with the `run` command, each input file (or each input file/input options combination) is parsed just once,
so commands will run more quickly than if you ran them individually at the command line.

Also like `run`, the input file(s) specified for the `repl` command will be the default input for all interactive commands.
You can override this temporarily by specifying an `-f` option in particular commands.

Before running a command, any input files which have changed on disk are automatically reloaded.
Also command aliases are reloaded if the config file has changed,
and addon commands are re-detected if PATH's contents have changed.

Any other general flags given to `repl` - input, reporting, or display flags such as
`-I`, `--strict`, `--alias`, `-b`/`-e`, `--depth`, `--cost`, `--value`, `--color` -
are also applied to every command you run.
A command can still override them by specifying its own flags.

The command syntax is the same as with `run`:

- enter one hledger command at a time, without the usual `hledger` first word
- empty lines and comment text from `#` to end of line are ignored
- use single or double quotes to quote arguments when needed
- type `exit` or `quit` or control-D to exit the REPL.

While it is running, the REPL remembers your command history, and you can navigate in the usual ways:

- Keypad or Emacs navigation keys to edit the current command line
- UP/DOWN or control-P/control-N to step back/forward through history
- control-R to search for a past command
- TAB to complete file paths.

Generally `repl` command lines should feel much like the normal hledger CLI, but you may find differences.
`repl` is a little stricter;
eg it requires full command names or official abbreviations (as seen in the commands list).
Command aliases defined in a config file can also be used.

The `commands` and `help` commands, and the command help flags
(`CMD --tldr`, `CMD -h/--help`, `CMD --info`, `CMD --man`), can be useful.

You can type control-C to cancel a long-running command (but only once; typing it a second time will exit the REPL).

To run a single shell command without leaving the REPL, type `! SHELLCMD`
(eg `! ls` or `! git status`).

And in most shells you can type control-Z to temporarily exit to the shell (and then `fg` to return to the REPL).

### Examples

Start the REPL and enter some commands:
```cli
$ hledger repl 
Enter hledger commands. To exit, enter 'quit' or 'exit', or send EOF.
2025> stats
Main file           : .../2025.journal
...
2025> stats -f 2024/2024.journal 
Main file           : .../2024.journal
...
2025> stats
Main file           : .../2025.journal
...
```

or:
```cli
$ hledger repl -f some.journal
Enter hledger commands. To exit, enter 'quit' or 'exit', or send EOF.
some> bs
...
some> print -b 'last week'
...
some> bs -f other.journal
...
```
