## repl

Start an interactive prompt, where you can run any of hledger's commands.
Data files are parsed just once, so the commands run faster.

```flags
Flags:
no command-specific flags
```

This command is experimental and could change in the future.

`hledger repl` starts a read-eval-print loop (REPL) where you can enter commands interactively.
As with the `run` command, each input file (or each input file/input options combination) is parsed just once,
so commands will run more quickly than if you ran them individually at the command line.

Also like `run`, the input file(s) specified for the `repl` command will be the default input for all interactive commands,
you can override this temporarily by specifying an `-f` option in particular commands,
and commands will not see any changes made to input files (eg by `add`) until you exit and restart the REPL.

The command syntax is the same as with `run`:

- enter one hledger command at a time, without the usual `hledger` first word
- empty lines and comment text from `#` to end of line are ignored
- use single or double quotes to quote arguments when needed
- type `exit` or `quit` or control-D to exit the REPL.

While it is running, the REPL remembers your command history, and you can navigate in the usual ways:

- Keypad or Emacs navigation keys to edit the current command line
- UP/DOWN or control-P/control-N to step back/forward through history
- control-R to search for a past command, etc.

The `commands` and `help` commands, and the command help flags (`CMD --tldr`, `CMD -h/--help`, `CMD --info`, `CMD --man`),
work in the usual way, and can be useful.

You can type control-C to cancel a long-running command (but only once; typing it a second time will exit the REPL).

And in most shells you can type control-Z to exit temporarily to the shell (and `fg` to return to the REPL).

You may find some differences in behaviour between `run` command lines and normal hledger command lines.
For example, in the REPL,

- the command name must be written first, options afterward
- full command names or official abbreviations (as in the command list) must be used
- options parsing with addon commands might be less flexible than the CLI
- the `stats` command gives false timings, currently

### Examples

Start the REPL and enter some commands:
```cli
$ hledger repl 
Enter hledger commands. To exit, enter 'quit' or 'exit', or send EOF.
% stats
Main file           : .../2025.journal
...
% stats -f 2024/2024.journal 
Main file           : .../2024.journal
...
% stats
Main file           : .../2025.journal
...
```

or:
```cli
$ hledger repl -f some.journal
Enter hledger commands. To exit, enter 'quit' or 'exit', or send EOF.
% bs
...
% print -b 'last week'
...
% bs -f other.journal
...
```
