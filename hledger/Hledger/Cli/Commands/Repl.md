## repl

Runs hledger commands interactively.

This command is EXPERIMENTAL and could change in the future.

```flags
Flags:
no command-specific flags
```

This command starts a read-eval-print loop (REPL) where you can enter commands interactively. To exit REPL, use "exit" or "quit", or send EOF.

It could also accept commands from standard input, if you pipe commands into it.

The commands will run more quickly than if run individually, because the input files would be parsed only once.

Syntax of the commands is intentionally simple:
- each line is a single hledger command
- lines that can't be interpreted as hledger commands are printed out as-is
- empty lines are skipped
- everything after `#` is considered to be a comment and will be ignored, and will not be printed out
- `echo <text>` will print out text, even if it could be recognized as a hledger command 

You can use single or double quotes to quote aguments that need it ('like this' or "like this").

### Caveats:

- `Repl`, like any other command, will load the input file(s) (specified by `LEDGER_JOURNAL` or by `-f` arguments). The contents of those files would be used by all the commands that `repl` runs. If you want a particular command to use a different input file, you can use `-f` flag for that particular command. This will override (not add) the input for that particular command. All the input files would be cached, and would be read only once.

### Examples:

To start the REPL:
```cli
hledger repl
```
or
```cli
hledger repl -f some.journal
```

To pipe commands into REPL:
```cli
(echo "files"; echo "stats") | hledger repl -f some.journal
```
