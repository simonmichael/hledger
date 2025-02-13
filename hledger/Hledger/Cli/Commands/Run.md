## run 

Runs a sequence of hledger commands on the same input file(s), either interactively or as a script.

This command is EXPERIMENTAL and syntax could change in the future.

```flags
Flags:
no command-specific flags
```

The commands will run more quickly than if run individually, because the input files would be parsed only once.

"run" has three ways of invocation:
- when invoked without arguments, it start a read-eval-print loop (REPL) where you can enter commands interactively. To exit REPL, use "exit" or "quit", or send EOF.

- when file names are given to "run", it will read commands from these files, in order.

- lastly, commands could be specified directly on the command line. All commands (including the very first one) should be preceded by argument "--"

Syntax of the commands (either in the file, or in REPL) is intentionally simple:
- each line is a single hledger command
- lines that can't be interpreted as hledger commands are printed out as-is
- empty lines are skipped
- everything after `#` is considered to be a comment and will be ignored, and will not be printed out
- `echo <text>` will print out text, even if it could be recognized as a hledger command 

You can use single quotes or double quotes to quote aguments that need quoting.

You can use `#!/usr/bin/env hledger run` in the first line of the file to make it a runnable script. If this complains about "binary `hledger run` not found", use `/usr/bin/env -S hledger run`.

### Caveats:

- If you meant to provide file name as an argument, but made a mistake and a gave file name that does not exist, "run" will attempt to interpret it as a command.

- Numeric flags like `-3` do not work, use long form `--depth 3`

### Examples:

To start the REPL:
```cli
hledger run
```

To provide commands on the command line, separate them with `--`:
```cli
hledger run -f some.journal -- balance assets --depth 2 -- balance liabilities --depth 3 --transpose
```

To provide commands in the file, as a runnable scripts:
```cli
#!/usr/bin/env -S hledger run
echo "List of accounts"
accounts

echo "Assets"
balance assets --depth 2

echo "Liabilities"
balance liabilities --depth 3 --transpose
```

