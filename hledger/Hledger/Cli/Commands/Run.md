## run 

Runs a sequnce of hledger commands on the same input file(s), either interactively or as a script.

```flags
Flags:
no command-specific flags
```

The commands will run more quickly than if run individually, because the input files would be parsed only once.

When file names are given to "run", it will read commands from these files, in order.

With no arguments, "run" will start a read-eval-print loop (REPL)
where you can enter commands interactively. To exit REPL, use "exit"
or "quit", or send EOF.

Syntax of the commands (either in the file, or in REPL) is intentionally simple:
- each line is a single hledger command
- lines that can't be interpreted as hledger commands are printed out as-is
- empty lines are skipped
- everything after `#` is considered to be a comment and will be ignored, and will not be printed out
- `echo <text>` will print out text, even if it could be recognized as a hledger command 

You can use single quotes or double quotes to quote aguments that need quoting.

You can use `#!/usr/bin/env hledger run` in the first line of the file to make it a runnable script.

For example:

```cli
#!/usr/bin/env hledger run
echo "List of accounts"
accounts

echo "Assets"
balance assets --depth 2

echo "Liabilities"
balance liabilities --depth 3 --transpose
```

