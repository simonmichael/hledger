## run 

Runs a sequence of hledger commands on the same input file(s), taking them from the command line or from file(s).

This command is EXPERIMENTAL and syntax could change in the future.

```flags
Flags:
no command-specific flags
```

The commands will run more quickly than if run individually, because the input files would be parsed only once.

"run" has two ways of invocation:

- when all positional arguments of "run" are valid file names, "run" will read commands from these files, in order: `run -f some.journal file1.txt file2.txt file3.txt`.

- commands could be specified directly on the command line. All commands (including the very first one) should be preceded by argument "--": `run -f some.journal -- cmd1 -- cmd2 -- cmd3`.

Syntax of the command is intentionally simple:
- each line read from a file is a single hledger command
- lines that can't be interpreted as hledger commands are printed out as-is
- empty lines are skipped
- everything after `#` is considered to be a comment and will be ignored, and will not be printed out
- `echo <text>` will print out text, even if it could be recognized as a hledger command 
- `run` is a valid command to use as well, so you can have `run` call `run` if you want to.

You can use single or double quotes to quote aguments that need it ('like this' or "like this").

You can use `#!/usr/bin/env hledger run` in the first line of the file to make it a runnable script. If this complains about "binary `hledger run` not found", use `/usr/bin/env -S hledger run`.

### Caveats:

- If you meant to provide file name as an argument, but made a mistake and a gave file name that does not exist, "run" will attempt to interpret it as a command.

- `Run`, like any other command, will load the input file(s) (specified by `LEDGER_JOURNAL` or by `-f` arguments). The contents of those files would be used by all the commands that `run` runs. If you want a particular command to use a different input file, you can use `-f` flag for that particular command. This will override (not add) the input for that particular command. All the input files would be cached, and would be read only once.

### Examples:

To provide commands on the command line, separate them with `--`:
```cli
hledger run -f some.journal -- balance assets --depth 2 -- balance liabilities -f /some/other.journal --depth 3 --transpose -- stats
```
This would load `some.journal`, run `balance assets --depth 2` on it, then run `balance liabilities --depth 3 --transpose` on `/some/other.journal`, and finally will run `stats` on `some.journal`

To provide commands in the file, as a runnable scripts:
```cli
#!/usr/bin/env -S hledger run -f some.journal
echo "List of accounts in some.journal"
accounts

echo "Assets of some.journal"
balance assets --depth 2

echo "Liabilities from /some/other.journal"
balance liabilities -f /some/other.journal --depth 3 --transpose

echo "Commands from anoter.script, applied to another.journal"
run -f anoter.journal another.script
```

