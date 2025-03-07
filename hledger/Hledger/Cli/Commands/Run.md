## run 

Run a sequence of hledger commands, provided as files or command line arguments.
Data files are parsed just once, so the commands run faster.

```flags
Flags:
no command-specific flags
```

This command is experimental and could change in the future.

You can use `run` in three ways:

- `hledger run -- CMD1 -- CMD2 -- CMD3`  - read commands from the command line, separated by `--`
- `hledger run SCRIPTFILE1 SCRIPTFILE2`  - read commands from one or more files
- `cat SCRIPTFILE1 | hledger run`        - read commands from standard input.

`run` first loads the input file(s) specified by `LEDGER_FILE` or by `-f` options, in the usual way.
Then it runs each command in turn, each using the same input data.
But if you want a particular command to use different input, you can specify an `-f` option within that command.
This will override (not add to) the default input, just for that command.

Each input file (more precisely, each combination of input file and input options) is parsed only once.
This means that commands will not see any changes made to these files, until the next run.
But the commands will run more quickly than if run individually (typically about twice as fast).

Command scripts, whether in a file or written on the command line, have a simple syntax:

- each line may contain a single hledger command and its arguments, without the usual `hledger` first word
- empty lines are ignored
- text from `#` to end of line is a comment, and ignored
- you can use single or double quotes to quote arguments when needed, as on the command line
- these extra commands are available: `echo TEXT` prints some text, and `exit` or `quit` ends the run.

On unix systems you can use `#!/usr/bin/env hledger run` in the first line of a command file to make it a runnable script.
If that gives an error, use `#!/usr/bin/env -S hledger run`.

It's ok to use the `run` command recursively within a command script.

### Caveats

You may find some differences in behaviour between `run` command lines and normal hledger command lines.
For example, with `run`,

- the command name must be written first, options afterward
- full command names or official abbreviations (as in the command list) must be used

### Examples

Run commands specified on the command line:
```cli
hledger -f some.journal run -- balance assets --depth 2 -- balance liabilities -f /some/other.journal --depth 3 --transpose -- stats
```
This would load `some.journal`, run `balance assets --depth 2` on it, then run `balance liabilities --depth 3 --transpose` on `/some/other.journal`, and finally run `stats` on `some.journal`

Run commands from standard input:
```cli
(echo "files"; echo "stats") | hledger -f some.journal run
```

Provide commands as a runnable script:
```cli
#!/usr/bin/env -S hledger run -f some.journal

echo "List of accounts in some.journal"
accounts

echo "Assets of some.journal"
balance assets --depth 2

echo "Liabilities from /some/other.journal"
balance liabilities -f /some/other.journal --depth 3 --transpose

echo "Commands from another.script, applied to another.journal"
run -f another.journal another.script
```
