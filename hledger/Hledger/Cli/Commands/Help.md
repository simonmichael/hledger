## help

Show some part of hledger's documentation. The first argument selects what to show:

- `quickref` (or none): a quick reference / overview
- `commands`:           all hledger commands, including addons and aliases
- `usage [CMD]`:        command line options help (like -h/--help)
- `examples [CMD]`:     command line examples
- `manual [TOPIC]`:     the user manual, optionally at the TOPIC heading
- `TOPIC`               the user manual at TOPIC (like `manual TOPIC`)
- `install | relnotes | docs | support | home`: hledger.org pages, in a web browser

TOPIC is a section heading in the manual, or a prefix, matched case insensitively.
The manual will be shown in a default viewer (info, man, pager, web browser),
or you can choose with -i/-m/-p/-w.

```flags
Flags:
  -i                       when showing the manual, use info
  -m                       when showing the manual, use man
  -p                       when showing the manual, use $PAGER or less
  -w                       when showing the manual, use a web browser
     --builtin             when showing commands, show only built-in commands
```

The manual is built in to your hledger executable, so it can be useful when offline,
or when you prefer the terminal to a web browser,
or when the appropriate hledger manual or viewers are not installed properly on your system.

By default it chooses the best viewer found in $PATH, trying in this order:
`info`, `man`, `$PAGER`, `less`, `more`, stdout.
(If a TOPIC is specified, `$PAGER` and `more` are not tried.)
You can force the use of info, man, or a pager with the `-i`, `-m`, or `-p` flags.
If no viewer can be found, or if running non-interactively, it just prints the manual to stdout.

Examples
```cli
$ hledger help -h                 # show the help command's options
$ hledger help                    # show the quick reference
$ hledger help commands           # show the commands list
$ hledger help manual commands    # show the "Commands" section of the manual
$ hledger help 'time periods' -m  # show the "Time periods" section of the manual, using man
$ hledger help examples print     # show brief examples for the print command
```
