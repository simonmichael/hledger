## help

Show some part of hledger's documentation. The first argument selects what to show:

- `quickref` (or none): a quick reference / overview
- `commands`:           all hledger commands, including addons and aliases
- `usage [CMD]`:        command line options help (like -h/--help)
- `examples [CMD]`:     command line examples
- `manual [TOPIC]`:     list the manual's topics, or show it at the TOPIC heading
- `TOPIC`               the user manual at TOPIC (like `manual TOPIC`)
- `install | relnotes | docs | support | home`: hledger.org pages, in a web browser

TOPIC is a section heading in the hledger, hledger-ui or hledger-web manual,
or part of one, matched case insensitively.
Enclose it in quotes if it contains spaces.
With no TOPIC (`hledger help manual`), all manual topics are listed.
A section name that appears in more than one manual is given a "-ui" or "-web"
suffix in the hledger-ui/hledger-web manuals (eg `options-ui`); these suffixed
names are matchable and appear in the topic list.
The manual will be shown in a default viewer (info, man, pager, web browser),
or you can choose with -i/-m/-p/-w.

```flags
Flags:
  -i                       show the manual with info
  -m                       show the manual with man
  -p                       show the manual with $PAGER or less
                           (less is always used if TOPIC is specified)
  -w                       show the manual on the web
  -l                       just list the manual topics matching TOPIC
     --builtin             with the commands topic, show only built-in
                           commands
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
$ hledger help                    # show the quick reference
$ hledger help -h                 # show the help command's options
$ hledger help commands           # list all commands
$ hledger help 'time periods'     # show the "Time periods" section in the manual
$ hledger help keys               # show the "KEYS" section in the hledger-ui manual
$ hledger help -l                 # list the manual's topics
$ hledger help -l journal         # list the manual topics matching "journal"
$ hledger help examples add       # show examples for the add command
```
