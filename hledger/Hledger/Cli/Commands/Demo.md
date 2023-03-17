## demo

Play small demos of hledger usage in the terminal.

_FLAGS

Run this command with no argument to list the demos.
To play a demo, write its number or name or a substring.
asciinema must be installed.

During playback, several keys are available:
- SPACE   pause/unpause
- .       step forward (while paused)
- CTRL-c  quit early

asciinema options can be added following a double-dash;
list them with `asciinema -h`. `-s` (speed) and 
`-i` (max idle time) are particularly useful.

Examples:
```shell
$ hledger demo                   # list available demos
$ hledger demo 1                 # play the first demo
$ hledger demo install -s5 -i.5  # play the demo named or containing "install",
                                 # at 5x speed, limiting idle time to 0.5s.
```
