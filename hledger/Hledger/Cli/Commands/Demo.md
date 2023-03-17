## demo

Play small demos of hledger usage in the terminal.

_FLAGS

Run this command with no argument to list the demos.
To play a demo, write its number or a prefix or substring of its title.
asciinema must be installed.

During playback, several keys are available:
- SPACE   pause/unpause
- .       step forward (while paused)
- CTRL-c  quit early

asciinema options can be added following a double dash, such as
`-s` (adjust speed) and `-i` (limit idle time).
Run `asciinema -h` to list these.

Examples:
```shell
$ hledger demo                   # list available demos
$ hledger demo 1                 # play the first demo
$ hledger demo install -s5 -i.5  # play the install demo at 5x speed,
                                 # with pauses limited to half a second.
```
