help\
Show the hledger user manual in one of several formats.

_FLAGS

This command shows the user manual built in to this hledger version,
using the best viewer it can find.
It can be useful if the correct version of the hledger manual,
or the usual viewing tools, are not installed on your system.

It will use the first of these viewers that it finds in $PATH: 
`info`, `man`, $PAGER, `less`, or stdout.
When run non-interactively, it always uses stdout.
Or you can force a particular viewer with the 
`--info/-i`, `--man/-m`, or `--pager/-p` flags.

