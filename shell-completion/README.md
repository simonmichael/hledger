Shell completion for CLI
========================

This code generates shell completion scripts for hledger's command line
interface.
Shell completion is usually triggered by pressing the tab key once or twice
after typing the command `hledger `.
(The exact behavior may differ in shells other than Bash.)

Currently, only Bash is supported but Zsh or Fish can be added.

[Demonstration video](https://asciinema.org/a/PdV2PzIU9oDQg1K5FjAX9n3vL)

The completions can handle hledger's CLI:

- commands and generic options
- command-specific options
- account names from journal files (but not yet for files named by --file)
- filenames for options that take a filename as argument

Installation
------------

First, generate the completion script for Bash:

```
# change into this folder:
cd shell-completion/
make
```

Hint: GNU make, GNU m4, and GNU parallel must be installed to call `make`.
The first two usually are.

Then, the generated completion script must be installed. TBD.
For now, you can use these two commands:

```
cp hledger-completion.bash ~/.hledger-completion.bash
echo 'source ~/.hledger-completion.bash' >> ~/.bashrc
```

After that, you have to start a new Bash, e.g. by typing `bash` on the current
shell.

Now, try it by typing `hledger` (with a space after the command) and press the
tab key twice. Then you can type a part of one of the suggestions and press tab
again to complete it.

Background
----------

The Bash completion script is generated (GNU make) by parsing output of `hledger`,
`hledger -h`, and `hledger <cmd> -h`. The script also uses `hledger accounts` for
account name completion. I propose that the Makefile is not run at every built
but rather manually when the CLI changes.
