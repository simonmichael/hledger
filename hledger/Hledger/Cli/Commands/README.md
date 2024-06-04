hledger's built-in commands.

Each command has a similarly-named code module, Somecommand.hs, and
documentation file, Somecommand.md.

The command doc is converted to plain text in Somecommand.txt, and
which is included by Somecommand.hs to form command line help (the
output of `hledger COMMAND --help`).

After changing md files, regenerating and committing the txt files is
optional. If you don't do it, it will get done later (before release).
It can be done by:

    ./Shake cmdhelp

Or, by you can regenerate them while also building packages:

    ./Shake hledger   # or, all packages: ./Shake build

Builds made with stack, cabal, etc. won't notice changes in these md
files, so use the above method if you need that.

The md files are also included by hledger/hledger_commands.m4.md to
form the hledger manual, which can be regenerated with:

    ./Shake manuals [website]

Here are more special features/conventions of command doc files (see
*.md for examples):

- The content is pandoc markdown. m4 macros are not supported.

- The format is "hledger command help". Basically there should be
  a command name and blank line,
  an optional parenthesised command alias/abbreviation and blank line,
  a short help preamble,
  a code block with `flags` class containing a `Flags:` line
  then the command-specific flags help output (or "none"),
  and an optional longer help postamble.
  See parseCommandHelp in ../CliOptions.hs for the exact format.

- In manuals, these are rendered with formatting and hyperlinks in
  output formats which support those.

- In --help output, they are rendered

  - as plain text
  - with blank lines removed from the preamble
  - with lines longer than 78 characters wrapped (unless they contain no spaces)
  - with code blocks unindented
  - with the `flags` code block replaced by dynamically generated flags help
    (for both command-specific and general flags)

The postamble often ends with one or more examples.
To avoid unsightly line wrapping in command line help, try to keep
code blocks to at most 78 characters wide. When necessary, we may be
forced to cheat and alter command output slightly, eg reducing the
width of register's typically 79-wide reports by one.
