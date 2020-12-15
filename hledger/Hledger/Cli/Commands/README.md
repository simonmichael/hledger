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

- In manuals, they are rendered with formatting and hyperlinks when
  supported by the various output formats.

- In command line help, they are rendered as plain text, with verbatim
  blocks unindented, and with lines longer than 78 characters wrapped
  (unless they contain no spaces).

- To avoid unsightly line wrapping in command line help, try to keep
  verbatim examples to at most 78 characters wide. When necessary, we
  may be forced to cheat and alter command output slightly, eg
  reducing the width of register's typically 79-wide reports by one.

- The first line should specify the command name and any aliases, as
  words separated by spaces and/or commas. It should end with a
  backslash (to force a line break in the output).

- Short help, about one paragraph in length, should begin on the
  second line.

- If the short help is more than one line, its first line should end
  with a single newline, not two (otherwise it will be displayed with
  two newlines after each line in command line help).

- In command line help, the short help is displayed with blank lines
  removed (paragraphs run together). Blank lines can still be used for
  markdown formatting though, eg to define a list.

- After the short help, there should be a paragraph containing just
  _FLAGS. This marks the end of the short help, and it will be
  replaced in command line help by the flags list. (Without it, the
  flags list appears at the end of command line help.) The flags list
  will not appear in the hledger manual.

- Long help (as many paragraphs as needed) follows the _FLAGS marker.
  This often ends with one or more examples.


XXX Command docs with examples wider than 78:
close
rewrite

