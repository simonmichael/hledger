## commodities

List the commodity symbols used or declared in the journal.

```flags
Flags:
     --used                 list commodities used
     --declared             list commodities declared
     --undeclared           list commodities used but not declared
     --unused               list commodities declared but not used
     --find                 list the first commodity matched by the first
                            argument (a case-insensitive infix regexp)
```


This command lists commodity symbols/names -
all of them by default,
or just the ones which have been used in transactions or `P` directives,
or declared with `commodity` directives,
or used but not declared,
or declared but not used,
or just the first one matched by a pattern (with `--find`, returning a non-zero exit code if it fails).

You can add `cur:` [query arguments](#queries) to further limit the commodities.
