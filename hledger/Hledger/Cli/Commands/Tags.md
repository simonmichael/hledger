## tags

List the tag names used or declared in the journal, or their values.
<!-- This section has the same name as Journal > Tags; 
     if reordering this and that, update all #tags[-1] links -->

```flags
Flags:
     --used                 list tags used
     --declared             list tags declared
     --undeclared           list tags used but not declared
     --unused               list tags declared but not used
     --values               list tag values instead of tag names
     --parsed               show them in the order they were parsed (mostly),
                            including duplicates
```

This command lists tag names - all of them by default,
or just the ones which have been used on transactions/postings/accounts,
or declared with `tag` directives,
or used but not declared,
or declared but not used.

You can add one TAGREGEX argument, to show only tags whose name is
matched by this case-insensitive, infix-matching regular expression.

After that, you can add query arguments to filter the 
transactions, postings, or accounts providing tags.

With `--values`, the tags' unique non-empty values are listed instead.

With `-E`/`--empty`, blank/empty values are also shown.

With `--parsed`, tags or values are shown in the order they were parsed, with duplicates included.
(Except, tags from account declarations are always shown first.)

Remember that accounts also acquire tags from their parents;
postings also acquire tags from their account and transaction;
and transactions also acquire tags from their postings.
