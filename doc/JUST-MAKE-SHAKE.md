# Just, make, and Shake

After many years of using on GNU Make for automating project tasks,
we now primarily use `just` <https://github.com/casey/just> instead.
`just` is better enough, and clean consolidated efficient project automation is so valuable,
that it is worthwhile, even though, for now, it is usually not installed by default and it lacks Make's file dependency tracking.
When dependency tracking, or more powerful code, is really needed, we typically use [[SHAKE|Shake.hs]] instead of make or just.

`Justfile` in the main repo's top directory is where we keep the recipes (scripts);
this is the starting point for automating hledger project tasks.
Install and run `just` to see the list of recipes. I suggest using a shorter command alias `j`.
Eg:

```cli
$ alias j=just
$ j             # list all scripts
$ j h test      # list scripts mentioning "test"
```

## Make

[GNU Make](https://www.gnu.org/software/make) makefiles are still used in a number of subdirectories - site, finance etc.

## Shake

`Shake.hs` in the main repo's top directory complements the Justfile; 
it is used for tasks requiring file dependencies or more complex code, such as building documentation and the web site.
Eg:

```cli
$ ./Shake.hs   # compile it for speed and git branch independence. Or: just Shake
$ ./Shake      # list scripts
```

## tools

Additional helper scripts and tools can be found in the `tools/` subdirectory.
