# Just, make, and Shake

After many years of relying on `make` for automating project tasks,
we now primarily use [`just`](https://github.com/casey/just) instead.
`just` is better enough, and clean consolidated efficient project automation is so valuable,
that it is worthwhile, even though, for now, it is usually not installed by default and it lacks Make's file dependency tracking.

`Justfile` in the main repo's top directory is where we keep the recipes (scripts);
this is the starting point for automating hledger project tasks.
Install and run `just` to see the list of recipes. I suggest using a shorter command alias `j`.
Eg:

```cli
$ alias j=just
$ j             # list all scripts
$ j h test      # list scripts mentioning "test"
```

## Shake

`Shake.hs` in the main repo's top directory complements the Justfile.
Tasks requiring file dependencies and/or more complex code, such as building documentation and the web site,
are usually scripted here, often with a corresponding alias in `Justfile`.
Eg:

```cli
$ ./Shake.hs   # compile it for speed and git branch independence. Or: just Shake
$ ./Shake      # list scripts
$ j site       # runs the `site` script in `Justfile` which calls the `site` script in `./Shake.hs` to do most of the work.
$ j -n site    # just --dry-run, show the commands that `just site` will run
```

## tools

Additional helper scripts and tools are kept in the `tools/` subdirectory.

## Make

[GNU Make](https://www.gnu.org/software/make) makefiles are still used in a number of subdirectories - site, finance etc.

