# Just, make, and Shake

After many years of relying on [GNU Make](https://www.gnu.org/software/make) for automating project tasks,
we now primarily use [just](https://github.com/casey/just) instead.
`just` is better enough, and clean consolidated efficient project automation is so valuable, that this is worthwhile -
even though `just` requires installing and lacks Make's file dependency tracking, for now.

`Justfile` in the main repo's top directory is the starting point for scripts automating hledger project tasks.
Install and run `just` to list them. I suggest using a shorter command alias `j`. Eg:

```cli
$ alias j=just
$ j             # list all scripts
$ j h test      # list scripts mentioning "test"
```

Makefiles are still used in a number of subdirectories, like `site/` and `finance/`.

## Shake

`Shake.hs` in the main repo's top directory complements the Justfile.
Tasks requiring file dependencies and/or more complex code, such as building documentation and the web site,
are usually scripted here, often with a corresponding alias in `Justfile`.
Eg:

```cli
$ ./Shake.hs   # compile it for speed and git branch independence. Or: just Shake
$ ./Shake      # list scripts
$ j site       # runs the `just site` which calls `./Shake site` to do the work
$ j -n site    # just --dry-run, show the commands that `just site` will run
```

## tools

Additional helper scripts and tools are kept in the `tools/` subdirectory.

