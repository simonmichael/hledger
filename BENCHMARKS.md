# Benchmarks

<div class=pagetoc>

<!-- toc -->
</div>

Benchmarks are standard performance measurements,
which we define using `bench` declarations in cabal files.
There is [one in hledger.cabal](https://github.com/simonmichael/hledger/blob/master/hledger/hledger.cabal#L228),
with related code and data files in [hledger/bench/](https://github.com/simonmichael/hledger/tree/master/hledger/bench).

To run the standard hledger benchmark, use `stack bench hledger`.
This installs haskell dependencies (but not system dependencies) and rebuilds as needed,
then runs [hledger/bench/bench.hs](https://github.com/simonmichael/hledger/blob/master/hledger/bench/bench.hs),
which by default shows quick elapsed-time measurements for several operations on a standard data file:

```shell
$ stack bench hledger
NOTE: the bench command is functionally equivalent to 'build --bench'
...
hledger-0.27: benchmarks
Running 1 benchmarks...
Benchmark bench: RUNNING...
Benchmarking hledger in /Users/simon/src/hledger/hledger with timeit
read bench/10000x1000x10.journal        [1.57s]
print                                   [1.29s]
register                                [1.92s]
balance                                 [0.21s]
stats                                   [0.23s]
Total: 5.22s
Benchmark bench: FINISH
```

bench.hs has some other modes, which you can use by compiling and running it directly.
`--criterion` reports more detailed and dependable measurements, but takes longer:

```shell
$ cd hledger; stack exec -- ghc -ibench bench/bench && bench/bench --criterion
...
Linking bench/bench ...
Benchmarking hledger in /Users/simon/src/hledger/hledger with criterion
benchmarking read bench/10000x1000x10.journal
time                 1.414 s    (1.234 s .. 1.674 s)
                     0.996 R²   (0.989 R² .. 1.000 R²)
mean                 1.461 s    (1.422 s .. 1.497 s)
std dev              59.69 ms   (0.0 s .. 62.16 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking print
time                 1.323 s    (1.279 s .. 1.385 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.305 s    (1.285 s .. 1.316 s)
std dev              17.20 ms   (0.0 s .. 19.14 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking register
time                 1.995 s    (1.883 s .. 2.146 s)
                     0.999 R²   (0.998 R² .. NaN R²)
mean                 1.978 s    (1.951 s .. 1.995 s)
std dev              25.09 ms   (0.0 s .. 28.26 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking balance
time                 251.3 ms   (237.6 ms .. 272.4 ms)
                     0.998 R²   (0.997 R² .. 1.000 R²)
mean                 260.4 ms   (254.3 ms .. 266.5 ms)
std dev              7.609 ms   (3.192 ms .. 9.638 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking stats
time                 325.5 ms   (299.1 ms .. 347.2 ms)
                     0.997 R²   (0.985 R² .. 1.000 R²)
mean                 329.2 ms   (321.5 ms .. 339.6 ms)
std dev              11.08 ms   (2.646 ms .. 14.82 ms)
variance introduced by outliers: 16% (moderately inflated)
```

`--simplebench` shows a table of elapsed-time measurements for the commands defined in [bench/default.bench](https://github.com/simonmichael/hledger/blob/master/hledger/bench/default.bench).
It can also show the results for multiple h/ledger executables side by side, if you tweak the bench.hs code.
Unlike the other modes, it does not link with the hledger code directly, but runs the "hledger" executable found in $PATH (so ensure that's the one you intend to test).

```shell
$ cd hledger; stack exec -- ghc -ibench bench/bench && bench/bench --simplebench
Benchmarking /Users/simon/.local/bin/hledger in /Users/simon/src/hledger/hledger with simplebench and shell
Using bench/default.bench
Running 4 tests 1 times with 1 executables at 2015-08-23 16:58:59.128112 UTC:
1: hledger -f bench/10000x1000x10.journal print	[3.27s]
1: hledger -f bench/10000x1000x10.journal register	[3.65s]
1: hledger -f bench/10000x1000x10.journal balance	[2.06s]
1: hledger -f bench/10000x1000x10.journal stats	[2.13s]

Summary (best iteration):

+-----------------------------------------++---------+
|                                         || hledger |
+=========================================++=========+
| -f bench/10000x1000x10.journal print    ||    3.27 |
| -f bench/10000x1000x10.journal register ||    3.65 |
| -f bench/10000x1000x10.journal balance  ||    2.06 |
| -f bench/10000x1000x10.journal stats    ||    2.13 |
+-----------------------------------------++---------+
```

bench's --simplebench mode is based on a standalone tool, [tools/simplebench.hs](https://github.com/simonmichael/hledger/blob/master/tools/simplebench.hs).
simplebench.hs is a generic benchmarker of one or more executables (specified on the command line) against one or more sets of command-line arguments (specified in a file).
It has a better command-line interface than bench.hs, so you may find it more convenient
for comparing multiple hledger versions, or hledger and ledger. Eg:

```shell
$ stack exec -- ghc tools/simplebench
[1 of 1] Compiling Main             ( tools/simplebench.hs, tools/simplebench.o )
Linking tools/simplebench ...
```
```shell
$ tools/simplebench -h
tools/simplebench -h
simplebench: at least one executable needed
bench [-f testsfile] [-n iterations] [-p precision] executable1 [executable2 ...]

Run some functional tests with each of the specified executables,
where a test is "zero or more arguments supported by all executables",
and report the best execution times.

  -f testsfile   --testsfile=testsfile    file containing tests, one per line, default: bench.tests
  -n iterations  --iterations=iterations  number of test iterations to run, default: 2
  -p precision   --precision=precision    show times with this precision, default: 2
  -v             --verbose                show intermediate results
  -h             --help                   show this help

Tips:
- executables may have arguments if enclosed in quotes
- tests can be commented out with #
- results are saved in benchresults.{html,txt}
```
```shell
cd hledger; $ ../tools/simplebench -f bench/default.bench hledger ledger
Using bench/default.bench
Running 4 tests 2 times with 2 executables at 2015-08-24 04:24:37.257068 UTC:

Summary (best iteration):

+-----------------------------------------++---------+--------+
|                                         || hledger | ledger |
+=========================================++=========+========+
| -f bench/10000x1000x10.journal print    ||    3.24 |   0.43 |
| -f bench/10000x1000x10.journal register ||    3.80 |   3.48 |
| -f bench/10000x1000x10.journal balance  ||    2.05 |   0.18 |
| -f bench/10000x1000x10.journal stats    ||    2.10 |   0.19 |
+-----------------------------------------++---------+--------+
```

Finally, for quick, fine-grained performance measurements when troubleshooting or optimising, I use
[dev.hs](https://github.com/simonmichael/hledger/blob/master/dev.hs).



