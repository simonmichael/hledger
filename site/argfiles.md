# Save frequently used options

You can save frequently used options and arguments in an 
[argument file](manual#argument-files), one per
line, then reuse them via a @FILE argument on the command line.
(hledger 1.4+)

Here's an example.
I keep frequently-used options for quick daily reports in a file
called `simple.args`. The name can be anything; I use a `.args` suffix
so I can find these easily. Here's the content of `simple.args`:
```
--alias=/:(business|personal):/=:
--alias=/:(bank|cash|online):/=:
--alias=/:bofi:/=:b
--alias=/:unify:/=:u
--alias=/:wf:/=:w
-2
cur:.
```

The format is one command-line flag or command-line argument per line.
Now if I write `@simple.args` in a hledger command line, it will be replaced
by all of the above options/flags:

These options are just an example, but in case you're wondering:
they remove some detail, giving simplified reports which are easier for me to read at a glance.

- the aliases simplify the chart of accounts, hiding some distinctions (eg business vs. personal) and flattening some bank account names
- the `-2` depth flag limits account depth to 2, hiding deeper subaccounts
- the `cur:.` argument shows only single-character currencies, hiding a bunch of cluttersome commodities I don't want to see

## Usage

Generate a balance report showing the simplified accounts:
```shell
$ hledger bal @simple.args
```
Start a live-updating hledger-ui showing the simplified asset accounts only:
```shell
$ hledger-ui --watch @simple.args assets
```

Options in the arguments file can be overridden by similar options later on
the command line, in the usual way. Eg, to show just a little more account detail:
```shell
$ hledger bal @simple.args -3
```

## Quoting

Special characters in the arguments file may need quoted,
following the usual rules of your shell (bash, fish etc.), 
but they'll need one less level of quoting than on the
command line. I think:
```shell
$ hledger bal @simple.args
```
is equivalent to writing:
```shell
$ hledger bal "--alias=/:(business|personal):/=:" "--alias=/:(bank|cash|online):/=:" "--alias=/:bofi:/=:b" "--alias=/:unify:/=:u" "--alias=/:wf:/=:w" "-2" "cur:."
```
So in this example, using the bash shell, the `|` pipe character does 
not need to be quoted in the arguments file (and should not be). 

## Suppressing this feature

If you actually need to write an argument beginning with @, 
eg let's say you have an account pattern beginning with that character, 
you'll want a way to disable this feature.  On unix systems at least, 
you can do that by inserting a `--` argument first. Eg:
```
$ hledger bal @somewhere.com       # looks for additional arguments in the ./somewhere.com file
$ hledger bal -- @somewhere.com    # matches account names containing "@somewhere.com"
```

On windows, this might or might not require a hledger built with GHC 8.2+. 
(Let us know.)
