# BACKLOG

An efficient public store of tasks/changes/design notes, mostly from SM's private backlog.
Things that I/we feel would be nice to have, or investigate further.
All help is welcome.
Some items are out of date and just need to be re-tested and discarded.
See also:
[TODO](TODO.md).

## cli

### help: clarify/improve the various kinds of command line help

#### survey/describe

Most of these are long, and best viewed with a pager, eg: hledger -h |
less

##### Quick command line help:

``` example
hledger     -h|--help
hledger-ui  -h|--help
hledger-web -h|--help
 General usage and command line flags for the main hledger UIs.

hledger
 hledger CLI's commands list.

hledger COMMAND -h|--help
 COMMAND's usage, flags, and manual section.

```

##### User manuals:

``` example
hledger help [-i|-m|-p] [TOPIC]
 View hledger user manual with info, man or pager, positioned at TOPIC.
 Good for jumping to a known (or guessed) topic.
 Good for viewing the manual if you don't have info or man.
 This is the main manual, covering the CLI, file formats, and concepts.

hledger --info
hledger-ui --info
hledger-web --info
 hledger, hledger-ui or hledger-web's builtin user manual, viewed with info.
 Best for exploring the topic tree.

hledger --man
hledger-ui --man
hledger-web --man
 hledger, hledger-ui or hledger-web's builtin user manual, viewed with man.
 Quick and searchable. 

info hledger, man hledger
info hledger-ui, man hledger-ui
info hledger-web, man hledger-web
 hledger, hledger-ui, or hledger-web installed user manuals.
 Same as above, if the manuals are properly installed on your system.

https://hledger.org/hledger.html
https://hledger.org/hledger-ui.html
https://hledger.org/hledger-web.html
 hledger, hledger-ui and hledger-web manuals, viewed on the web.
 Best for comfortable viewing and navigation when online.

```

### help improvements

#### auto pager

#### colour

#### show commands to get more detailed help

#### list output formats

#### list input formats

#### list runtime-detected input formats

#### more intro/manual in the main --help

#### commands list updates

Financial reports:
aregister (areg) show an account's transactions and running balance
register (reg) show postings in all/matched accounts and their total
balancesheet (bs) show assets, liabilities and net worth
balancesheetequity (bse) show assets, liabilities and equity
cashflow (cf) show changes in liquid assets
incomestatement (is) show revenues and expenses
balance (bal) show custom balance reports, budgets, gains..
roi show a return on investment report

#### --pivot: list possible arguments in help

### --version: include latest commit date

### --version: include git branch name if not master ?

### disable colors when writing to .txt file ?

### trailing colon makes alias ineffective

`--alias expenses:personal:=expenses:`

### unclear error if a trailing slash is written after file name

shell completion might wrongly add a trailing slash, eg as with this
symlink,
resulting in this unclear error:
~/notes$ stats -f current.journal/
hledger: SourcePos {sourceName =
"/Users/simon/.sm/notes/current.journal/", sourceLine = Pos 9,
sourceColumn = Pos 1} reading
/Users/simon/.sm/notes/current.journal/2018.prices:
/Users/simon/current.journal/2018.prices: openFile: inappropriate type
(Not a directory)

### -h/--help should work even with unrecognised flags/missing args

#### hledger --help ui --watch

#### hledger ui --watch --help

#### hledger --nosuchflag --help

#### hledger nosuchcommand --help

##### not expected to work; hledger --help nosuchcommand does work

##### unify hledger --help & hledger COMMAND --help more ?

### @ does not expand file names, eg @~/somefile

### bad --width parse error

$ hledger -f examples/sample.journal reg -w 80,-1
hledger: could not parse width option: ParseErrorBundle {bundleErrors =
TrivialError 3 (Just (Tokens ('-' :| ""))) (fromList [Label ('d'
:| "igit")]) :| [], bundlePosState = PosState {pstateInput =
"80,-1", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName =
"(unknown)", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth
= Pos 8, pstateLinePrefix = ""}} (use -h to see usage)

### balance assertion error shows ugly transaction

hledger: balance assertion error in "/Users/simon/notes/2018.journal"
(line 4949, column 54):
in transaction:
Transaction {tindex = 976, tsourcepos = JournalSourcePos
"/Users/simon/notes/2018.journal" (4948,4950), tdate = 2018-09-01,
tdate2 = Nothing, tstatus = *, tcode = "5VN72122C99690620",
tdescription = "add funds to paypal", tcomment = "for:,
time:06:53:51, type:Bank Deposit to PP Account , status:Pending,
balance:6.99, gross:6.99, fee:0.00\n", ttags =
[("for",""),("time","06:53:51"),("type","Bank Deposit to PP
Account"),("status","Pending"),("balance","6.99"),("gross","6.99"),("fee","0.00")],
tpostings = [PostingPP {pdate="Nothing", pdate2="Nothing",
pstatus="", paccount="assets:personal:online:paypal", pamount=Mixed
[Amount {acommodity =
"$", aquantity = 6.99, aprice = NoPrice, astyle = AmountStylePP "L False 2 Just '.' Nothing..", amultiplier = False}], pcomment="", ptype=RegularPosting, ptags=[], pbalanceassertion=Just (Amount {acommodity = "$",
aquantity = 6.99, aprice = NoPrice, astyle = AmountStylePP "L False 2
Just '.' Nothing..", amultiplier = False},GenericSourcePos
"/Users/simon/notes/2018.journal" 4949 54), ptransaction=Just
"<txn>", porigin=Nothing},PostingPP {pdate="Nothing",
pdate2="Nothing", pstatus="",
paccount="assets:personal:bank:wf:checking", pamount=Mixed [Amount
{acommodity = "$", aquantity = -6.99, aprice = NoPrice, astyle =
AmountStylePP "L False 2 Just '.' Nothing..", amultiplier =
False}], pcomment="", ptype=RegularPosting, ptags=[],
pbalanceassertion=Nothing, ptransaction=Just "<txn>",
porigin=Nothing}], tpreceding~commentlines~ = ""}
after posting:
assets:personal:online:paypal $6.99
balance assertion details:
date: 2018/09/01
account: assets:personal:online:paypal
commodity: $
calculated: $-93.01
asserted: $6.99 (difference: +$100.00)

### color support like stack's (still a few things remaining)

#### <http://no-color.org/> color should not be added by default if the `NO~COLOR~` environment variable is present.

#### Existing global option `--color=WHEN` is now also available as a

non-project-specific yaml configuration parameter `color:`.

#### Adopt the standard proposed at <http://no-color.org/>, that color should

not be
added by default if the `NO~COLOR~` environment variable is present.

#### New command `stack ls stack-colors` lists the styles and the associated

'ANSI'
control character sequences that stack uses to color some of its
output.
See
`stack ls stack-colors --help` for more information.

#### New global option `--stack-colors=STYLES`, also available as a

non-project-specific yaml configuration parameter, allows a stack user
to
redefine the default styles that stack uses to color some of its
output.
See
`stack --help` for more information.

#### British English spelling of 'color' (colour) accepted as an alias for

`--color`, `--stack-colors`, `stack ls stack-colors` at the
command line
and
for `color:` and `stack-colors:` in yaml configuration files.

### generate commands list more dynamically from command docs

#### names, aliases, descriptions

### inconsistent repeated options behaviour: -b 1/1 -b 2/1 should use the last date & be documented

### pass -- -h/--help through to tasty

### underquoted $ gives "empty list" error

$ bal --budget ^sm^:exp date:jan -M cur:\$ --tree
Budget performance in 2019/01:

hledger: Prelude.maximum: empty list

### 1275 drop/depth error message followup

Two commands:

$ hledger bal --drop '-999999999999999'
$ hledger bal --drop='-999999999999999'

Current output:
hledger: could not parse drop number: --depth=999999999999999 (use -h
to see usage)
hledger: argument to drop must lie in the range 0 to
9223372036854775807, but is -999999999999999 (use -h to see usage)

My proposal would look like:
hledger: drop's argument '--depth=999999999999999' must be a
positive integer less than 9223372036854775807
hledger: drop's argument '-999999999999999' must be a positive
integer less than 9223372036854775807

Variations:
hledger: drop's argument '-999999999999999' must be a positive
integer less than 2^63^
hledger: drop's argument '-999999999999999' must between 0 and 2^63^
hledger: drop's argument '-999999999999999' should be 0 <= N <
2^63^

### do recompile stack script addon if source is newer

ie, change:
$ hledger check-tag-files # compiles if there's no compiled version

### drop abbreviation uniqueness requirement ?

### get actions from <https://medium.com/@jdxcode/12-factor-cli-apps-dd3c227a0e46>

### group common options as in CliOptions.reportflags

### hledger: "date:monday-" gave a date parse error ()

### <http://neilmitchell.blogspot.com/2020/07/automatic-uis-for-command-lines-with.html>

### improve error message:

hledger: balance assignments cannot be used with accounts which are
posted to by transaction modifier rules (auto postings).
Please write the posting amount explicitly, or remove the rule.

### red color is bad on powershell navy background

```
Guest72
Hi, does anybody know how to change the negative value color in
powershell. It's drakred and almost not readable on the dark blue
background
f-a
not sure, but meanwhile using --color=never could help
Guest72
better than dark red ;)
```

### show an extra newline after txt reports, for better display when showing one after another

### show name of reader responsible for a parse error

### ugly --alias parse error

$ bal --alias a
hledger: parse error at ParseErrorBundle {bundleErrors = TrivialError 1
(Just EndOfInput) (fromList [Tokens ('=' :| "")]) :| [],
bundlePosState = PosState {pstateInput = "a", pstateOffset = 0,
pstateSourcePos = SourcePos {sourceName = "--alias a", sourceLine =
Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix =
""}}

### ugly parse error from malformed --width argument

ghci> :main areg sm.*foo -w 350,50,50
Transactions in sm:assets:foo and subaccounts:
2020-01-01 *** Exception: could not parse width option:
ParseErrorBundle {bundleErrors = TrivialError 6 (Just (Tokens (',' :|
""))) (fromList [Label ('d' :| "igit"),EndOfInput]) :| [],
bundlePosState = PosState {pstateInput = "350,50,50", pstateOffset =
0, pstateSourcePos = SourcePos {sourceName = "(unknown)", sourceLine =
Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix =
""}} (use -h to see usage)

### non-empty standard input activates -f- if there is no explicit -f ?

### --invert should be supported by all commands

### --positive flips signs of normally negative accounts (liabilities, revenues, equity)

so eg both revenues and expenses sections of income statement are
positive

### --pivot should work with all hledger tools (ui, web, api..)

### warn about missing command rather than "unknown flag"

~$ hledger-0.27.1 -f src/hledger/data/sample.journal -D date:2008/01
hledger-0.27.1: Unknown flag: -D

### balance assertion error improvements

#### show assertions in transaction

2016/04/01 * refill negative budget envelopes (personal)
[assets:personal:bank:wf:checking:month:gifts] $69.56 = 0
[assets:personal:bank:wf:checking:month:food] $97.58 = 0
[assets:personal:bank:wf:checking:month:personal care] $80.00 = $1
[assets:personal:bank:wf:checking:available] $-247.14

#### show line/column number of assertion

#### show indication inline

2016/04/01 * refill negative budget envelopes (personal)
[assets:personal:bank:wf:checking:month:gifts] $69.56 = 0
[assets:personal:bank:wf:checking:month:food] $97.58 = 0 <- failed,
calculated: $7.12 (difference: +$7.12)
[assets:personal:bank:wf:checking:month:personal care] $80.00 = $1
[assets:personal:bank:wf:checking:available] $-247.14

### consistent/more headings for options

### don't immediately convert auto-balancing amounts to the price's commodity

#### discuss on #ledger

#### example

1/1
a E4 @ $1
b ; <- should fill in -E4 @ $1 instead of -$4

### ignore inapplicable common flags when harmless, eg --width

### --help, --info after +RTS and/or -- should be passed through

### --unreal opposite of --real

### --pivot cleanups

<https://github.com/simonmichael/hledger/pull/323#issuecomment-185631456>

#### add --pivot to add-on option lists

#### move pivot example to its own section

### reports should indicate whether each item has hidden subitems (and possibly include them in the report)

### show abnormal-sign balances in red (rather than negative numbers)

### split up output formats by command

### html reports

#### add html output to other reports

##### register

##### postings

#### add --view to open browser ?

#### add detailed mode or report generating register for each cell

## compat

### compat: support reading with beancount2ledger when in PATH

### compat: support reading with ledger2beancount & beancount2ledger when in PATH

## doc

### doc: quickstart: update download instructions, make maintainable

### CONTRIBUTING: a github API script to generate the open issues table with issue counts

### automate CREDITS updating

#### ./Shake credits

#### github commit links

#### github issue links

#### github images

### a nice HISTORY page (higher level view of commits)

#### automate

##### ./Shake history

### doc: house mortgage

Chris Leyon, Ledger list 2021:

The entry for my house purchase looks like this:

2014-12-18 Buy house
Assets:Fixed:House $445,000.00 ; Selling price of house
Assets:Current:XYZ Bank:Checking $455.76
Expenses:Interest:Mortgage Interest $390.60
Expenses:Taxes:Property Tax $282.49 ; Current qtr taxes
Expenses:Taxes:Property Tax $2,154.00 ; Next qtr taxes
Expenses:Utilities:Sewer $18.45 ; Current qtr sewer
Expenses:Utilities:Sewer $141.48 ; Next qtr sewer
Expenses:House:Warranty $32.10
Liabilities:Loans:Mortgage Principal $-427,750.00 ; PV of mortgage at
t=0
Assets:Current:XYZ Bank:Savings $-20,500.00 ; Down payment
Assets:Current:AnAssetAccount $-5,001.37 ; More money down
Assets:Current:Escrow $960.49
Expenses:House:Settlement $440.00 ; Origination charges
Expenses:House:Settlement $325.00 ; Appraisal fee
Expenses:House:Settlement $75.00 ; Tax service
Expenses:House:Settlement $50.00 ; Appraisal management
Expenses:House:Settlement $2,301.00 ; Title services and lender's
title insurance
Expenses:House:Settlement $105.00 ; Owner's title insurance
Expenses:House:Settlement $220.00 ; Government recording charge
Expenses:House:Settlement $300.00 ; Survey

This establishes the "Liabilities:Loans:Mortgage Principal" account
which tracks the loan balance. "Assets:Fixed:House" can be used to
calculate your equity (as in home equity, not an equity account type).
Various charges need to be paid at closing time: title fees, current and
next quarter taxes, utility services, etc. It also categorizes
"Expenses:Interest:Mortgage Interest" as a separate category from
other types of interest, for tax purposes. Finally, it also seeds the
"Assets:Current:Escrow" account which is the source account for paying
all sorts of property tax and insurance expenses.

A monthly mortgage payment might look like this:

2015-03-23=2015-04-01 (1000) Loan Servicing Company
Liabilities:Loans:Mortgage Principal $585.63
Liabilities:Loans:Mortgage Principal $100.00 ; Pay a little extra
principal every month
Expenses:Interest:Mortgage Interest $909.08
Assets:Current:Escrow $1,024.48
Assets:Current:XYZ Bank:Checking $-2,619.19

This set up has tracked my mortgage payments and balances for several
years, down to the penny.

### hledger manual: toc: why are OUTPUT, PIVOTING not clickable ?

## entry

### entry command

#### a convenient non-interactive version of hledger add; and an extension point for data entry validations/automations

hledger entry [ONELINEENTRY]

#### ONELINEENTRY is a single argument, in quotes: a journal entry using double-space instead of newlines

hledger entry '2021-01-01 * (123) farmers market expenses:food $10
assets:checking ; date:1/3'

#### or with no argument, each line from standard input generates a journal entry

#### with -a|--add, appends to the journal, like add/import

#### if entry does not begin with a date, uses today's date

hledger entry 'farmers market expenses:food $10 assets:cash'

#### if any other required parts are omitted, they are filled from similar past transactions, like add

These will match the txn above, and make the postings shown:
hledger entry 'farmers' # expenses:food $10, assets:cash -$10
hledger entry 'farmers expenses:food $11' # expenses:food $11,
assets:cash -$11
hledger entry 'farmers $11' # same
hledger entry 'farmers 11' # same
hledger entry 'farmers expenses:food 6 expenses:snacks 5' #
expenses:food $6, expenses:snacks $5, assets:cash -$11

#### leaf names of known accounts will be expanded

hledger entry 'farmers food 6 snacks 5' # same

#### missing commodity symbol could also be inferred from source account's balance

#### or with a flag, missing required parts will give an error

##### --complete, --only, --standalone, --no-infer, --no-past, --no-journal

#### entry can run validation checks, including fancy ones like "asset accounts may not go negative"

## ops

### new hledger.org vps

## pkg

### need build-tools section in our package.yaml files to avoid build errors with happy etc. ?

cf <https://github.com/haskell/cabal/issues/5867#issuecomment-967280170>

## process

### refine RELEASING doc/process

### automate changelog finalisation

### automate release note generation

###  {#section-1}

## reports

### reports: allow -c '0.%' to set style of -%

### reports: relax the "whole subperiods" rule

#### when there's only one ?

bal -YH -e tomorrow would be titled with tomorrow's date instead of
12-31

#### when there's multiple, but no transactions before the begin date or after the end date ?

## timeclock

### timeclock: improve error message

hledger: clock-out time less than clock-in time in:
2021-09-17 * 12:00-06:00
(no-electronics) -6.00h

### timeclock: ugly parse error

hledger: line 6: expected timeclock code o but got i
CallStack (from HasCallStack):
error, called at ./Hledger/Data/Timeclock.hs:85:32 in
hledger-lib-1.22.99-HCWXy7WanhBI3o1AfvBpXy:Hledger.Data.Timeclock

## timedot

### timedot: more flexible parsing

#### ignore all preamble lines (before first date line)

#### ignore all amountless lines

#### ignore org list bullets/checkboxes

#### check timedotstrict: disables ignoring preamble/amountless lines

### timedot: --alias doesn't work with timeclock, timedot ?
