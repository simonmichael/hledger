#!/usr/bin/env stack
{- stack runghc --verbosity info
  --package hledger-lib
  --package hledger
  --package cmdargs
  --package text
-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

import Control.Arrow (first)
import Data.Maybe
import Data.List
import Data.String.Here
import System.Console.CmdArgs
import Hledger.Cli

-- hledger-budget REPORT-COMMAND [--no-offset] [--no-buckets] [OPTIONS...]

budgetmode :: Mode RawOpts
budgetmode = (hledgerCommandMode
    [here| budget
Perform some subset of reports available in core hledger but process automated
and periodic transactions. Also simplify tree of accounts to ease view of
"budget buckets". People familiar with ledger budgeting
(http://www.ledger-cli.org/3.0/doc/ledger3.html#Budgeting)
may consider this tool as an alias to `ledger --budget`.
FLAGS
With this tool you may either use so called periodic transactions that being
issued with each new period or use a family of approaches with automated
transactions. You may want to look at [budgeting section of
plaintextaccounting](http://plaintextaccounting.org/#budgeting).

Periodic transaction that being interpreted by this tool may look like:

~ monthly from 2017/3
    income:salary  $-4,000.00
    expenses:taxes  $1,000
    expenses:housing:rent  $1,200
    expenses:grocery  $400
    expenses:leisure  $200
    expenses:health  $200
    expenses  $100
    assets:savings

Header of such entries starts with `'~'` (tilde symbol) following by an
interval with an effect period when transactions should be injected.

Effect of declaring such periodic transaction is:

- Transactions will be injected at the beginning of each period. I.e. for
  monthly it will always refer to 1st day of month.
- Injected transaction will have inverted amounts to offset existing associated
  expenses. I.e. for this example negative balance indicates how much you have
  within your budget and positive amounts indicates how far you off from your
  budget.
- Set of accounts across of all periodic transactions will form kinda buckets
  where rest of the accounts will be sorted into. Each account not mentioned in
  any of periodic transaction will be dropped without changing of balance for
  parent account. I.e. for this example postings for `expenses:leisure:movie`
  will contribute to the  balance of `expenses:leisure` only in reports.

Note that beside a periodic transaction all automated transactions will be
handled in a similar way how they are handled in `rewrite` command.

Bucketing

It is very common to have more expense accounts than budget
"envelopes"/"buckets". For this reason all periodic transactions are treated as
a source of information about your budget "buckets".

I.e. example from previous section will build a sub-tree of accounts that look like

assets:savings
expenses
  taxes
  housing:rent
  grocery
  leisure
  health
income:salary

All accounts used in your transactions journal files will be classified
according to that tree to contribute to an appropriate bucket of budget.

Everything else will be collected under virtual account `<unbucketed>` to give
you an idea of what parts of your accounts tree is not budgeted. For example
`liabilities` will contributed to that entry.

Reports

You can use `budget` command to produce next reports:

- `balance` - the most important one to track how you follow your budget. If
  you use month-based budgeting you may want to use `--monthly` and
  `--row-total` option to see how you are doing through the months. You also
  may find it useful to add `--tree` option to see aggregated totals per
  intermediate node of accounts tree.
- `register` - might be useful if you want to see long history (ex. `--weekly`)
  that is too wide to fit into your terminal.
- `print` - this is mostly to check what actually happens. But you may use it
  if you prefer to generate budget transactions and store it in a separate
  journal for some less popular budgeting scheme.

Extra options for reports

You may tweak behavior of this command with additional options `--no-offset` and `--no-bucketing`.

- Don't use these options if your budgeting schema includes both periodic
  transactions, and "bucketing". Unless you want to figure out how your
  budgeting might look like. You may find helpful values of average column from
  report

$ hledger budget -- bal --period 'monthly to last month' --no-offset --average

- Use `--no-offset` and `--no-bucketing` if your schema fully relies on
  automated transactions and hand-crafted budgeting transactions. In this mode
  only automated transactions will be processed. I.e. when you journal looks
  something like

= ^expenses:food
  budget:gifts  *-1
  assets:budget  *1

2017/1/1 Budget for Jan
  assets:bank  $-1000
  budget:gifts  $200
  budget:misc

- Use `--no-bucketing` only if you want to produce a valid journal. For example
  when you want to pass it as an input for other `hledger` command. Most people
  will find this useless.

Recommendations

- Automated transaction should follow same rules that usual transactions follow
  (i.e. keep balance for real and balanced virtual postings).
- Don't change the balance of real asset and liability accounts for which you
  usually put assertions. Keep in mind that `hledger` do not apply modification
  transactions.
- In periodic transactions to offset your budget use either top-level account
  like `Assets` or introduce a "virtual" one like `Assets:Bank:Budget` that
  will be a child to the one you want to offset.

|]
    [] -- ungrouped flags
    [("\nBudgeting", budgetFlags), generalflagsgroup2] -- groupped flags
    [] -- hidden flags
    ([], Nothing)
    ) { modeGroupModes = Group
        { groupUnnamed = map fst actions
        , groupNamed = []
        , groupHidden = []
        }
    }

budgetFlags :: [Flag RawOpts]
budgetFlags =
    [ flagNone ["no-buckets"] (setboolopt "no-buckets") "show all accounts besides mentioned in periodic transactions"
    , flagNone ["no-offset"] (setboolopt "no-offset") "do not add up periodic transactions"
    ]

actions :: [(Mode RawOpts, CliOpts -> Journal -> IO ())]
actions = first injectBudgetFlags <$>
    [ (balancemode, balance)
    , (balancesheetmode, balancesheet)
    , (cashflowmode, cashflow)
    , (incomestatementmode, incomestatement)
    , (registermode, register)
    , (printmode, print')
    ]

injectBudgetFlags :: Mode RawOpts -> Mode RawOpts
injectBudgetFlags = injectFlags "\nBudgeting" budgetFlags

-- maybe lenses will help...
injectFlags :: String -> [Flag RawOpts] -> Mode RawOpts -> Mode RawOpts
injectFlags section flags mode0 = mode' where
    mode' = mode0 { modeGroupFlags = groupFlags' }
    groupFlags0 = modeGroupFlags mode0
    groupFlags' = groupFlags0 { groupNamed = namedFlags' }
    namedFlags0 = groupNamed groupFlags0
    namedFlags' =
        case ((section ==) . fst) `partition` namedFlags0 of
            ([g], gs) -> (fst g, snd g ++ flags) : gs
            _ -> (section, flags) : namedFlags0

journalBalanceTransactions' :: CliOpts -> Journal -> IO Journal
journalBalanceTransactions' opts j = do
    let assrt = not . ignore_assertions_ $ inputopts_ opts
    either error' return $ journalBalanceTransactions assrt j

budgetWrapper :: (CliOpts -> Journal -> IO ()) -> CliOpts -> Journal -> IO ()
budgetWrapper f opts' j = do
        -- use original transactions as input for journalBalanceTransactions to re-infer balances/prices
        let modifier = originalTransaction . foldr (flip (.) . runModifierTransaction') id mtxns
            runModifierTransaction' = fmap txnTieKnot . runModifierTransaction Any
            mtxns = jmodifiertxns j
            dates = spanUnion (jdatespan j) (periodAsDateSpan $ period_ $ reportopts_ opts')
            ts' = map modifier $ jtxns j
            ts'' | boolopt "no-offset" $ rawopts_ opts' = ts'
                 | otherwise= [makeBudget t | pt <- jperiodictxns j, t <- runPeriodicTransaction pt dates] ++ ts'
            makeBudget t = txnTieKnot $ t
                { tdescription = "Budget transaction"
                , tpostings = map makeBudgetPosting $ tpostings t
                }
            makeBudgetPosting p = p { pamount = negate $ pamount p }
        j' <- journalBalanceTransactions' opts' j{ jtxns = ts'' }

        -- re-map account names into buckets from periodic transaction
        let buckets = budgetBuckets j
            remapAccount "" = "<unbucketed>"
            remapAccount an
                | an `elem` buckets = an
                | otherwise = remapAccount (parentAccountName an)
            remapPosting p = p { paccount = remapAccount $ paccount p, porigin = Just . fromMaybe p $ porigin p }
            remapTxn = mapPostings (map remapPosting)
        let j'' | boolopt "no-buckets" $ rawopts_ opts' = j'
                | null buckets = j'
                | otherwise = j' { jtxns = remapTxn <$> jtxns j' }

        -- finally feed to real command
        f opts' j''

budgetBuckets :: Journal -> [AccountName]
budgetBuckets = nub . map paccount . concatMap ptpostings . jperiodictxns

mapPostings :: ([Posting] -> [Posting]) -> (Transaction -> Transaction)
mapPostings f t = txnTieKnot $ t { tpostings = f $ tpostings t }

main :: IO ()
main = do
    rawopts <- fmap decodeRawOpts . processArgs $ budgetmode
    opts <- rawOptsToCliOpts rawopts
    withJournalDo opts budget

budget :: CliOpts -> Journal -> IO ()
budget opts journal =
    case find (\e -> command_ opts `elem` modeNames (fst e)) actions of
        Just (_, action) -> budgetWrapper action opts journal
        Nothing -> print budgetmode
