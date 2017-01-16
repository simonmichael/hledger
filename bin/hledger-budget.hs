#!/usr/bin/env stack
{- stack runghc --verbosity info
  --package hledger-lib
  --package hledger
  --package text
-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow (first)
import Data.Maybe
import Data.List
import System.Console.CmdArgs
import Hledger.Cli
import Hledger.Cli.Main (mainmode)
import Hledger.Data.AutoTransaction

budgetFlags :: [Flag RawOpts]
budgetFlags =
    [ flagNone ["no-buckets"] (setboolopt "no-buckets") "show all accounts besides mentioned in periodic transactions"
    , flagNone ["no-offset"] (setboolopt "no-offset") "do not add up periodic transactions"
    ]

actions :: [(Mode RawOpts, CliOpts -> IO ())]
actions = first injectBudgetFlags <$>
    [ (manmode, man)
    , (infomode, info')
    , (balancemode, flip withJournalDo' balance)
    , (balancesheetmode, flip withJournalDo' balancesheet)
    , (cashflowmode, flip withJournalDo' cashflow)
    , (incomestatementmode, flip withJournalDo' incomestatement)
    , (registermode, flip withJournalDo' register)
    , (printmode, flip withJournalDo' print')
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

cmdmode :: Mode RawOpts
cmdmode = (mainmode [])
    { modeNames = ["hledger-budget"]
    , modeGroupModes = Group
        { groupUnnamed = map fst actions
        , groupNamed = []
        , groupHidden = []
        }
    }

journalBalanceTransactions' :: CliOpts -> Journal -> IO Journal
journalBalanceTransactions' opts j = do
    let assrt = not $ ignore_assertions_ opts
    either error' return $ journalBalanceTransactions assrt j

withJournalDo' :: CliOpts -> (CliOpts -> Journal -> IO ()) -> IO ()
withJournalDo' opts = withJournalDo opts . wrapper where
    wrapper f opts' j = do
        -- use original transactions as input for journalBalanceTransactions to re-infer balances/prices
        let modifier = originalTransaction . foldr (flip (.) . runModifierTransaction') id mtxns
            runModifierTransaction' = fmap txnTieKnot . runModifierTransaction Any
            mtxns = jmodifiertxns j
            dates = jdatespan j
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
    rawopts <- fmap decodeRawOpts . processArgs $ cmdmode
    opts <- rawOptsToCliOpts rawopts
    case find (\e -> command_ opts `elem` modeNames (fst e)) actions of
        Just (amode, _) | "h" `elem` map fst (rawopts_ opts) -> print amode
        Just (_, action) -> action opts
        Nothing -> print cmdmode
