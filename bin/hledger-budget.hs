#!/usr/bin/env stack
{- stack runghc --verbosity info
  --package hledger-lib
  --package hledger
  --package text
-}
{-# LANGUAGE OverloadedStrings #-}
import Data.List
import System.Console.CmdArgs
import Hledger.Cli
import Hledger.Cli.Main (mainmode)
import Hledger.Data.AutoTransaction

actions :: [(Mode RawOpts, CliOpts -> IO ())]
actions =
    [ (manmode, man)
    , (infomode, info')
    , (balancemode, flip withJournalDo' balance)
    , (balancesheetmode, flip withJournalDo' balancesheet)
    , (cashflowmode, flip withJournalDo' cashflow)
    , (incomestatementmode, flip withJournalDo' incomestatement)
    , (registermode, flip withJournalDo' register)
    , (printmode, flip withJournalDo' print')
    ]

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
    wrapper f opts' j = f opts' =<< journalBalanceTransactions' opts' j{ jtxns = ts' } where
        -- use original transactions as input for journalBalanceTransactions to re-infer balances/prices
        modifier = originalTransaction . foldr (flip (.) . fmap txnTieKnot . runModifierTransaction Any) id mtxns
        mtxns = jmodifiertxns j
        ts' = map modifier $ jtxns j

main :: IO ()
main = do
    rawopts <- fmap decodeRawOpts . processArgs $ cmdmode
    opts <- rawOptsToCliOpts rawopts
    let cmd = command_ opts
    case find (\e -> cmd `elem` modeNames (fst e)) actions of
        Just (amode, _) | "h" `elem` map fst (rawopts_ opts) -> print amode
        Just (_, action) -> action opts
        Nothing -> print cmdmode
