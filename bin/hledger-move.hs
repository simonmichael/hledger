#!/usr/bin/env stack
-- stack runghc --verbosity info
--   --package hledger --package string-qq --package text --package time --package microlens
--
-- Using unreleased hledger: from inside the hledger source tree,
--
-- Run interpreted:
-- bin/hledger-move.hs
--
-- Compile:
-- stack ghc -- bin/hledger-move.hs -ihledger-lib -ihledger
-- or use bin/compile.sh
--
-- Debug:
-- stack ghci bin/hledger-move.hs --ghc-options=-'ihledger-lib -ihledger'
--
-- Watch compilation:
-- stack exec ghcid bin/hledger-move.hs -- --command="ghci -ihledger-lib -ihledger"
--
-- There are some tests in hledger/test/_move.test

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (when)
import Data.Function (on)
import Data.List (find, groupBy, mapAccumL)
import Data.Maybe (fromMaybe, isNothing)
import Data.String.QQ (s)
import Data.Time (addDays)
import Safe (headDef)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import Lens.Micro ((^.))
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  -- Command name and help text goes here. Current limitations:
  -- help text must be above _FLAGS, blank lines will not be displayed.
  [s| hledger-move
Print an entry to move funds between accounts, preserving costs and subaccounts

Usage: hledger-move AMT FROMACCT TOACCT

AMT is a positive hledger amount, as in journal format.
FROMACCT is an account name or regular expression, as in an acct: query.
The alphabetically first account name it matches is the source account.
TOACCT is an account name or regexp selecting the destination account.

This command prints a journal entry which you can add to your journal,
representing a transfer of the requested amount from the source account
to the destination account.

The commodity to be moved is determined by AMT's commodity symbol.

This command can also move amounts from subaccounts (one level, at least).
It will move amounts first out of the main source account if possible,
then as needed out of each subaccount in alphanumerical order of names,
until the total requested amount is moved.
(This is useful when withdrawing from an account with subaccounts
representing investment lots; if these are named by acquisition date
(eg ":YYYYMMDD"), they will be moved in FIFO order.)

This command is mainly intended for moving assets.
If there are not sufficient positive balances in the source account(s)
to supply the requested amount, the command will fail.

Any source subaccounts used will be recreated under the destination account.
Or, to consolidate amounts in the main destination account 
(discarding lot information), use the --consolidate flag.


Examples:

$ hledger-move $50 assets:checking assets:cash  # withdraw cash from bank
$ hledger-move ADA1000 ada:wallet1 ada:wallet2  # move 1000 ADA, keeping lots

_FLAGS
|]
{- NOT YET IMPLEMENTED:

As a convenience, no symbol means "move the account's only commodity";
this works when the source account contains just one commodity.
$ hledger-move 50 checking cash                 # the same, less typing

A zero AMT means "move all of the specified commodity".

or the keyword "all"
An "all" AMT does the same, but for all commodities present;
it works when all of the source account's commodities are positive.
$ hledger-move all savings checking               # all savings -> checking
$ hledger-move all assets:broker1:FOO assets:broker2:FOO  # move all lots

It is aware of account balances, and prevents overdraft/overpay:
it will fail if the requested transfer would make
the source account go negative (as when overdrawing an asset)
or the destination account go positive (as when over-paying a liability).
You can disable this validation by adding the --force flag.

balance assertions

-}

------------------------------------------------------------------------------
  [flagNone ["consolidate"]   (setboolopt "consolidate") "don't recreate subaccounts"
  -- ,flagNone ["force"]         (setboolopt "force") "don't prevent overdraw/overpay"
  ]
  [generalflagsgroup1]
  []
  ([arg "AMT"
   ,arg "FROMACCT"
   ,arg "TOACCT"
   ], 
   Nothing
  )
  where
    arg name = flagArg (\val rawopts -> Right $ setopt name val rawopts) name
------------------------------------------------------------------------------

main :: IO ()
main = do
  copts@CliOpts{rawopts_=rawopts, reportspec_=rspec0} <- getHledgerCliOpts cmdmode
  withJournalDo copts $ \j -> do
    -- d <- getCurrentDay
    let
      -- arg errors
      -- clunky
      shortusage = "Usage: hledger-move AMT FROMACCT TOACCT"
      longusage  = unlines
        [ shortusage
        , "AMT       the total amount to move, as a hledger amount with commodity symbol"
        , "FROMACCT  the main account to move it from; subaccounts can also be drained"
        , "TOACCT    the main account to move it to; subaccounts can be recreated here"
        ]
      -- No args should show usage, not "Error:" (but I guess still needs a failure exit code)
      exitUsage = unsafePerformIO $ hPutStrLn stderr longusage >> exitFailure
      mamtarg      = maybestringopt "AMT" rawopts
      mfromacctarg = maybestringopt "FROMACCT" rawopts
      mtoacctarg   = maybestringopt "TOACCT" rawopts
      noargs       = all isNothing [mamtarg, mfromacctarg, mtoacctarg]
      amtarg       = fromMaybe (error' $ "Please specify the amount to move as first argument.\n"++shortusage) mamtarg  -- won't happen
      fromacctarg  = fromMaybe (error' $ "Please specify a source account name or pattern as second argument.\n"++shortusage) mfromacctarg
      toacctarg    = fromMaybe (error' $ "Please specify a destination account name or pattern as third argument.\n"++shortusage) mtoacctarg

      consolidate = boolopt "consolidate" rawopts
      force       = boolopt "force" rawopts

      -- parse the AMT arg as a cost-less Amount (any provided cost is ignored)
      eamt = styleAmount (journalCommodityStyles j) . amountStripPrices <$> parseamount amtarg
      amt = case eamt of
        Left err ->
          error' $ "could not parse " ++ show amtarg ++ " as a hledger amount\n" ++ customErrorBundlePretty err ++ "\n" ++shortusage
        Right a | isNegativeAmount a ->
          error' $ amtarg ++ " is negative, please specify a positive amount to move.\n"++shortusage
        Right a -> a
      comm = acommodity amt
      -- when comparing with zero, it needs to have the same commodity
      zero = amt{aquantity=0}
      accts = journalAccountNamesDeclaredOrImplied j
      fromacct = amt `seq` fromMaybe (error' $ fromacctarg ++ " did not match any account.") $ firstMatch (T.pack fromacctarg) accts
      fromacctlen = length $ accountNameComponents fromacct
      toacct   = fromacct `seq` fromMaybe (error' $ toacctarg   ++ " did not match any account.") $ firstMatch (T.pack toacctarg) accts

      -- get account names and balances of fromacct and any subs, ordered by name
      ropts = (_rsReportOpts rspec0){balanceaccum_=Historical, accountlistmode_=ALFlat}
      rspec =
        setDefaultConversionOp NoConversionOp  -- ?
        rspec0{
           _rsReportOpts = ropts
          ,_rsQuery      = Acct $ accountNameToAccountRegex $ fromacct
          }
      acctbals = fst $ balanceReport rspec j
      availablebal =
        headDef zero $ amounts $
        filterMixedAmountByCommodity comm $
        mixedAmountStripPrices $ sum $ map fourth4 acctbals

      -- Take just enough of these account balances, in the order given,
      -- to cover the requested AMT. Or if there is not enough, take what is there.
      -- AMT is a single-commodity, cost-less amount.
      -- Account balances can be multi-commodity, but only AMT's commodity will be moved.
      -- An account balance could also possibly have multiple costs in that commodity;
      -- in that case we raise an error, for now. (Could take amounts in order of cost's
      -- commodity and amount).
      (unmoved, moveamts) = go (dbgamt "requested amt to move" amt) [] acctbals
        where
          dbgamt lbl  = id  -- dbg0With (((lbl++": ")++).showAmount)
          dbgmamt lbl = id  -- dbg0With (((lbl++": ")++).showMixedAmountOneLine)

          go :: Amount -> [(AccountName, MixedAmount)] -> [BalanceReportItem] -> (Amount, [(AccountName, MixedAmount)])
          go stilltomove balscollected [] = (stilltomove, reverse balscollected)
          go stilltomove balscollected ((acct,_,_,bal):rest)
            | stilltomovenext > zero = go stilltomovenext ((acct,balincomm) : balscollected) rest 
            | otherwise =
                let
                  -- the final balance portion to move
                  finalamt = dbgamt "final amt to move" $
                    (balincommsinglecost + stilltomovenext)
                      {aprice=aprice balincommsinglecost}  -- + discards cost, need to restore it
                in (0, reverse $ (acct, mixed [finalamt]) : balscollected)
            where
              -- how much of the requested commodity is in this account
              comm = acommodity stilltomove
              balincomm = filterMixedAmountByCommodity comm bal
              -- for now, ensure there is at most one cost basis (and convert to Amount)
              balincommsinglecost =
                case amounts $ balincomm of
                  [b] -> dbgamt ("acct balance in "++show comm) b
                  _   -> error' $ "sorry, we can't yet move funds out of a multi-cost balance ("
                            ++ showMixedAmountOneLine balincomm ++ ")"
              -- subtract this from the amount remaining to move (ignoring cost)
              stilltomovenext = dbgamt "remaining amt to move" $
                stilltomove - amountStripPrices balincommsinglecost

      -- since balance assertion amounts are required to be exact, the
      -- amounts in opening/closing transactions should be too (#941, #1137)
      -- amountSetFullPrecision
      fromps = [
        posting{paccount = a
                ,pamount = mixedAmount $ negate b
                -- ,pbalanceassertion = Just nullassertion{baamount=precise b{aquantity=0, aprice=Nothing}}
                }

        | -- get the balances for each commodity and transaction price
          (a,mixedb) <- moveamts
        , let bs0 = amounts mixedb
          -- mark the last balance in each commodity with True
        , let bs2 = concat [reverse $ zip (reverse bs1) (True : repeat False)
                            | bs1 <- groupBy ((==) `on` acommodity) bs0]
        , (b, islast) <- bs2
        ]

      tops = if consolidate
        then [
          posting{paccount = toacct, pamount = mixed [amt]}
          ]
        else [
          posting{paccount = a', pamount = negate b}
          | Posting{paccount=a, pamount=b} <- fromps
          , let subacctcomps = drop fromacctlen $ accountNameComponents a
          , let a' = accountNameFromComponents $ toacctcomps ++ subacctcomps
          ]
          where
            toacctcomps = accountNameComponents toacct

    if
      | noargs -> exitUsage
      | unmoved > zero -> error' $
        "could not move " ++ showAmount amt ++ ", only " ++ showAmount availablebal ++ " is available in commodity " ++ show comm 
      | otherwise ->
        T.putStr $ showTransaction $ nulltransaction{
          tdate        = _rsDay rspec
          ,tdescription = ""
          ,tpostings    = fromps ++ tops
        }

firstMatch :: T.Text -> [T.Text] -> Maybe T.Text
firstMatch pat vals =
  let re = toRegexCI' pat
  in find (regexMatchText re) vals
