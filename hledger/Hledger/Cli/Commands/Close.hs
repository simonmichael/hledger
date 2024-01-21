{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Hledger.Cli.Commands.Close (
  closemode
 ,close
)
where

import Data.Function (on)
import Data.List (groupBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Calendar (addDays)
import Lens.Micro ((^.))
import System.Console.CmdArgs.Explicit as C

import Hledger
import Hledger.Cli.CliOptions
import Safe (lastDef, readMay)

defclosedesc  = "closing balances"
defopendesc   = "opening balances"
defretaindesc = "retain earnings"

defcloseacct  = "equity:opening/closing balances"
defretainacct = "equity:retained earnings"

closemode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Close.txt")
  [flagOpt "" ["migrate"]    (\s opts -> Right $ setopt "migrate" s opts) "NEW" ("show closing and opening transactions,"
    <> " for Asset and Liability accounts by default,"
    <> " tagged for easy matching,"
    <> " with NEW (eg a new year) as tag value."
    )
  ,flagOpt "" ["close"]      (\s opts -> Right $ setopt "close" s opts)  "NEW" "(default) show a closing transaction"
  ,flagOpt "" ["open"]       (\s opts -> Right $ setopt "open" s opts)   "NEW" "show an opening transaction"
  ,flagOpt "" ["assign"]     (\s opts -> Right $ setopt "assign" s opts) "NEW" "show opening balance assignments"
  ,flagOpt "" ["assert"]     (\s opts -> Right $ setopt "assert" s opts) "NEW" "show closing balance assertions"
  ,flagOpt "" ["retain"]     (\s opts -> Right $ setopt "retain" s opts) "NEW" "show a retain earnings transaction, for Revenue and Expense accounts by default"
  ,flagNone ["explicit","x"] (setboolopt "explicit") "show all amounts explicitly"
  ,flagNone ["show-costs"]   (setboolopt "show-costs") "show amounts with different costs separately"
  ,flagNone ["interleaved"]  (setboolopt "interleaved") "show source and destination postings together"
  ,flagReq  ["close-desc"]   (\s opts -> Right $ setopt "close-desc" s opts) "DESC" "set closing transaction's description"
  ,flagReq  ["close-acct"]   (\s opts -> Right $ setopt "close-acct" s opts) "ACCT" "set closing transaction's destination account"
  ,flagReq  ["open-desc"]    (\s opts -> Right $ setopt "open-desc"  s opts) "DESC" "set opening transaction's description"
  ,flagReq  ["open-acct"]    (\s opts -> Right $ setopt "open-acct"  s opts) "ACCT" "set opening transaction's source account"
  ]
  [generalflagsgroup1]
  (hiddenflags
    ++  -- keep supporting old flag names for compatibility
    [flagNone ["closing"]   (setboolopt "close")                                   "old spelling of --close"
    ,flagNone ["opening"]   (setboolopt "open")                                    "old spelling of --open"
    ,flagReq  ["close-to"]  (\s opts -> Right $ setopt "close-acct" s opts) "ACCT" "old spelling of --close-acct"
    ,flagReq  ["open-from"] (\s opts -> Right $ setopt "open-acct"  s opts) "ACCT" "old spelling of --open-acct"
    ]
  )
  ([], Just $ argsFlag "[--migrate|--close|--open|--assign|--assert|--retain] [ACCTQUERY]")

-- | The close command's mode (subcommand).
-- The code depends on these spellings.
data CloseMode = Migrate | Close | Open | Assign | Assert | Retain deriving (Eq,Show,Read)

-- | Pick the rightmost flag spelled like a CloseMode (--migrate, --close, --open, etc), or default to Close.
closeModeFromRawOpts :: RawOpts -> CloseMode
closeModeFromRawOpts rawopts = lastDef Close $ collectopts (\(name,_) -> readMay (capitalise name)) rawopts

-- Debugger, beware: close is incredibly devious; simple rules combine to make a horrid maze.
-- Tests are in hledger/test/close.test.
close copts@CliOpts{rawopts_=rawopts, reportspec_=rspec0} j = do
  let
    mode_ = closeModeFromRawOpts rawopts
    defacctsq_    = if mode_ == Retain then Type [Revenue, Expense] else Type [Asset, Liability]
    defcloseacct_ = if mode_ == Retain then defretainacct else defcloseacct
    closeacct = T.pack $ fromMaybe defcloseacct_ $ maybestringopt "close-acct" rawopts
    openacct  = maybe closeacct T.pack $ maybestringopt "open-acct" rawopts

    -- For easy matching and exclusion, a recognisable tag is added to all generated transactions,
    -- with the mode flag's argument if any (NEW, eg a new year number) as its argument.
    comment = T.pack $ if
      | mode_ == Assert -> "balances:" <> flagval
      | mode_ == Retain -> "retain:"   <> flagval
      | otherwise       -> "start:"    <> flagval
      where
        flagval = fromMaybe "" $ maybestringopt modeflag rawopts
          where modeflag = lowercase $ show mode_

    ropts = (_rsReportOpts rspec0){balanceaccum_=Historical, accountlistmode_=ALFlat}
    rspec1 = setDefaultConversionOp NoConversionOp rspec0{_rsReportOpts=ropts}

    -- Dates of the closing and opening transactions.
    -- "The default closing date is yesterday, or the journal's end date, whichever is later.
    -- You can change this by specifying a [report end date](#report-start--end-date) with `-e`.
    -- The last day of the report period will be the closing date, eg `-e 2024` means "close on 2023-12-31".
    -- The opening date is always the day after the closing date."
    argsq = _rsQuery rspec1
    yesterday = addDays (-1) $ _rsDay rspec1
    yesterdayorjournalend = case journalLastDay False j of
      Just journalend -> max yesterday journalend
      Nothing         -> yesterday
    mreportlastday = addDays (-1) <$> queryEndDate False argsq
    closedate = fromMaybe yesterdayorjournalend  mreportlastday
    opendate = addDays 1 closedate

    -- should we show the amount(s) on the equity posting(s) ?
    explicit = boolopt "explicit" rawopts || copts ^. infer_costs

    -- the accounts to close
    argsacctq = filterQuery (\q -> queryIsAcct q || queryIsType q) argsq
    q2 = if queryIsNull argsacctq then And [argsq, defacctsq_] else argsq
    -- always exclude the balancing equity account
    q3 = And [q2, Not $ Acct $ accountNameToAccountOnlyRegex closeacct]
    -- the balances to close
    rspec3 = rspec1{_rsQuery=q3}
    (acctbals',_) = balanceReport rspec3 j
    acctbals = map (\(a,_,_,b) -> (a, if show_costs_ ropts then b else mixedAmountStripPrices b)) acctbals'
    totalamt = maSum $ map snd acctbals

    -- since balance assertion amounts are required to be exact, the
    -- amounts in opening/closing transactions should be too (#941, #1137)
    precise = amountSetFullPrecision

    -- interleave equity postings next to the corresponding closing posting, or put them all at the end ?
    interleaved = boolopt "interleaved" rawopts

    -- the closing (balance-asserting or balance-zeroing) transaction
    mclosetxn
      | mode_ `notElem` [Migrate, Close, Assert, Retain] = Nothing
      | otherwise = Just nulltransaction{
          tdate=closedate, tdescription=closedesc, tcomment=comment, tpostings=closeps
          }
      where
        closedesc = T.pack $ fromMaybe defclosedesc_ $ maybestringopt "close-desc" rawopts
          where defclosedesc_ = if mode_ == Retain then defretaindesc else defclosedesc
        closeps
          -- XXX some duplication
          | mode_ == Assert =
            [ posting{
                  paccount          = a
                  ,pamount           = mixedAmount $ precise b{aquantity=0, aprice=Nothing}
                  -- after each commodity's last posting, assert 0 balance (#1035)
                  -- balance assertion amounts are unpriced (#824)
                  ,pbalanceassertion =
                      if islast
                      then Just nullassertion{baamount=precise b}
                      else Nothing
                  }
              | -- get the balances for each commodity and transaction price
                (a,mb) <- acctbals
              , let bs0 = amounts mb
                -- mark the last balance in each commodity with True
              , let bs2 = concat [reverse $ zip (reverse bs1) (True : repeat False)
                                | bs1 <- groupBy ((==) `on` acommodity) bs0]
              , (b, islast) <- bs2
            ]

          | otherwise =
            concat [
              posting{paccount          = a
                    ,pamount           = mixedAmount . precise $ negate b
                    -- after each commodity's last posting, assert 0 balance (#1035)
                    -- balance assertion amounts are unpriced (#824)
                    ,pbalanceassertion =
                        if islast
                        then Just nullassertion{baamount=precise b{aquantity=0, aprice=Nothing}}
                        else Nothing
                    }

              -- maybe an interleaved posting transferring this balance to equity
              : [posting{paccount=closeacct, pamount=mixedAmount $ precise b} | interleaved]

              | -- get the balances for each commodity and transaction price
                (a,mb) <- acctbals
              , let bs0 = amounts mb
                -- mark the last balance in each commodity with True
              , let bs2 = concat [reverse $ zip (reverse bs1) (True : repeat False)
                                | bs1 <- groupBy ((==) `on` acommodity) bs0]
              , (b, islast) <- bs2
            ]

            -- or a final multicommodity posting transferring all balances to equity
            -- (print will show this as multiple single-commodity postings)
            ++ [posting{paccount=closeacct, pamount=if explicit then mixedAmountSetFullPrecision totalamt else missingmixedamt} | not interleaved]

    -- the opening (balance-assigning or balance-unzeroing) transaction
    mopentxn
      | mode_ `notElem` [Migrate, Open, Assign] = Nothing
      | otherwise = Just nulltransaction{
          tdate=opendate, tdescription=opendesc, tcomment=comment, tpostings=openps
          }
      where
        opendesc  = T.pack $ fromMaybe defopendesc  $ maybestringopt "open-desc"  rawopts
        openps
          | mode_ == Assign =
            [ posting{paccount         = a
                    ,pamount           = missingmixedamt
                    ,pbalanceassertion = Just nullassertion{baamount=b}
                        -- case mcommoditysum of
                        --   Just s  -> Just nullassertion{baamount=precise s}
                        --   Nothing -> Nothing
                    }

              | (a,mb) <- acctbals
              , let bs0 = amounts mb
                -- mark the last balance in each commodity with the unpriced sum in that commodity (for a balance assertion)
              , let bs2 = concat [reverse $ zip (reverse bs1) (Just commoditysum : repeat Nothing)
                                | bs1 <- groupBy ((==) `on` acommodity) bs0
                                , let commoditysum = (sum bs1)]
              , (b, _mcommoditysum) <- bs2
            ]
            ++ [posting{paccount=openacct, pamount=if explicit then mixedAmountSetFullPrecision (maNegate totalamt) else missingmixedamt} | not interleaved]

          | otherwise =
            concat [
              posting{paccount          = a
                    ,pamount           = mixedAmount $ precise b
                    ,pbalanceassertion =
                        case mcommoditysum of
                          Just s  -> Just nullassertion{baamount=precise s{aprice=Nothing}}
                          Nothing -> Nothing
                    }
              : [posting{paccount=openacct, pamount=mixedAmount . precise $ negate b} | interleaved]

              | (a,mb) <- acctbals
              , let bs0 = amounts mb
                -- mark the last balance in each commodity with the unpriced sum in that commodity (for a balance assertion)
              , let bs2 = concat [reverse $ zip (reverse bs1) (Just commoditysum : repeat Nothing)
                                | bs1 <- groupBy ((==) `on` acommodity) bs0
                                , let commoditysum = (sum bs1)]
              , (b, mcommoditysum) <- bs2
            ]
            ++ [posting{paccount=openacct, pamount=if explicit then mixedAmountSetFullPrecision (maNegate totalamt) else missingmixedamt} | not interleaved]

  -- print them
  maybe (pure ()) (T.putStr . showTransaction) mclosetxn
  maybe (pure ()) (T.putStr . showTransaction) mopentxn
 