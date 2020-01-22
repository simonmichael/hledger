{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Close (
  closemode
 ,close
)
where

import Control.Monad (when)
import Data.Function (on)
import Data.List (groupBy)
import Data.Maybe
import qualified Data.Text as T (pack)
import Data.Time.Calendar
import System.Console.CmdArgs.Explicit as C

import Hledger
import Hledger.Cli.CliOptions

defclosingdesc = "closing balances"
defopeningdesc = "opening balances"
defclosingacct = "equity:opening/closing balances"
defopeningacct = defclosingacct

closemode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Close.txt")
  [flagNone ["close"]        (setboolopt "close") "show just closing transaction"
  ,flagNone ["open"]         (setboolopt "open") "show just opening transaction"
  ,flagReq  ["close-desc"]   (\s opts -> Right $ setopt "close-desc" s opts) "DESC" ("description for closing transaction (default: "++defclosingdesc++")")
  ,flagReq  ["open-desc"]    (\s opts -> Right $ setopt "open-desc"  s opts) "DESC" ("description for opening transaction (default: "++defopeningdesc++")")
  ,flagReq  ["close-acct"]   (\s opts -> Right $ setopt "close-acct" s opts) "ACCT" ("account to transfer closing balances to (default: "++defclosingacct++")")
  ,flagReq  ["open-acct"]    (\s opts -> Right $ setopt "open-acct"  s opts) "ACCT" ("account to transfer opening balances from (default: "++defopeningacct++")")
  ,flagNone ["explicit","x"] (setboolopt "explicit") "show all amounts explicitly"
  ,flagNone ["interleaved"]  (setboolopt "interleaved") "keep equity and non-equity postings adjacent"
  ,flagNone ["show-costs"]   (setboolopt "show-costs") "keep balances with different costs separate"
  ]
  [generalflagsgroup1]
  (hiddenflags ++
    -- old close flags for compatibility, hidden
    [flagNone ["closing"] (setboolopt "close") "old spelling of --close"
    ,flagNone ["opening"] (setboolopt "open") "old spelling of --open"
    ,flagReq  ["close-to"]  (\s opts -> Right $ setopt "close-acct" s opts) "ACCT" ("old spelling of --close-acct")
    ,flagReq  ["open-from"] (\s opts -> Right $ setopt "open-acct" s opts) "ACCT" ("old spelling of --open-acct")
    ])
  ([], Just $ argsFlag "[QUERY]")

-- debugger, beware: close is incredibly devious. simple rules combine to make a horrid maze.
-- tests are in tests/close.test.
close CliOpts{rawopts_=rawopts, reportopts_=ropts} j = do
  today <- getCurrentDay
  let
    -- show opening entry, closing entry, or (default) both ?
    (opening, closing) =
      case (boolopt "open" rawopts, boolopt "close" rawopts) of
        (False, False) -> (True, True)
        (o, c)         -> (o, c)

    -- descriptions to use for the closing/opening transactions
    closingdesc = fromMaybe (T.pack defclosingdesc) $ T.pack <$> maybestringopt "close-desc" rawopts
    openingdesc = fromMaybe (T.pack defopeningdesc) $ T.pack <$> maybestringopt "open-desc" rawopts

    -- accounts to close to and open from
    -- if only one is specified, it is used for both
    (closingacct, openingacct) =
      let (mc, mo) =
            (T.pack <$> maybestringopt "close-acct" rawopts, T.pack <$> maybestringopt "open-acct" rawopts)
      in case (mc, mo) of
        (Just c, Just o)   -> (c, o)
        (Just c, Nothing)  -> (c, c)
        (Nothing, Just o)  -> (o, o)
        (Nothing, Nothing) -> (T.pack defclosingacct, T.pack defopeningacct)

    -- dates of the closing and opening transactions
    ropts_ = ropts{balancetype_=HistoricalBalance, accountlistmode_=ALFlat}
    q = queryFromOpts today ropts_
    openingdate = fromMaybe today $ queryEndDate False q
    closingdate = addDays (-1) openingdate

    -- should we show the amount(s) on the equity posting(s) ?
    explicit = boolopt "explicit" rawopts

    -- should we preserve cost information ?
    normalise = case boolopt "show-costs" rawopts of
                  True  -> normaliseMixedAmount
                  False -> normaliseMixedAmount . mixedAmountStripPrices

    -- the balances to close
    (acctbals,_) = balanceReportFromMultiBalanceReport ropts_ q j
    totalamt = sum $ map (\(_,_,_,b) -> normalise b) acctbals

    -- since balance assertion amounts are required to be exact, the
    -- amounts in opening/closing transactions should be too (#941, #1137)
    precise = setFullPrecision

    -- interleave equity postings next to the corresponding closing posting, or put them all at the end ?
    interleaved = boolopt "interleaved" rawopts

    -- the closing transaction
    closingtxn = nulltransaction{tdate=closingdate, tdescription=closingdesc, tpostings=closingps}
    closingps =
      concat [
        [posting{paccount          = a
                ,pamount           = mixed [precise $ negate b]
                -- after each commodity's last posting, assert 0 balance (#1035)
                -- balance assertion amounts are unpriced (#824)
                ,pbalanceassertion =
                    if islast
                    then Just nullassertion{baamount=precise b{aquantity=0, aprice=Nothing}}
                    else Nothing
                }
        ]
        -- maybe an interleaved posting transferring this balance to equity
        ++ [posting{paccount=closingacct, pamount=Mixed [precise b]} | interleaved]

        | -- get the balances for each commodity and transaction price
          (a,_,_,mb) <- acctbals
        , let bs = amounts $ normalise mb
          -- mark the last balance in each commodity with True
        , let bs' = concat [reverse $ zip (reverse bs) (True : repeat False)
                           | bs <- groupBy ((==) `on` acommodity) bs]
        , (b, islast) <- bs'
        ]
      
      -- or a final multicommodity posting transferring all balances to equity
      -- (print will show this as multiple single-commodity postings)
      ++ [posting{paccount=closingacct, pamount=if explicit then mapMixedAmount precise totalamt else missingmixedamt} | not interleaved]

    -- the opening transaction
    openingtxn = nulltransaction{tdate=openingdate, tdescription=openingdesc, tpostings=openingps}
    openingps =
      concat [
        [posting{paccount          = a
                ,pamount           = mixed [precise b]
                ,pbalanceassertion =
                    case mcommoditysum of
                      Just s  -> Just nullassertion{baamount=precise s{aprice=Nothing}}
                      Nothing -> Nothing
                }
        ]
        ++ [posting{paccount=openingacct, pamount=Mixed [precise $ negate b]} | interleaved]

        | (a,_,_,mb) <- acctbals
        , let bs = amounts $ normalise mb
          -- mark the last balance in each commodity with the unpriced sum in that commodity (for a balance assertion)
        , let bs' = concat [reverse $ zip (reverse bs) (Just commoditysum : repeat Nothing)
                           | bs <- groupBy ((==) `on` acommodity) bs
                           , let commoditysum = (sum bs)]
        , (b, mcommoditysum) <- bs'
        ]
      ++ [posting{paccount=openingacct, pamount=if explicit then mapMixedAmount precise (negate totalamt) else missingmixedamt} | not interleaved]

  -- print them
  when closing $ putStr $ showTransaction closingtxn
  when opening $ putStr $ showTransaction openingtxn

