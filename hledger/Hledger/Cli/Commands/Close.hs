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

defclosingacct = "equity:closing balances"
defopeningacct = "equity:opening balances"

closemode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Close.txt")
  [flagNone ["closing"] (setboolopt "closing") "show just closing transaction"
  ,flagNone ["opening"] (setboolopt "opening") "show just opening transaction"
  ,flagReq  ["close-to"] (\s opts -> Right $ setopt "close-to" s opts) "ACCT" ("account to transfer closing balances to (default: "++defclosingacct++")")
  ,flagReq  ["open-from"] (\s opts -> Right $ setopt "open-from" s opts) "ACCT" ("account to transfer opening balances from (default: "++defopeningacct++")")
  ,flagNone ["interleaved"] (setboolopt "interleaved") "keep equity and non-equity postings adjacent"
  ,flagNone ["show-costs"] (setboolopt "show-costs") "keep balances with different costs separate"
  ]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- debugger, beware: close is incredibly devious. simple rules combine to make a horrid maze.
-- tests are in tests/close.test.
close CliOpts{rawopts_=rawopts, reportopts_=ropts} j = do
  today <- getCurrentDay
  let
    -- show opening entry, closing entry, or (default) both ?
    (opening, closing) =
      case (boolopt "opening" rawopts, boolopt "closing" rawopts) of
        (False, False) -> (True, True)
        (o, c)         -> (o, c)

    -- accounts to close to and open from
    -- if a name is specified for only one, it is used for both
    (closingacct, openingacct) =
      let (mc, mo) = (T.pack <$> maybestringopt "close-to" rawopts, T.pack <$> maybestringopt "open-from" rawopts)
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
    closingtxn = nulltransaction{tdate=closingdate, tdescription="closing balances", tpostings=closingps}
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
      ++ [posting{paccount=closingacct, pamount=mapMixedAmount precise totalamt} | not interleaved]

    -- the opening transaction
    openingtxn = nulltransaction{tdate=openingdate, tdescription="opening balances", tpostings=openingps}
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
      ++ [posting{paccount=openingacct, pamount=mapMixedAmount precise $ negate totalamt} | not interleaved]

  -- print them
  when closing $ putStr $ showTransaction closingtxn
  when opening $ putStr $ showTransaction openingtxn

