{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Close (
  closemode
 ,close
) 
where

import Control.Monad (when)
import Data.Maybe
import Data.Time.Calendar
import System.Console.CmdArgs.Explicit as C

import Hledger
import Hledger.Cli.CliOptions

closemode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Close.txt")
  [flagNone ["opening"] (\opts -> setboolopt "opening" opts) "show just opening transaction"
  ,flagNone ["closing"] (\opts -> setboolopt "closing" opts) "show just closing transaction"
  ]
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[QUERY]")

close CliOpts{rawopts_=rawopts, reportopts_=ropts} j = do
  today <- getCurrentDay
  let 
      (opening, closing) = 
        case (boolopt "opening" rawopts, boolopt "closing" rawopts) of
          (False, False) -> (True, True) -- by default show both opening and closing
          (o, c) -> (o, c)
      ropts_ = ropts{balancetype_=HistoricalBalance, accountlistmode_=ALFlat}
      q = queryFromOpts today ropts_
      openingdate = fromMaybe today $ queryEndDate False q
      closingdate = addDays (-1) openingdate
      (acctbals,_) = balanceReportFromMultiBalanceReport ropts_ q j
      balancingamt = negate $ sum $ map (\(_,_,_,b) -> normaliseMixedAmountSquashPricesForDisplay b) acctbals

      -- since balance assertion amounts are required to be exact, the
      -- amounts in opening/closing transactions should be too (#941)
      -- setprec = setFullPrecision
      setprec = setMinimalPrecision
      -- balance assertion amounts will be unpriced, cf #824
      closingps = [posting{paccount=a
                          ,pamount=mixed [setprec $ negate b]
                          ,pbalanceassertion=Just assertion{baamount=setprec b{aquantity=0, aprice=NoPrice}}
                          }
                  | (a,_,_,mb) <- acctbals
                  , b <- amounts $ normaliseMixedAmountSquashPricesForDisplay mb
                  ]
                  ++ [posting{paccount="equity:closing balances", pamount=negate balancingamt}]

      openingps = [posting{paccount=a
                          ,pamount=mixed [setprec b]
                          ,pbalanceassertion=Just assertion{baamount=setprec b{aprice=NoPrice}}
                          }
                  | (a,_,_,mb) <- acctbals
                  , b <- amounts $ normaliseMixedAmountSquashPricesForDisplay mb
                  ]
                  ++ [posting{paccount="equity:opening balances", pamount=balancingamt}]

  when closing $ putStr $ showTransaction (nulltransaction{tdate=closingdate, tdescription="closing balances", tpostings=closingps})
  when opening $ putStr $ showTransaction (nulltransaction{tdate=openingdate, tdescription="opening balances", tpostings=openingps})

