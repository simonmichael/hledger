#!/usr/bin/env stack
{- stack runghc --verbosity info
  --package hledger
-}

import Data.Maybe
import Data.Time
import qualified Data.Text as T
import Control.Monad
import Hledger.Cli

cmdmode :: Mode RawOpts
cmdmode = (defCommandMode ["hledger-prices"]) {
     modeArgs = ([], Nothing)
    ,modeHelp = "print all prices from journal"
    ,modeGroupFlags = Group {
         groupNamed = [
             ("Input",     inputflags)
            ,("Misc",      helpflags)
            ]
        ,groupHidden = []
        ,groupUnnamed = [
             flagNone ["costs"] (setboolopt "costs")
                "collect prices from postings"
            ]
        }
    }

showPrice :: MarketPrice -> String
showPrice mp = unwords ["P", show $ mpdate mp, T.unpack . quoteCommoditySymbolIfNeeded $ mpcommodity mp, showAmountWithZeroCommodity $ mpamount mp]

amountCost :: Day -> Amount -> Maybe MarketPrice
amountCost d a =
    case aprice a of
        NoPrice -> Nothing
        UnitPrice pa -> Just
            MarketPrice { mpdate = d, mpcommodity = acommodity a, mpamount = pa }
        TotalPrice pa -> Just
            MarketPrice { mpdate = d, mpcommodity = acommodity a, mpamount = pa `divideAmount` abs (aquantity a) }

postingCosts :: Posting -> [MarketPrice]
postingCosts p = mapMaybe (amountCost date) . amounts $ pamount p  where
   date = fromMaybe (tdate . fromJust $ ptransaction p) $ pdate p

allPostsings :: Journal -> [Posting]
allPostsings = concatMap tpostings . jtxns

main :: IO ()
main = do
    opts <- getCliOpts cmdmode
    withJournalDo opts{ ignore_assertions_ = True } $ \_ j -> do
        let cprices = concatMap postingCosts . allPostsings $ j
            printPrices = mapM_ (putStrLn . showPrice)
        when (boolopt "costs" $ rawopts_ opts) $ printPrices cprices
        printPrices $ jmarketprices j
