#!/usr/bin/env stack
{- stack runghc --verbosity info
   --package hledger-lib
   --package hledger
   --package here
-}

{-# LANGUAGE QuasiQuotes #-}

import Data.String.Here
import Hledger
import Hledger.Cli
import Text.Printf

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [here| check-dates
Check that transactions' dates are monotonically increasing.
With --date2, checks secondary dates instead.
With --strict, dates must also be unique.
With a query, only matched transactions' dates are checked.
Reads the default journal file, or another specified with -f.
FLAGS
  |]
  [flagNone ["strict"] (\opts -> setboolopt "strict" opts) "makes date comparing strict"]
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "[QUERY]")
------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- getHledgerCliOpts cmdmode
  withJournalDo opts $
   \CliOpts{rawopts_=opts,reportopts_=ropts} j -> do
    d <- getCurrentDay
    let ropts_ = ropts{accountlistmode_=ALFlat}
    let q = queryFromOpts d ropts_
    let ts = filter (q `matchesTransaction`) $
             jtxns $ journalSelectingAmountFromOpts ropts j
    let strict = boolopt "strict" opts
    let date = transactionDateFn ropts
    let compare a b =
          if strict
          then date a <  date b
          else date a <= date b
    case checkTransactions compare ts of
     FoldAcc{fa_previous=Nothing} -> putStrLn "ok (empty journal)"
     FoldAcc{fa_error=Nothing}    -> putStrLn "ok"
     FoldAcc{fa_error=Just error, fa_previous=Just previous} ->
      putStrLn $ printf ("ERROR: transaction out of%s date order"
       ++ "\nPrevious date: %s"
       ++ "\nDate: %s"
       ++ "\nLocation: %s"
       ++ "\nTransaction:\n\n%s")
       (if strict then " STRICT" else "")
       (show $ date previous)
       (show $ date error)
       (show $ tsourcepos error)
       (showTransactionUnelided error)

data FoldAcc a b = FoldAcc
 { fa_error    :: Maybe a
 , fa_previous :: Maybe b
 }

foldWhile :: (a -> FoldAcc a b -> FoldAcc a b) -> FoldAcc a b -> [a] -> FoldAcc a b
foldWhile _ acc [] = acc
foldWhile fold acc (a:as) =
  case fold a acc of
   acc@FoldAcc{fa_error=Just _} -> acc
   acc -> foldWhile fold acc as

checkTransactions :: (Transaction -> Transaction -> Bool)
 -> [Transaction] -> FoldAcc Transaction Transaction
checkTransactions compare ts =
  foldWhile fold FoldAcc{fa_error=Nothing, fa_previous=Nothing} ts
  where
    fold current acc@FoldAcc{fa_previous=Nothing} = acc{fa_previous=Just current}
    fold current acc@FoldAcc{fa_previous=Just previous} =
      if compare previous current
      then acc{fa_previous=Just current}
      else acc{fa_error=Just current}

