{-# LANGUAGE NoOverloadedStrings #-} -- prevent trouble if turned on in ghci
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Checkdates (
  checkdatesmode
 ,checkdates
) where

import Hledger
import Hledger.Cli.CliOptions
import System.Console.CmdArgs.Explicit
import Text.Printf

checkdatesmode :: Mode RawOpts
checkdatesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Checkdates.txt")
  [flagNone ["strict"] (setboolopt "strict") "makes date comparing strict"]
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

checkdates :: CliOpts -> Journal -> IO ()
checkdates CliOpts{rawopts_=rawopts,reportopts_=ropts} j = do
  d <- getCurrentDay
  let ropts_ = ropts{accountlistmode_=ALFlat}
  let q = queryFromOpts d ropts_
  let ts = filter (q `matchesTransaction`) $
           jtxns $ journalSelectingAmountFromOpts ropts j
  let strict = boolopt "strict" rawopts
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
checkTransactions compare = foldWhile f FoldAcc{fa_error=Nothing, fa_previous=Nothing}
  where
    f current acc@FoldAcc{fa_previous=Nothing} = acc{fa_previous=Just current}
    f current acc@FoldAcc{fa_previous=Just previous} =
      if compare previous current
      then acc{fa_previous=Just current}
      else acc{fa_error=Just current}
