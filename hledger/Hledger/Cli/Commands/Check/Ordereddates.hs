module Hledger.Cli.Commands.Check.Ordereddates (
  journalCheckOrdereddates
)
where

import Hledger
import Hledger.Cli.CliOptions
import Control.Monad (forM)
import Data.List (groupBy)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Hledger.Read.Error (makeTransactionErrorExcerpt)

-- XXX does this need CliOpts ? Can it move to Hledger.Read.Checks ?
journalCheckOrdereddates :: CliOpts -> Journal -> Either String ()
journalCheckOrdereddates CliOpts{reportspec_=rspec} j = do
  let 
    ropts = (_rsReportOpts rspec){accountlistmode_=ALFlat}
    -- check date ordering within each file, not across files
    filets = 
      groupBy (\t1 t2 -> transactionFile t1 == transactionFile t2) $
      filter (_rsQuery rspec `matchesTransaction`) $
      jtxns $ journalApplyValuationFromOpts rspec j  -- XXX why apply valuation ?
    checkunique = False -- boolopt "unique" rawopts  XXX was supported by checkdates command
    compare a b = if checkunique then getdate a < getdate b else getdate a <= getdate b
      where getdate = transactionDateFn ropts
  either Left (const $ Right ()) $ 
   forM filets $ \ts ->
    case checkTransactions compare ts of
      FoldAcc{fa_previous=Nothing} -> Right ()
      FoldAcc{fa_error=Nothing}    -> Right ()
      FoldAcc{fa_error=Just t, fa_previous=Just tprev} -> Left $ printf
        "%s:%d:%d-%d:\n%stransaction date%s is out of order with previous transaction date %s%s" 
        f l col col2 ex datenum tprevdate oruniquestr
        where
          (f,l,mcols,ex) = makeTransactionErrorExcerpt t finderrcols
          col  = maybe 0 fst mcols
          col2 = maybe 0 (fromMaybe 0 . snd) mcols
          finderrcols _t = Just (1, Just 10)
          datenum   = if date2_ ropts then "2" else ""
          tprevdate = show $ (if date2_ ropts then transactionDate2 else tdate) tprev
          oruniquestr = if checkunique then ", and/or not unique" else ""  -- XXX still used ?

data FoldAcc a b = FoldAcc
 { fa_error    :: Maybe a
 , fa_previous :: Maybe b
 }

checkTransactions :: (Transaction -> Transaction -> Bool)
  -> [Transaction] -> FoldAcc Transaction Transaction
checkTransactions compare = foldWhile f FoldAcc{fa_error=Nothing, fa_previous=Nothing}
  where
    f current acc@FoldAcc{fa_previous=Nothing} = acc{fa_previous=Just current}
    f current acc@FoldAcc{fa_previous=Just previous} =
      if compare previous current
      then acc{fa_previous=Just current}
      else acc{fa_error=Just current}

foldWhile :: (a -> FoldAcc a b -> FoldAcc a b) -> FoldAcc a b -> [a] -> FoldAcc a b
foldWhile _ acc [] = acc
foldWhile fold acc (a:as) =
  case fold a acc of
   acc@FoldAcc{fa_error=Just _} -> acc
   acc -> foldWhile fold acc as
