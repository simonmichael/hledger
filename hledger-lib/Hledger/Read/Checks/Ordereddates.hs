module Hledger.Read.Checks.Ordereddates (
  journalCheckOrdereddates
)
where

import Control.Monad (forM)
import Data.List (groupBy)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)

import Hledger.Data
import Hledger.Read.Error

journalCheckOrdereddates :: WhichDate -> Journal -> Either String ()
journalCheckOrdereddates whichdate j = do
  let 
    -- we check date ordering within each file, not across files
    -- note, relying on txns always being sorted by file here
    txnsbyfile = groupBy (\t1 t2 -> transactionFile t1 == transactionFile t2) $ jtxns j
    getdate = transactionDateOrDate2 whichdate
    compare a b = getdate a <= getdate b
  either Left (const $ Right ()) $ 
   forM txnsbyfile $ \ts ->
    case checkTransactions compare ts of
      FoldAcc{fa_previous=Nothing} -> Right ()
      FoldAcc{fa_error=Nothing}    -> Right ()
      FoldAcc{fa_error=Just t, fa_previous=Just tprev} -> Left $ printf
        "%s:%d:%d-%d:\n%stransaction date%s is out of order with previous transaction date %s" 
        f l col col2 ex datenum tprevdate
        where
          (f,l,mcols,ex) = makeTransactionErrorExcerpt t finderrcols
          col  = maybe 0 fst mcols
          col2 = maybe 0 (fromMaybe 0 . snd) mcols
          finderrcols _t = Just (1, Just 10)
          datenum   = if whichdate==SecondaryDate then "2" else ""
          tprevdate = show $ getdate tprev

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
