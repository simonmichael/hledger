module Hledger.Cli.Commands.Check.Ordereddates (
  journalCheckOrdereddates
)
where

import qualified Data.Text as T
import Hledger
import Hledger.Cli.CliOptions
import Control.Monad (forM)
import Data.List (groupBy)

journalCheckOrdereddates :: CliOpts -> Journal -> Either String ()
journalCheckOrdereddates CliOpts{reportspec_=rspec} j = do
  let 
    ropts = (rsOpts rspec){accountlistmode_=ALFlat}
    -- check date ordering within each file, not across files
    filets = 
      groupBy (\t1 t2 -> transactionFile t1 == transactionFile t2) $
      filter (rsQuery rspec `matchesTransaction`) $
      jtxns $ journalApplyValuationFromOpts rspec j
    checkunique = False -- boolopt "unique" rawopts  XXX was supported by checkdates command
    compare a b = if checkunique then getdate a < getdate b else getdate a <= getdate b
      where getdate = transactionDateFn ropts
  either Left (const $ Right ()) $ 
   forM filets $ \ts ->
    case checkTransactions compare ts of
      FoldAcc{fa_previous=Nothing} -> Right ()
      FoldAcc{fa_error=Nothing}    -> Right ()
      FoldAcc{fa_error=Just error, fa_previous=Just previous} -> do
        let
          datestr = if date2_ ropts then "2" else ""
          uniquestr = if checkunique then " and/or not unique" else ""
          positionstr = showGenericSourcePos $ tsourcepos error
          txn1str = T.unpack . linesPrepend  (T.pack "  ")               $ showTransaction previous
          txn2str = T.unpack . linesPrepend2 (T.pack "> ") (T.pack "  ") $ showTransaction error
        Left $
          "transaction date" <> datestr <> " is out of order"
          <> uniquestr <> "\nat " <> positionstr <> ":\n\n"
          <> txn1str <> txn2str

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
