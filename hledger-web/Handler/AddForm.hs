-- | POST helpers.

module Handler.AddForm where

import Import

import Control.Applicative
import Data.Either (lefts,rights)
import Data.List (sort)
import qualified Data.List as L (head) -- qualified keeps dev & prod builds warning-free
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Parsec (digit, eof, many1, string, runParser)

import Hledger.Utils
import Hledger.Data hiding (num)
import Hledger.Read
import Hledger.Cli hiding (num)


-- | Handle a post from the transaction add form.
postAddForm :: Handler Html
postAddForm = do
  VD{..} <- getViewData
  -- XXX gruesome form handling, port to yesod-form. cf #234
  mjournalpath <- lookupPostParam  "journal"
  mdate <- lookupPostParam  "date"
  mdesc <- lookupPostParam  "description"
  let edate = maybe (Left "date required") (either (\e -> Left $ showDateParseError e) Right . fixSmartDateStrEither today . strip . unpack) mdate
      edesc = Right $ maybe "" unpack mdesc
      ejournalpath = maybe
                       (Right $ journalFilePath j)
                       (\f -> let f' = unpack f in
                              if f' `elem` journalFilePaths j
                              then Right f'
                              else Left $ "unrecognised journal file path: " ++ f'
                              )
                       mjournalpath
      estrs = [edate, edesc, ejournalpath]
      (errs1, [date, desc, journalpath]) = case (lefts estrs, rights estrs) of
        ([], [_,_,_]) -> ([], rights estrs)
        _             -> (lefts estrs, [error "",error "",error ""]) -- RHS won't be used
  (params,_) <- runRequestBody
  -- mtrace params
  let paramnamep s = do {string s; n <- many1 digit; eof; return (read n :: Int)}
      numberedParams s =
        reverse $ dropWhile (T.null . snd) $ reverse $ sort
        [ (n,v) | (k,v) <- params
                , let en = parsewith (paramnamep s) $ T.unpack k
                , isRight en
                , let Right n = en
                ]
      acctparams = numberedParams "account"
      amtparams  = numberedParams "amount"
      num = length acctparams
      paramErrs | map fst acctparams == [1..num] &&
                  map fst amtparams `elem` [[1..num], [1..num-1]] = []
                | otherwise = ["malformed account/amount parameters"]
      eaccts = map (parsewith (accountnamep <* eof) . strip . T.unpack . snd) acctparams
      eamts  = map (runParser (amountp <* eof) nullctx "" . strip . T.unpack . snd) amtparams
      (accts, acctErrs) = (rights eaccts, map show $ lefts eaccts)
      (amts', amtErrs)  = (rights eamts, map show $ lefts eamts)
      amts | length amts' == num = amts'
           | otherwise           = amts' ++ [missingamt]
      -- if no errors so far, generate a transaction and balance it or get the error.
      errs = errs1 ++ if not (null paramErrs) then paramErrs else (acctErrs ++ amtErrs)
      et | not $ null errs = Left errs
         | otherwise = either (\e -> Left ["unbalanced postings: " ++ (L.head $ lines e)]) Right
                        (balanceTransaction Nothing $ nulltransaction {
                            tdate=parsedate date
                           ,tdescription=desc
                           ,tpostings=[nullposting{paccount=acct, pamount=Mixed [amt]} | (acct,amt) <- zip accts amts]
                           })
  -- display errors or add transaction
  case et of
   Left errs' -> do
    error $ show errs' -- XXX
    -- save current form values in session
    -- setMessage $ toHtml $ intercalate "; " errs
    setMessage [shamlet|
                 Errors:<br>
                 $forall e<-errs'
                  \#{e}<br>
               |]
   Right t -> do
    let t' = txnTieKnot t -- XXX move into balanceTransaction
    liftIO $ do ensureJournalFileExists journalpath
                appendToJournalFileOrStdout journalpath $ showTransaction t'
    setMessage [shamlet|<span>Transaction added.|]

  redirect (JournalR) -- , [("add","1")])
