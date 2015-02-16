-- | Add form data & handler. (The layout and js are defined in
-- Foundation so that the add form can be in the default layout for
-- all views.)

module Handler.AddForm where

import Import

import Control.Applicative
import Data.Either (lefts,rights)
import Data.List (sort)
import qualified Data.List as L (head) -- qualified keeps dev & prod builds warning-free
-- import Data.Maybe
import Data.Text (unpack)
import qualified Data.Text as T
-- import Data.Time.Calendar
import Text.Parsec (digit, eof, many1, string, runParser)
-- import Yesod.Form.Jquery

import Hledger.Utils
import Hledger.Data hiding (num)
import Hledger.Read
import Hledger.Cli hiding (num)


-- Part of the data required from the add form.
-- Don't know how to handle the variable posting fields with yesod-form yet.
data AddForm = AddForm
    { addFormJournalFile  :: Maybe Text -- FilePath
    , addFormDate         :: Maybe Text -- Day
    , addFormDescription  :: Maybe Text -- String
    -- , addFormPostings     :: [(AccountName, String)]
    }
  deriving Show

postAddForm :: Handler Html
postAddForm = do
  VD{..} <- getViewData
  let showErrors errs = do
        -- error $ show errs -- XXX uncomment to prevent redirect, for debugging
        setMessage [shamlet|
                     Error:<br>
                     $forall e<-errs
                      \#{e}<br>
                   |]
  formresult <- runInputPostResult $ AddForm
    <$> iopt textField "journal"
    <*> iopt textField "date"
        -- (jqueryDayField def
        -- {
        --   jdsChangeYear = True     -- give a year dropdown
        -- , jdsYearRange = "1900:-5" -- 1900 till five years ago
        -- }) "date"
    <*> iopt textField "description"
  case formresult of
    FormMissing          -> showErrors ["there is no form data"::String]
    FormFailure errs     -> showErrors errs
    FormSuccess formdata -> do
      let AddForm{
             addFormJournalFile=mjournalfile
            ,addFormDate       =mdate
            ,addFormDescription=mdesc
            } = formdata
          date = parsedate $ fixSmartDateStr today $ maybe "today" (strip . unpack) mdate
          desc = maybe "" unpack mdesc
          journalfile = maybe
                        (journalFilePath j)
                        (\f' -> let f = unpack f' in
                                if f `elem` journalFilePaths j
                                then f
                                else error $ "the selected journal file is unknown: " ++ f)
                        mjournalfile

      (params,_) <- runRequestBody
      let numberedParams s =
            reverse $ dropWhile (T.null . snd) $ reverse $ sort
            [ (n,v) | (k,v) <- params
                    , let en = parsewith (paramnamep s) $ T.unpack k
                    , isRight en
                    , let Right n = en
                    ]
            where paramnamep s = do {string s; n <- many1 digit; eof; return (read n :: Int)}
          acctparams = numberedParams "account"
          amtparams  = numberedParams "amount"
          num = length acctparams
          paramErrs | num == 0 = ["at least one posting must be entered"]
                    | map fst acctparams == [1..num] &&
                      map fst amtparams `elem` [[1..num], [1..num-1]] = []
                    | otherwise = ["the posting parameters are malformed"]
          eaccts = map (parsewith (accountnamep <* eof) . strip . T.unpack . snd) acctparams
          eamts  = map (runParser (amountp <* eof) nullctx "" . strip . T.unpack . snd) amtparams
          (accts, acctErrs) = (rights eaccts, map show $ lefts eaccts)
          (amts', amtErrs)  = (rights eamts, map show $ lefts eamts)
          amts | length amts' == num = amts'
               | otherwise           = amts' ++ [missingamt]
          -- if no errors so far, generate a transaction and balance it or get the error.
          errs = if not (null paramErrs) then paramErrs else (acctErrs ++ amtErrs)
          et | not $ null errs = Left errs
             | otherwise = either (\e -> Left [L.head $ lines e]) Right
                            (balanceTransaction Nothing $ nulltransaction {
                                tdate=date
                               ,tdescription=desc
                               ,tpostings=[nullposting{paccount=acct, pamount=Mixed [amt]} | (acct,amt) <- zip accts amts]
                               })
      -- display errors or add transaction
      case et of
       Left errs -> showErrors errs
       Right t -> do
        let t' = txnTieKnot t -- XXX move into balanceTransaction
        liftIO $ do ensureJournalFileExists journalfile
                    appendToJournalFileOrStdout journalfile $ showTransaction t'
        setMessage [shamlet|<span>Transaction added.|]

  redirect (JournalR) -- , [("add","1")])
