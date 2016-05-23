{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings, QuasiQuotes, RecordWildCards #-}
-- | Add form data & handler. (The layout and js are defined in
-- Foundation so that the add form can be in the default layout for
-- all views.)

module Handler.AddForm where

import Import

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.Either (lefts,rights)
import Data.List (sort)
import qualified Data.List as L (head) -- qualified keeps dev & prod builds warning-free
import Data.Text (append, pack, unpack)
import qualified Data.Text as T
import Data.Time.Calendar
import Text.Parsec (digit, eof, many1, string, runParser)

import Hledger.Utils
import Hledger.Data hiding (num)
import Hledger.Read
import Hledger.Cli hiding (num)


-- Part of the data required from the add form.
-- Don't know how to handle the variable posting fields with yesod-form yet.
data AddForm = AddForm
    { addFormDate         :: Day
    , addFormDescription  :: Maybe Text -- String
    -- , addFormPostings     :: [(AccountName, String)]
    , addFormJournalFile  :: Maybe Text -- FilePath
    }
  deriving Show

postAddForm :: Handler Html
postAddForm = do
  let showErrors errs = do
        -- error $ show errs -- XXX uncomment to prevent redirect for debugging
        setMessage [shamlet|
                    Errors:<br>
                    $forall e<-errs
                     \#{e}<br>
                   |]
                                
  -- 1. process the fixed fields with yesod-form

  VD{..} <- getViewData
  let
      validateJournalFile :: Text -> Either FormMessage Text
      validateJournalFile f
        | unpack f `elem` journalFilePaths j = Right f
        | otherwise                          = Left $ MsgInvalidEntry $ pack "the selected journal file \"" `append` f `append` "\"is unknown"

      validateDate :: Text -> Handler (Either FormMessage Day)
      validateDate s = return $
        case fixSmartDateStrEither' today $ strip $ unpack s of
          Right d  -> Right d
          Left _   -> Left $ MsgInvalidEntry $ pack "could not parse date \"" `append` s `append` pack "\":" -- ++ show e)

  formresult <- runInputPostResult $ AddForm
    <$> ireq (checkMMap validateDate (pack . show) textField) "date"
    <*> iopt textField "description"
    <*> iopt (check validateJournalFile textField) "journal"
  
  ok <- case formresult of
    FormMissing      -> showErrors ["there is no form data"::String] >> return False
    FormFailure errs -> showErrors errs >> return False
    FormSuccess dat  -> do
      let AddForm{
             addFormDate       =date
            ,addFormDescription=mdesc
            ,addFormJournalFile=mjournalfile
            } = dat
          desc = maybe "" unpack mdesc
          journalfile = maybe (journalFilePath j) unpack mjournalfile

      -- 2. the fixed fields look good; now process the posting fields adhocly,
      -- getting either errors or a balanced transaction

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
          eamts  = map (runParser (amountp <* eof) mempty "" . strip . T.unpack . snd) amtparams
          (accts, acctErrs) = (rights eaccts, map show $ lefts eaccts)
          (amts', amtErrs)  = (rights eamts, map show $ lefts eamts)
          amts | length amts' == num = amts'
               | otherwise           = amts' ++ [missingamt]
          errs = if not (null paramErrs) then paramErrs else (acctErrs ++ amtErrs)
          etxn | not $ null errs = Left errs
               | otherwise = either (\e -> Left [L.head $ lines e]) Right
                              (balanceTransaction Nothing $ nulltransaction {
                                  tdate=date
                                 ,tdescription=desc
                                 ,tpostings=[nullposting{paccount=acct, pamount=Mixed [amt]} | (acct,amt) <- zip accts amts]
                                 })
      case etxn of
       Left errs -> showErrors errs >> return False
       Right t -> do
        -- 3. all fields look good and form a balanced transaction; append it to the file
        liftIO $ do ensureJournalFileExists journalfile
                    appendToJournalFileOrStdout journalfile $
                      showTransaction $
                      txnTieKnot -- XXX move into balanceTransaction
                      t
        setMessage [shamlet|<span>Transaction added.|]
        return True

  if ok then redirect JournalR else redirect (JournalR, [("add","1")])
