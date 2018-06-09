{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings, QuasiQuotes, RecordWildCards, TypeFamilies #-}
-- | Add form data & handler. (The layout and js are defined in
-- Foundation so that the add form can be in the default layout for
-- all views.)

module Handler.AddForm where

import Import

import Control.Monad.State.Strict (evalStateT)
import Data.Either (lefts, rights)
import Data.List (sort)
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void (Void)
import Safe (headMay)
import Text.Megaparsec
import Text.Megaparsec.Char

import Hledger
import Hledger.Cli.Commands.Add (appendToJournalFileOrStdout)

-- Part of the data required from the add form.
-- Don't know how to handle the variable posting fields with yesod-form yet.
-- XXX Variable postings fields
data AddForm = AddForm
    { addFormDate         :: Day
    , addFormDescription  :: Maybe Text
    , addFormJournalFile  :: Maybe Text
    } deriving Show

postAddForm :: Handler Html
postAddForm = do
  let showErrors errs = do
        setMessage [shamlet|
                    Errors:<br>
                    $forall e<-errs
                     \#{e}<br>
                   |]
  -- 1. process the fixed fields with yesod-form

  VD{..} <- getViewData
  let validateJournalFile :: Text -> Either FormMessage Text
      validateJournalFile f
        | T.unpack f `elem` journalFilePaths j = Right f
        | otherwise = Left $ MsgInvalidEntry $ "the selected journal file \"" <> f <> "\"is unknown"

      validateDate :: Text -> Either FormMessage Day
      validateDate s = case fixSmartDateStrEither' today (T.strip s) of
        Right d  -> Right d
        Left _   -> Left $ MsgInvalidEntry $ "could not parse date \"" <> s <> "\":"

  formresult <- runInputPostResult $ AddForm
    <$> ireq (checkMMap (pure . validateDate) (T.pack . show) textField) "date"
    <*> iopt textField "description"
    <*> iopt (check validateJournalFile textField) "journal"

  ok <- case formresult of
    FormMissing      -> showErrors ["there is no form data" :: Text] >> return False
    FormFailure errs -> showErrors errs >> return False
    FormSuccess dat  -> do
      let AddForm{
             addFormDate       =date
            ,addFormDescription=mdesc
            ,addFormJournalFile=mjournalfile
            } = dat
          desc = fromMaybe "" mdesc
          journalfile = maybe (journalFilePath j) T.unpack mjournalfile

      -- 2. the fixed fields look good; now process the posting fields adhocly,
      -- getting either errors or a balanced transaction

      (params,_) <- runRequestBody
      let numberedParams s =
            reverse $ dropWhile (T.null . snd) $ reverse $ sort
            [ (n,v) | (k,v) <- params
                    , let en = parsewith (paramnamep s) k :: Either (ParseError Char Void) Int
                    , isRight en
                    , let Right n = en
                    ]
            where paramnamep s = do {string s; n <- some digitChar; eof; return (read n :: Int)}
          acctparams = numberedParams "account"
          amtparams  = numberedParams "amount"
          num = length acctparams
          paramErrs | num == 0 = ["at least one posting must be entered"]
                    | map fst acctparams == [1..num] &&
                      map fst amtparams `elem` [[1..num], [1..num-1]] = []
                    | otherwise = ["the posting parameters are malformed"]
          eaccts = map (runParser (accountnamep <* eof) "" . textstrip  . snd) acctparams
          eamts  = map (runParser (evalStateT (amountp <* eof) mempty) "" . textstrip . snd) amtparams
          (accts, acctErrs) = (rights eaccts, map show $ lefts eaccts)
          (amts', amtErrs)  = (rights eamts, map show $ lefts eamts)
          amts | length amts' == num = amts'
               | otherwise           = amts' ++ [missingamt]
          errs = if not (null paramErrs) then paramErrs else (acctErrs ++ amtErrs)
          etxn | not $ null errs = Left errs
               | otherwise = either (Left . maybeToList . headMay . lines) Right
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
