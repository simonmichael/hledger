{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings, QuasiQuotes, RecordWildCards, ScopedTypeVariables, TypeFamilies #-}
-- | Add form data & handler. (The layout and js are defined in
-- Foundation so that the add form can be in the default layout for
-- all views.)

module Handler.AddForm where

import Import

import Control.Monad.State.Strict (evalStateT)
import Data.List (sortBy)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Void (Void)
import Safe (headMay)
import Text.Blaze (ToMarkup)
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

addForm :: Day -> Journal -> FormInput Handler AddForm
addForm today j = AddForm
    <$> ireq (checkMMap (pure . validateDate) (T.pack . show) textField) "date"
    <*> iopt textField "description"
    <*> iopt (check validateJournalFile textField) "journal"
  where
    validateJournalFile :: Text -> Either FormMessage Text
    validateJournalFile f
      | T.unpack f `elem` journalFilePaths j = Right f
      | otherwise = Left $ MsgInvalidEntry $ "the selected journal file \"" <> f <> "\"is unknown"
    validateDate :: Text -> Either FormMessage Day
    validateDate s = case fixSmartDateStrEither' today (T.strip s) of
      Right d  -> Right d
      Left _   -> Left $ MsgInvalidEntry $ "could not parse date \"" <> s <> "\":"

postAddForm :: Handler Html
postAddForm = do
  -- 1. process the fixed fields with yesod-form
  VD{..} <- getViewData
  formresult <- runInputPostResult (addForm today j)

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
      let acctparams = parseNumberedParameters "account" params
          amtparams  = parseNumberedParameters "amount" params
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
        liftIO (appendTransaction journalfile t)
        setMessage [shamlet|<span>Transaction added.|]
        return True

  if ok then redirect JournalR else redirect (JournalR, [("add","1")])

parseNumberedParameters :: Text -> [(Text, Text)] -> [(Int, Text)]
parseNumberedParameters s =
  reverse . dropWhile (T.null . snd) . sortBy (flip compare) . mapMaybe parseNum
  where
    parseNum :: (Text, Text) -> Maybe (Int, Text)
    parseNum (k, v) = case parsewith paramnamep k of
      Left (_ :: ParseError Char Void) -> Nothing
      Right k' -> Just (k', v)
    paramnamep = string s *> (read <$> some digitChar) <* eof

-- XXX move into balanceTransaction
appendTransaction :: FilePath -> Transaction -> IO ()
appendTransaction journalfile t = do
  ensureJournalFileExists journalfile
  appendToJournalFileOrStdout journalfile $
    showTransaction (txnTieKnot t)

showErrors :: ToMarkup a => [a] -> Handler ()
showErrors errs = setMessage [shamlet|
Errors:<br>
$forall e<-errs
  \#{e}<br>
|]
