{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.AddR
  ( postAddR
  ) where

import Import

import Control.Monad.State.Strict (evalStateT)
import Data.List (dropWhileEnd, sort)
import qualified Data.Text as T
import Data.Void (Void)
import Safe (headMay)
import Text.Megaparsec
import Text.Megaparsec.Char

import Handler.AddForm (AddForm(..), addForm)
import Handler.Common (showErrors)

import Hledger
import Hledger.Cli.Commands.Add (appendToJournalFileOrStdout)

postAddR :: Handler ()
postAddR = do
  VD{today, j} <- getViewData
  -- 1. process the fixed fields with yesod-form
  runInputPostResult (addForm today j) >>= \case
    FormMissing      -> bail ["there is no form data"]
    FormFailure errs -> bail errs
    FormSuccess form -> do
      let journalfile = maybe (journalFilePath j) T.unpack $ addFormJournalFile form
      -- 2. the fixed fields look good; now process the posting fields adhocly,
      -- getting either errors or a balanced transaction
      (params,_) <- runRequestBody
      let acctparams = parseNumberedParameters "account" params
          amtparams  = parseNumberedParameters "amount" params
          pnum = length acctparams
      when (pnum == 0) (bail ["at least one posting must be entered"])
      when (map fst acctparams /= [1..pnum] || map fst amtparams `notElem` [[1..pnum], [1..pnum-1]])
        (bail ["the posting parameters are malformed"])

      let eaccts = runParser (accountnamep <* eof) "" . textstrip  . snd <$> acctparams
          eamts  = runParser (evalStateT (amountp <* eof) mempty) "" . textstrip . snd <$> amtparams
          (acctErrs, accts) = partitionEithers eaccts
          (amtErrs, amts')  = partitionEithers eamts
          amts | length amts' == pnum = amts'
               | otherwise = amts' ++ [missingamt]
          errs = T.pack . parseErrorPretty <$> acctErrs ++ amtErrs
      unless (null errs) (bail errs)

      let etxn = balanceTransaction Nothing $ nulltransaction
            { tdate = addFormDate form
            , tdescription = fromMaybe "" $ addFormDescription form
            , tpostings = (\(ac, am) -> nullposting {paccount = ac, pamount = Mixed [am]}) <$> zip accts amts
            }
      case etxn of
       Left errs' -> bail (fmap T.pack . maybeToList . headMay $ lines errs')
       Right t -> do
        -- 3. all fields look good and form a balanced transaction; append it to the file
        liftIO (appendTransaction journalfile t)
        setMessage "Transaction added."
        redirect JournalR
  where
    bail :: [Text] -> Handler ()
    bail xs = showErrors xs >> redirect (JournalR, [("add","1")])

parseNumberedParameters :: Text -> [(Text, Text)] -> [(Int, Text)]
parseNumberedParameters s =
  dropWhileEnd (T.null . snd) . sort . mapMaybe parseNum
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
