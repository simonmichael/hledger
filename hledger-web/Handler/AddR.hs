{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.AddR
  ( postAddR
  ) where

import Import

import Control.Monad.State.Strict (evalStateT)
import Data.List (sortBy)
import qualified Data.Text as T
import Data.Void (Void)
import Safe (headMay)
import Text.Megaparsec
import Text.Megaparsec.Char

import Handler.AddForm (AddForm(..), addForm)
import Handler.Common (showErrors)

import Hledger
import Hledger.Cli.Commands.Add (appendToJournalFileOrStdout)

postAddR :: Handler Html
postAddR = do
  -- 1. process the fixed fields with yesod-form
  VD{today, j} <- getViewData
  formresult <- runInputPostResult (addForm today j)

  ok <- case formresult of
    FormMissing      -> showErrors ["there is no form data" :: Text] >> return False
    FormFailure errs -> showErrors errs >> return False
    FormSuccess form -> do
      let journalfile = maybe (journalFilePath j) T.unpack $ addFormJournalFile form

      -- 2. the fixed fields look good; now process the posting fields adhocly,
      -- getting either errors or a balanced transaction
      (params,_) <- runRequestBody
      let acctparams = parseNumberedParameters "account" params
          amtparams  = parseNumberedParameters "amount" params
          pnum = length acctparams
          paramErrs | pnum == 0 = ["at least one posting must be entered"]
                    | map fst acctparams == [1..pnum] &&
                      map fst amtparams `elem` [[1..pnum], [1..pnum-1]] = []
                    | otherwise = ["the posting parameters are malformed"]
          eaccts = map (runParser (accountnamep <* eof) "" . textstrip  . snd) acctparams
          eamts  = map (runParser (evalStateT (amountp <* eof) mempty) "" . textstrip . snd) amtparams
          (accts, acctErrs) = (rights eaccts, map show $ lefts eaccts)
          (amts', amtErrs)  = (rights eamts, map show $ lefts eamts)
          amts | length amts' == pnum = amts'
               | otherwise           = amts' ++ [missingamt]
          errs = if not (null paramErrs) then paramErrs else acctErrs ++ amtErrs
          etxn | not $ null errs = Left errs
               | otherwise = either (Left . maybeToList . headMay . lines) Right
                              (balanceTransaction Nothing $ nulltransaction {
                                  tdate = addFormDate form
                                 ,tdescription = fromMaybe "" $ addFormDescription form
                                 ,tpostings=[nullposting{paccount=acct, pamount=Mixed [amt]} | (acct,amt) <- zip accts amts]
                                 })
      case etxn of
       Left errs' -> showErrors errs' >> return False
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
