{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hledger.Web.Widget.Common
  ( accountQuery
  , accountOnlyQuery
  , balanceReportAsHtml
  , helplink
  , mixedAmountAsHtml
  , fromFormSuccess
  , writeJournalTextIfValidAndChanged
  , journalFile404
  , transactionFragment
  , removeDates
  , removeInacct
  , replaceInacct
  ) where

import Data.Default (def)
import Data.Foldable (find, for_)
import Data.List (elemIndex)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName)
import Text.Blaze ((!), textValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (preEscapedString)
import Text.Hamlet (hamletFile)
import Text.Printf (printf)
import Yesod

import Hledger
import Hledger.Cli.Utils (writeFileWithBackupIfChanged)
import Hledger.Web.Settings (manualurl)
import qualified Hledger.Query as Query

journalFile404 :: FilePath -> Journal -> HandlerFor m (FilePath, Text)
journalFile404 f j =
  case find ((== f) . fst) (jfiles j) of
    Just (_, txt) -> pure (takeFileName f, txt)
    Nothing -> notFound

fromFormSuccess :: Applicative m => m a -> FormResult a -> m a
fromFormSuccess h FormMissing = h
fromFormSuccess h (FormFailure _) = h
fromFormSuccess _ (FormSuccess a) = pure a

-- | A helper for postEditR/postUploadR: check that the given text
-- parses as a Journal, and if so, write it to the given file, if the
-- text has changed. Or, return any error message encountered.
--
-- As a convenience for data received from web forms, which does not
-- have normalised line endings, line endings will be normalised (to \n)
-- before parsing.
--
-- The file will be written (if changed) with the current system's native
-- line endings (see writeFileWithBackupIfChanged).
--
writeJournalTextIfValidAndChanged :: MonadHandler m => FilePath -> Text -> m (Either String ())
writeJournalTextIfValidAndChanged f t = do
  -- Ensure unix line endings, since both readJournal (cf
  -- formatdirectivep, #1194) writeFileWithBackupIfChanged require them.
  -- XXX klunky. Any equivalent of "hSetNewlineMode h universalNewlineMode" for form posts ?
  let t' = T.replace "\r" "" t
  liftIO (readJournal def (Just f) t') >>= \case
    Left e -> return (Left e)
    Right _ -> do
      _ <- liftIO (writeFileWithBackupIfChanged f t')
      return (Right ())

-- | Link to a topic in the manual.
helplink :: Text -> Text -> HtmlUrl r
helplink topic label _ = H.a ! A.href u ! A.target "hledgerhelp" $ toHtml label
  where u = textValue $ manualurl <> if T.null topic then "" else T.cons '#' topic

-- | Render a "BalanceReport" as html.
balanceReportAsHtml :: Eq r => (r, r) -> r -> Bool -> Journal -> Text -> [QueryOpt] -> BalanceReport -> HtmlUrl r
balanceReportAsHtml (journalR, registerR) here hideEmpty j q qopts (items, total) =
  $(hamletFile "templates/balance-report.hamlet")
  where
    l = ledgerFromJournal Any j
    indent a = preEscapedString $ concat $ replicate (2 + 2 * a) "&nbsp;"
    hasSubAccounts acct = maybe True (not . null . asubs) (ledgerAccount l acct)
    matchesAcctSelector acct = Just True == ((`matchesAccount` acct) <$> inAccountQuery qopts)

accountQuery :: AccountName -> Text
accountQuery = ("inacct:" <>) .  quoteIfSpaced

accountOnlyQuery :: AccountName -> Text
accountOnlyQuery = ("inacctonly:" <>) . quoteIfSpaced

mixedAmountAsHtml :: MixedAmount -> HtmlUrl a
mixedAmountAsHtml b _ =
  for_ (lines (showMixedAmountWithoutPrice False b)) $ \t -> do
    H.span ! A.class_ c $ toHtml t
    H.br
  where
    c = case isNegativeMixedAmount b of
      Just True -> "negative amount"
      _ -> "positive amount"

-- Make a slug to uniquely identify this transaction
-- in hyperlinks (as far as possible).
transactionFragment :: Journal -> Transaction -> String
transactionFragment j Transaction{tindex, tsourcepos} = 
  printf "transaction-%d-%d" tfileindex tindex
  where
    -- the numeric index of this txn's file within all the journal files,
    -- or 0 if this txn has no known file (eg a forecasted txn)
    tfileindex = maybe 0 (+1) $ elemIndex (sourceFilePath tsourcepos) (journalFilePaths j)

removeDates :: Text -> [Text]
removeDates =
    map quoteIfSpaced .
    filter (\term ->
        not $ T.isPrefixOf "date:" term || T.isPrefixOf "date2:" term) .
    Query.words'' Query.prefixes

removeInacct :: Text -> [Text]
removeInacct =
    map quoteIfSpaced .
    filter (\term ->
        not $ T.isPrefixOf "inacct:" term || T.isPrefixOf "inacctonly:" term) .
    Query.words'' Query.prefixes

replaceInacct :: Text -> Text -> Text
replaceInacct q acct = T.unwords $ acct : removeInacct q
