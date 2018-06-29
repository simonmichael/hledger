{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Widget.Common
  ( accountQuery
  , accountOnlyQuery
  , balanceReportAsHtml
  , helplink
  , mixedAmountAsHtml
  , fromFormSuccess
  , writeValidJournal
  , journalFile404
  ) where

import Data.Default (def)
import Data.Foldable (find, for_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeFileName)
import Text.Blaze ((!), textValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (preEscapedString)
import Text.Hamlet (hamletFile)
import Yesod

import Hledger
import Hledger.Cli.Utils (writeFileWithBackupIfChanged)
import Hledger.Web.Settings (manualurl)

#if MIN_VERSION_yesod(1,6,0)
journalFile404 :: FilePath -> Journal -> HandlerFor m (FilePath, Text)
#else
journalFile404 :: FilePath -> Journal -> HandlerT m IO (FilePath, Text)
#endif
journalFile404 f j =
  case find ((== f) . fst) (jfiles j) of
    Just (_, txt) -> pure (takeFileName f, txt)
    Nothing -> notFound

fromFormSuccess :: Applicative m => m a -> FormResult a -> m a
fromFormSuccess h FormMissing = h
fromFormSuccess h (FormFailure _) = h
fromFormSuccess _ (FormSuccess a) = pure a

writeValidJournal :: MonadHandler m => FilePath -> Text -> m (Either String ())
writeValidJournal f txt =
  liftIO (readJournal def (Just f) txt) >>= \case
    Left e -> return (Left e)
    Right _ -> do
      _ <- liftIO (writeFileWithBackupIfChanged f txt)
      return (Right ())


-- | Link to a topic in the manual.
helplink :: Text -> Text -> HtmlUrl r
helplink topic label _ = H.a ! A.href u ! A.target "hledgerhelp" $ toHtml label
  where u = textValue $ manualurl <> if T.null topic then "" else T.cons '#' topic

-- | Render a "BalanceReport" as html.
balanceReportAsHtml :: Eq r => (r, r) -> r -> Bool -> Journal -> [QueryOpt] -> BalanceReport -> HtmlUrl r
balanceReportAsHtml (journalR, registerR) here hideEmpty j qopts (items, total) =
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
  for_ (lines (showMixedAmountWithoutPrice b)) $ \t -> do
    H.span ! A.class_ c $ toHtml t
    H.br
  where
    c = case isNegativeMixedAmount b of
      Just True -> "negative amount"
      _ -> "positive amount"
