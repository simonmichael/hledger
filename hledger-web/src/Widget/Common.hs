{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Widget.Common
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
import Yesod

import Hledger
import Hledger.Cli.Utils (writeFileWithBackupIfChanged)
import Settings (manualurl)

journalFile404 :: FilePath -> Journal -> HandlerFor m (FilePath, Text)
journalFile404 f j =
  case find ((== f) . fst) (jfiles j) of
    Just (_, txt) -> pure (takeFileName f, txt)
    Nothing -> notFound

fromFormSuccess :: HandlerFor m a -> FormResult a -> HandlerFor m a
fromFormSuccess h FormMissing = h
fromFormSuccess h (FormFailure _) = h
fromFormSuccess _ (FormSuccess a) = return a

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
balanceReportAsHtml :: Eq r => (r, r) -> r -> Journal -> [QueryOpt] -> BalanceReport -> HtmlUrl r
balanceReportAsHtml (journalR, registerR) here j qopts (items, total) = [hamlet|
<tr :here == journalR:.inacct>
  <td .top .acct>
    <a href=@{journalR} :here == journalR:.inacct
       title="Show general journal entries, most recent first">
      Journal
  <td .top>
$forall (acct, adisplay, aindent, abal) <- items
  <tr .#{inacctClass acct}>
    <td .acct>
      <div .ff-wrapper>
        \#{indent aindent}
        <a href="@?{acctLink acct}" .#{inacctClass acct}
           title="Show transactions affecting this account and subaccounts">
          #{adisplay}
        $if hasSubs acct
          <a href="@?{acctOnlyLink acct}" .only .hidden-sm .hidden-xs
             title="Show transactions affecting this account but not subaccounts">only
    <td>
      ^{mixedAmountAsHtml abal}
<tr .total>
  <td>
  <td>
    ^{mixedAmountAsHtml total}
|] where
  l = ledgerFromJournal Any j
  inacctClass acct = case inAccountQuery qopts of
    Just m' -> if m' `matchesAccount` acct then "inacct" else ""
    Nothing -> "" :: Text
  hasSubs acct = maybe True (not . null . asubs) (ledgerAccount l acct)
  indent a = preEscapedString $ concat $ replicate (2 + 2 * a) "&nbsp;"
  acctLink acct = (registerR, [("q", accountQuery acct)])
  acctOnlyLink acct = (registerR, [("q", accountOnlyQuery acct)])

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
