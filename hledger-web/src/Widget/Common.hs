{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Widget.Common
  ( accountQuery
  , accountOnlyQuery
  , balanceReportAsHtml
  , helplink
  , mixedAmountAsHtml
  , numberTransactionsReportItems
  ) where

import Data.Foldable (for_)
import Data.List (mapAccumL)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, toGregorian)
import Text.Blaze
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (preEscapedString)
import Yesod

import Hledger
import Settings (manualurl)

-- | Link to a topic in the manual.
helplink :: Text -> Text -> HtmlUrl r
helplink topic label _ = H.a ! A.href u ! A.target "hledgerhelp" $ toHtml label
  where u = textValue $ manualurl <> if T.null topic then "" else T.cons '#' topic

-- | Render a "BalanceReport" as html.
balanceReportAsHtml :: r -> Journal -> [QueryOpt] -> BalanceReport -> HtmlUrl r
balanceReportAsHtml registerR j qopts (items, total) = [hamlet|
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

numberTransactionsReportItems :: [TransactionsReportItem] -> [(Int, Bool, Bool, TransactionsReportItem)]
numberTransactionsReportItems = snd . mapAccumL number (0, nulldate)
  where
    number :: (Int, Day) -> TransactionsReportItem -> ((Int, Day), (Int, Bool, Bool, TransactionsReportItem))
    number (!n, !prevd) i@(t, _, _, _, _, _) = ((n', d), (n', newday, newmonth, i))
      where
        n' = n + 1
        d = tdate t
        newday = d /= prevd
        newmonth = dm /= prevdm || dy /= prevdy
        (dy, dm, _) = toGregorian d
        (prevdy, prevdm, _) = toGregorian prevd

mixedAmountAsHtml :: MixedAmount -> HtmlUrl a
mixedAmountAsHtml b _ =
  for_ (lines (showMixedAmountWithoutPrice b)) $ \t -> do
    H.span ! A.class_ c $ toHtml t
    H.br
  where
    c = case isNegativeMixedAmount b of
      Just True -> "negative amount"
      _ -> "positive amount"
