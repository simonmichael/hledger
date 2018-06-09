{-# LANGUAGE CPP, OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}
-- | Common page components and rendering helpers.
-- For global page layout, see Application.hs.

module Handler.Common where

import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, toGregorian)
import Text.Blaze (ToMarkup)
import Text.Blaze.Internal (preEscapedString)
import Yesod

import Settings (manualurl)

import Hledger

-- -- | Navigation link, preserving parameters and possibly highlighted.
-- navlink :: ViewData -> String -> AppRoute -> String -> HtmlUrl AppRoute
-- navlink VD{..} s dest title = [hamlet|
-- <a##{s}link.#{style} href=@?{u'} title="#{title}">#{s}
-- |]
--   where u' = (dest, if null q then [] else [("q", pack q)])
--         style | dest == here = "navlinkcurrent"
--               | otherwise    = "navlink" :: Text

-- -- | Links to the various journal editing forms.
-- editlinks :: HtmlUrl AppRoute
-- editlinks = [hamlet|
-- <a#editformlink href="#" onclick="return editformToggle(event)" title="Toggle journal edit form">edit
-- \ | #
-- <a#addformlink href="#" onclick="return addformToggle(event)" title="Toggle transaction add form">add
-- <a#importformlink href="#" onclick="return importformToggle(event)" style="display:none;">import transactions
-- |]

-- | Link to a topic in the manual.
helplink :: Text -> Text -> HtmlUrl r
helplink topic label = [hamlet|<a href=#{u} target=hledgerhelp>#{label}|]
  where u = manualurl <> if T.null topic then "" else T.cons '#' topic

----------------------------------------------------------------------
-- hledger report renderers

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
numberTransactionsReportItems [] = []
numberTransactionsReportItems items = number 0 nulldate items
  where
    number :: Int -> Day -> [TransactionsReportItem] -> [(Int, Bool, Bool, TransactionsReportItem)]
    number _ _ [] = []
    number n prevd (i@(t, _, _, _, _, _):rest) = (n+1, newday, newmonth, i): number (n+1) d rest
      where
        d = tdate t
        newday = d /= prevd
        newmonth = dm /= prevdm || dy /= prevdy
        (dy, dm, _) = toGregorian d
        (prevdy, prevdm, _) = toGregorian prevd

mixedAmountAsHtml :: MixedAmount -> HtmlUrl a
mixedAmountAsHtml b = [hamlet|
$forall t <- ts
  <span .#{c}>#{t}
  <br>
|] where
  ts = lines (showMixedAmountWithoutPrice b)
  c = case isNegativeMixedAmount b of
    Just True -> "negative amount" :: Text
    _         -> "positive amount"

showErrors :: ToMarkup a => [a] -> HandlerFor m ()
showErrors errs = setMessage [shamlet|
Errors:<br>
$forall e <- errs
  \#{e}<br>
|]
