{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
-- | /journal handlers.

module Handler.JournalR where

import Import

import Handler.AddForm
import Handler.Common

import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Utils
import Hledger.Cli.Options
import Hledger.Web.Options

-- | The formatted journal view, with sidebar.
getJournalR :: Handler Html
getJournalR = do
  vd@VD{..} <- getViewData
  let -- XXX like registerReportAsHtml
      inacct = inAccount qopts
      -- injournal = isNothing inacct
      filtering = m /= Any
      -- showlastcolumn = if injournal && not filtering then False else True
      title = case inacct of
                Nothing       -> "General Journal"++s2
                Just (a,inclsubs) -> "Transactions in "++a++s1++s2
                                      where s1 = if inclsubs then "" else " (excluding subaccounts)"
                where
                  s2 = if filtering then ", filtered" else ""
      maincontent = journalTransactionsReportAsHtml opts vd $ journalTransactionsReport (reportopts_ $ cliopts_ opts) j m
  hledgerLayout vd "journal" [hamlet|
       <h2#contenttitle>#{title}
       <!-- p>Journal entries record movements of commodities between accounts. -->
       <a#addformlink role="button" style="cursor:pointer;" onClick="addformReset(true);" data-toggle="modal" data-target="#addmodal"  title="Add a new transaction to the journal" style="margin-top:1em;">Add a transaction
       ^{maincontent}
     |]

postJournalR :: Handler Html
postJournalR = postAddForm

-- | Render a "TransactionsReport" as html for the formatted journal view.
journalTransactionsReportAsHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
journalTransactionsReportAsHtml _ vd (_,items) = [hamlet|
<table.transactionsreport>
 <tr.headings>
  <th.date style="text-align:left;">
   Date
   <span .glyphicon .glyphicon-chevron-up>
  <th.description style="text-align:left;">Description
  <th.account style="text-align:left;">Account
  <th.amount style="text-align:right;">Amount
 $forall i <- numberTransactionsReportItems items
  ^{itemAsHtml vd i}
 |]
 where
-- .#{datetransition}
   itemAsHtml :: ViewData -> (Int, Bool, Bool, Bool, TransactionsReportItem) -> HtmlUrl AppRoute
   itemAsHtml VD{..} (n, _, _, _, (t, _, split, _, amt, _)) = [hamlet|
<tr ##{date} .item.#{evenodd}.#{firstposting} style="vertical-align:top;" title="#{show t}">
 <td.date>#{date}
 <td.description colspan=2>#{elideRight 60 desc}
 <td.amount style="text-align:right;">
  $if showamt
   \#{mixedAmountAsHtml amt}
$forall p' <- tpostings t
 <tr .item.#{evenodd}.posting title="#{show t}">
  <td.date>
  <td.description>
  <td.account>
   &nbsp;
   <a href="/register?q=inacct:'#{paccount p'}'##{date}" title="#{paccount p'}">#{elideAccountName 40 $ paccount p'}
  <td.amount style="text-align:right;">#{mixedAmountAsHtml $ pamount p'}
<tr.#{evenodd}>
 <td>&nbsp;
 <td>
 <td>
 <td>
|]
     where
       evenodd = if even n then "even" else "odd" :: String
       -- datetransition | newm = "newmonth"
       --                | newd = "newday"
       --                | otherwise = "" :: String
       (firstposting, date, desc) = (False, show $ tdate t, tdescription t)
       -- acctquery = (here, [("q", pack $ accountQuery acct)])
       showamt = not split || not (isZeroMixedAmount amt)

