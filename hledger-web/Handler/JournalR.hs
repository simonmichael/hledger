{-# LANGUAGE OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}
-- | /journal handlers.

module Handler.JournalR where

import Import

import Handler.Common
       (accountQuery, hledgerLayout, mixedAmountAsHtml,
        numberTransactionsReportItems)

import Hledger.Cli.CliOptions
import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Utils
import Hledger.Web.WebOptions

-- | The formatted journal view, with sidebar.
getJournalR :: Handler Html
getJournalR = do
  vd@VD{j, m, opts, qopts} <- getViewData
  -- XXX like registerReportAsHtml
  let title = case inAccount qopts of
        Nothing -> "General Journal"
        Just (a, inclsubs) -> "Transactions in " <> a <> if inclsubs then "" else " (excluding subaccounts)"
      title' = title <> if m /= Any then ", filtered" else ""
      maincontent = transactionsReportAsHtml $ journalTransactionsReport (reportopts_ $ cliopts_ opts) j m
  hledgerLayout vd "journal" [hamlet|
       <div .row>
        <h2 #contenttitle>#{title'}
        <!-- p>Journal entries record movements of commodities between accounts. -->
        <a #addformlink role="button" style="cursor:pointer; margin-top:1em;" data-toggle="modal" data-target="#addmodal" title="Add a new transaction to the journal" href="#">Add a transaction
       <div .table-responsive>
        ^{maincontent}
     |]

-- | Render a "TransactionsReport" as html for the formatted journal view.
transactionsReportAsHtml :: (w, [TransactionsReportItem]) -> HtmlUrl AppRoute
transactionsReportAsHtml (_,items) = [hamlet|
<table .transactionsreport .table .table-condensed>
 <thead>
  <th .date style="text-align:left;">
   Date
  <th .description style="text-align:left;">Description
  <th .account style="text-align:left;">Account
  <th .amount style="text-align:right;">Amount
 $forall i <- numberTransactionsReportItems items
  ^{transactionReportItem i}
 |]

transactionReportItem :: (Int, Bool, Bool, Bool, TransactionsReportItem) -> HtmlUrl AppRoute
transactionReportItem (_, _, _, _, (torig, _, split, _, amt, _)) = [hamlet|
<tr .title #transaction-#{tindex torig}>
 <td .date nowrap>#{date}
 <td .description colspan=2>#{textElideRight 60 desc}
 <td .amount style="text-align:right;">
  $if showamt
   \#{mixedAmountAsHtml amt}
$forall p' <- tpostings torig
 <tr .item .posting title="#{show torig}">
  <td .nonhead>
  <td .nonhead>
  <td .nonhead>
   &nbsp;
   <a href="@?{acctlink (paccount p')}##{tindex torig}" title="#{paccount p'}">#{elideAccountName 40 $ paccount p'}
  <td .amount .nonhead style="text-align:right;">#{mixedAmountAsHtml $ pamount p'}
|]
     where
       acctlink a = (RegisterR, [("q", accountQuery a)])
       (date, desc) = (show $ tdate torig, tdescription torig)
       showamt = not split || not (isZeroMixedAmount amt)

