{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
-- | /journal handlers.

module Handler.JournalR where

-- import Data.Text (Text)
import qualified Data.Text as T
import Import

import Handler.AddForm
import Handler.Common

import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Utils
import Hledger.Cli.CliOptions
import Hledger.Web.WebOptions

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
                Just (a,inclsubs) -> "Transactions in "++T.unpack a++s1++s2
                                      where s1 = if inclsubs then "" else " (excluding subaccounts)"
                where
                  s2 = if filtering then ", filtered" else ""
      maincontent = journalTransactionsReportAsHtml opts vd $ journalTransactionsReport (reportopts_ $ cliopts_ opts) j m
  hledgerLayout vd "journal" [hamlet|
       <div .row>
        <h2 #contenttitle>#{title}
        <!-- p>Journal entries record movements of commodities between accounts. -->
        <a #addformlink role="button" style="cursor:pointer; margin-top:1em;" data-toggle="modal" data-target="#addmodal" title="Add a new transaction to the journal" >Add a transaction
       <div .table-responsive>
        ^{maincontent}
     |]

postJournalR :: Handler Html
postJournalR = postAddForm

-- | Render a "TransactionsReport" as html for the formatted journal view.
journalTransactionsReportAsHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
journalTransactionsReportAsHtml _ vd (_,items) = [hamlet|
<table .transactionsreport .table .table-condensed>
 <thead>
  <th .date style="text-align:left;">
   Date
  <th .description style="text-align:left;">Description
  <th .account style="text-align:left;">Account
  <th .amount style="text-align:right;">Amount
 $forall i <- numberTransactionsReportItems items
  ^{itemAsHtml vd i}
 |]
 where
-- .#{datetransition}
   itemAsHtml :: ViewData -> (Int, Bool, Bool, Bool, TransactionsReportItem) -> HtmlUrl AppRoute
   itemAsHtml VD{..} (_, _, _, _, (torig, _, split, _, amt, _)) = [hamlet|
<tr .title #transaction-#{tindex torig}>
 <td .date>#{date}
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
       acctlink a = (RegisterR, [("q", T.pack $ accountQuery a)])
       -- datetransition | newm = "newmonth"
       --                | newd = "newday"
       --                | otherwise = "" :: String
       (date, desc) = (show $ tdate torig, tdescription torig)
       -- acctquery = (here, [("q", T.pack $ accountQuery acct)])
       showamt = not split || not (isZeroMixedAmount amt)

