-- | /register handlers.

module Handler.RegisterR where

import Import

import Data.Maybe

import Handler.Common
import Handler.Post
import Handler.Utils

import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Utils
import Hledger.Cli.Options
import Hledger.Web.Options

-- | The main journal/account register view, with accounts sidebar.
getRegisterR :: Handler Html
getRegisterR = do
  vd@VD{..} <- getViewData
  -- staticRootUrl <- (staticRoot . settings) <$> getYesod
  let -- injournal = isNothing inacct
      filtering = m /= Any
      -- title = "Transactions in "++a++s1++s2
      title = a++s1++s2
               where
                 (a,inclsubs) = fromMaybe ("all accounts",True) $ inAccount qopts
                 s1 = if inclsubs then "" else " (excluding subaccounts)"
                 s2 = if filtering then ", filtered" else ""
      maincontent = registerReportHtml opts vd $ accountTransactionsReport (reportopts_ $ cliopts_ opts) j m $ fromMaybe Any $ inAccountQuery qopts
  hledgerLayout vd "register" [hamlet|
       <h2#contenttitle>#{title}
       <!-- p>Transactions affecting this account, with running balance. -->
       ^{maincontent}
     |]

postRegisterR :: Handler Html
postRegisterR = handlePost

-- Generate html for an account register, including a balance chart and transaction list.
registerReportHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
registerReportHtml opts vd r = [hamlet|
 ^{registerChartHtml $ map snd $ transactionsReportByCommodity r}
 ^{registerItemsHtml opts vd r}
|]

-- Generate html for a transaction list from an "TransactionsReport".
registerItemsHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
registerItemsHtml _ vd (balancelabel,items) = [hamlet|
<table.registerreport>
 <tr.headings>
  <th.date style="text-align:left;">
   Date
   <span .glyphicon .glyphicon-chevron-up>
  <th.description style="text-align:left;">Description
  <th.account style="text-align:left;">To/From Account
  <th.amount style="text-align:right;">Amount Out/In
  <th.balance style="text-align:right;">#{balancelabel'}
 $forall i <- numberTransactionsReportItems items
  ^{itemAsHtml vd i}
 |]
 where
   insomeacct = isJust $ inAccount $ qopts vd
   balancelabel' = if insomeacct then balancelabel else "Total"

   -- filtering = m /= Any
   itemAsHtml :: ViewData -> (Int, Bool, Bool, Bool, TransactionsReportItem) -> HtmlUrl AppRoute
   itemAsHtml VD{..} (n, newd, newm, _, (t, _, split, acct, amt, bal)) = [hamlet|

<tr ##{date} .item.#{evenodd}.#{firstposting}.#{datetransition} title="#{show t}" style="vertical-align:top;">
 <td.date><a href="/journal##{date}">#{date}
 <td.description title="#{show t}">#{elideRight 30 desc}
 <td.account>#{elideRight 40 acct}
 <td.amount style="text-align:right; white-space:nowrap;">
  $if showamt
   \#{mixedAmountAsHtml amt}
 <td.balance style="text-align:right;">#{mixedAmountAsHtml bal}
|]
 -- $else
 --  $forall p' <- tpostings t
 --   <tr.item.#{evenodd}.posting>
 --   <td.date>
 --   <td.description>
 --   <td.account>&nbsp;<a href="@?{accountUrl here $ paccount p'}" title="Show transactions in #{paccount p'}">#{elideRight 40 $ paccount p'}
 --    <td.amount style="text-align:right;">#{mixedAmountAsHtml $ pamount p'}
 --    <td.balance style="text-align:right;">

     where
       evenodd = if even n then "even" else "odd" :: String
       datetransition | newm = "newmonth"
                      | newd = "newday"
                      | otherwise = "" :: String
       (firstposting, date, desc) = (False, show $ tdate t, tdescription t)
       -- acctquery = (here, [("q", pack $ accountQuery acct)])
       showamt = not split || not (isZeroMixedAmount amt)

-- | Generate javascript/html for a register balance line chart based on
-- the provided "TransactionsReportItem"s.
               -- registerChartHtml :: forall t (t1 :: * -> *) t2 t3 t4 t5.
               --                      Data.Foldable.Foldable t1 =>
               --                      t1 (Transaction, t2, t3, t4, t5, MixedAmount)
               --                      -> t -> Text.Blaze.Internal.HtmlM ()
registerChartHtml :: [[TransactionsReportItem]] -> HtmlUrl AppRoute
registerChartHtml itemss =
 -- have to make sure plot is not called when our container (maincontent)
 -- is hidden, eg with add form toggled
 [hamlet|
<div#register-chart style="width:600px;height:100px; margin-bottom:1em;">
<script type=text/javascript>
 \$(document).ready(function() {
   /* render chart with flot, if visible */
   var chartdiv = $('#register-chart');
   if (chartdiv.is(':visible'))
     \$.plot(chartdiv,
             [
              $forall items <- itemss
               [
                $forall i <- reverse items
                 [#{dayToJsTimestamp $ triDate i}, #{triSimpleBalance i}],
                []
               ],
              []
             ],
             {
               xaxis: {
                mode: "time",
                timeformat: "%y/%m/%d"
               }
             }
             );
  });
|]

