{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}
-- | /register handlers.

module Handler.RegisterR where

import Import

import Data.Time
import Data.List (intersperse)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import Safe (headMay)

import Handler.AddForm (postAddForm)
import Handler.Common

import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Utils
import Hledger.Cli.CliOptions
import Hledger.Web.WebOptions

-- | The main journal/account register view, with accounts sidebar.
getRegisterR :: Handler Html
getRegisterR = do
  vd@VD{..} <- getViewData
  let title = a <> s1 <> s2
        where
          (a,inclsubs) = fromMaybe ("all accounts",True) $ inAccount qopts
          s1 = if inclsubs then "" else " (excluding subaccounts)"
          s2 = if m /= Any then ", filtered" else ""
  hledgerLayout vd "register" $ do
    _ <- [hamlet|<h2 #contenttitle>#{title}|]
    registerReportHtml opts vd $ accountTransactionsReport (reportopts_ $ cliopts_ opts) j m $ fromMaybe Any $ inAccountQuery qopts

postRegisterR :: Handler Html
postRegisterR = postAddForm

-- | Generate html for an account register, including a balance chart and transaction list.
registerReportHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
registerReportHtml opts vd r = [hamlet|
 <div .hidden-xs>
  ^{registerChartHtml $ transactionsReportByCommodity r}
 ^{registerItemsHtml opts vd r}
|]

-- | Generate html for a transaction list from an "TransactionsReport".
registerItemsHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
registerItemsHtml _ vd (balancelabel,items) = [hamlet|
<div .table-responsive>
 <table.registerreport .table .table-striped .table-condensed>
  <thead>
   <tr>
    <th style="text-align:left;">
     Date
     <span .glyphicon .glyphicon-chevron-up>
    <th style="text-align:left;">Description
    <th style="text-align:left;">To/From Account(s)
    <th style="text-align:right; white-space:normal;">Amount Out/In
    <th style="text-align:right; white-space:normal;">#{balancelabel'}
  $forall i <- numberTransactionsReportItems items
   ^{itemAsHtml vd i}
 |]
 where
   insomeacct = isJust $ inAccount $ qopts vd
   balancelabel' = if insomeacct then balancelabel else "Total"

   itemAsHtml :: ViewData -> (Int, Bool, Bool, Bool, TransactionsReportItem) -> HtmlUrl AppRoute
   itemAsHtml VD{..} (n, newd, newm, _, (torig, tacct, split, acct, amt, bal)) = [hamlet|
<tr ##{tindex torig} .item.#{evenodd}.#{firstposting}.#{datetransition} title="#{show torig}" style="vertical-align:top;">
 <td .date>
  <a href="@{JournalR}#transaction-#{tindex torig}">#{date}
 <td .description title="#{show torig}">#{textElideRight 30 desc}
 <td .account>#{elideRight 40 acct}
 <td .amount style="text-align:right; white-space:nowrap;">
  $if showamt
   \#{mixedAmountAsHtml amt}
 <td .balance style="text-align:right;">#{mixedAmountAsHtml bal}
|]
     where
       evenodd = if even n then "even" else "odd" :: Text
       datetransition | newm = "newmonth"
                      | newd = "newday"
                      | otherwise = "" :: Text
       (firstposting, date, desc) = (False, show $ tdate tacct, tdescription tacct)
       showamt = not split || not (isZeroMixedAmount amt)

-- | Generate javascript/html for a register balance line chart based on
-- the provided "TransactionsReportItem"s.
registerChartHtml :: [(CommoditySymbol, (String, [TransactionsReportItem]))] -> HtmlUrl AppRoute
registerChartHtml percommoditytxnreports =
 -- have to make sure plot is not called when our container (maincontent)
 -- is hidden, eg with add form toggled
 [hamlet|
<label #register-chart-label style=""><br>
<div #register-chart style="height:150px; margin-bottom:1em; display:block;">
<script type=text/javascript>
 \$(document).ready(function() {
   var $chartdiv = $('#register-chart');
   if ($chartdiv.is(':visible')) {
     \$('#register-chart-label').text('#{charttitle}');
     var seriesData = [
      $forall (c,(_,items)) <- percommoditytxnreports
       /* we render each commodity using two series:
        * one with extra data points added to show a stepped balance line */
       {
        data: [
          $forall i <- reverse items
           [
            #{dayToJsTimestamp $ triDate i},
            #{simpleMixedAmountQuantity $ triCommodityBalance c i}
           ],
        ],
        label: '#{shownull $ T.unpack c}',
        color: #{colorForCommodity c},
        lines: {
          show: true,
          steps: true,
        },
        points: {
          show: false,
        },
        clickable: false,
        hoverable: false,
       },
       /* and one with the original data, showing one clickable, hoverable point per transaction */
       {
        data: [
          $forall i <- reverse items
           [
            #{dayToJsTimestamp $ triDate i},
            #{simpleMixedAmountQuantity $ triCommodityBalance c i},
            '#{showMixedAmountWithZeroCommodity $ triCommodityAmount c i}',
            '#{showMixedAmountWithZeroCommodity $ triCommodityBalance c i}',
            '#{concat $ intersperse "\\n" $ lines  $ show $ triOrigTransaction i}',
            #{tindex $ triOrigTransaction i}
           ],
          /* [] */
        ],
        label: '',
        color: #{colorForCommodity c},
        lines: {
          show: false,
        },
        points: {
          show: true,
        },
       },
     ]
     var plot = registerChart($chartdiv, seriesData);
     \$chartdiv.bind("plotclick", registerChartClick);
   };
 });
|]
 where
   charttitle = case maybe "" (fst.snd) $ headMay percommoditytxnreports of
     "" -> ""
     s  -> s <> ":"
   colorForCommodity = fromMaybe 0 . flip lookup commoditiesIndex
   commoditiesIndex = zip (map fst percommoditytxnreports) [0..] :: [(CommoditySymbol,Int)]
   simpleMixedAmountQuantity = maybe 0 aquantity . headMay . amounts
   shownull c = if null c then " " else c

dayToJsTimestamp :: Day -> Integer
dayToJsTimestamp d =
  read (formatTime defaultTimeLocale "%s" t) * 1000 -- XXX read
  where
    t = UTCTime d (secondsToDiffTime 0)
