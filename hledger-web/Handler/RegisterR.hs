-- | /register handlers.

module Handler.RegisterR where

import Import

import Data.List
import Data.Maybe
import Safe

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
 ^{registerChartHtml $ transactionsReportByCommodity r}
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
  <th.account style="text-align:left;">To/From Account(s)
  <th.amount style="text-align:right; white-space:normal;">Amount Out/In
  <th.balance style="text-align:right; white-space:normal;">#{balancelabel'}
 $forall i <- numberTransactionsReportItems items
  ^{itemAsHtml vd i}
 |]
 where
   insomeacct = isJust $ inAccount $ qopts vd
   balancelabel' = if insomeacct then balancelabel else "Total"

   -- filtering = m /= Any
   itemAsHtml :: ViewData -> (Int, Bool, Bool, Bool, TransactionsReportItem) -> HtmlUrl AppRoute
   itemAsHtml VD{..} (n, newd, newm, _, (torig, tacct, split, acct, amt, bal)) = [hamlet|

<tr ##{date} .item.#{evenodd}.#{firstposting}.#{datetransition} title="#{show torig}" style="vertical-align:top;">
 <td.date><a href="/journal##{date}">#{date}
 <td.description title="#{show torig}">#{elideRight 30 desc}
 <td.account>#{elideRight 40 acct}
 <td.amount style="text-align:right; white-space:nowrap;">
  $if showamt
   \#{mixedAmountAsHtml amt}
 <td.balance style="text-align:right;">#{mixedAmountAsHtml bal}
|]

     where
       evenodd = if even n then "even" else "odd" :: String
       datetransition | newm = "newmonth"
                      | newd = "newday"
                      | otherwise = "" :: String
       (firstposting, date, desc) = (False, show $ tdate tacct, tdescription tacct)
       -- acctquery = (here, [("q", pack $ accountQuery acct)])
       showamt = not split || not (isZeroMixedAmount amt)

-- | Generate javascript/html for a register balance line chart based on
-- the provided "TransactionsReportItem"s.
               -- registerChartHtml :: forall t (t1 :: * -> *) t2 t3 t4 t5.
               --                      Data.Foldable.Foldable t1 =>
               --                      t1 (Transaction, t2, t3, t4, t5, MixedAmount)
               --                      -> t -> Text.Blaze.Internal.HtmlM ()
registerChartHtml :: [(Commodity, (String, [TransactionsReportItem]))] -> HtmlUrl AppRoute
registerChartHtml percommoditytxnreports =
 -- have to make sure plot is not called when our container (maincontent)
 -- is hidden, eg with add form toggled
 [hamlet|
<label#register-chart-label style=""><br>
<div#register-chart style="width:85%; height:150px; margin-bottom:1em; display:block;">
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
            #{simpleMixedAmountQuantity $ triCommodityBalance c i},
           ],
          /* [] */
        ],
        label: '#{shownull c}',
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
           -- [#{dayToJsTimestamp $ ltrace "\ndate" $ triDate i}, #{ltrace "balancequantity" $ simpleMixedAmountQuantity $ triCommodityBalance c i}, '#{ltrace "balance" $ show $ triCommodityBalance c i}, '#{ltrace "amount" $ show $ triCommodityAmount c i}''],
 where
   charttitle = case maybe "" (fst.snd) $ headMay percommoditytxnreports
           of "" -> ""
              s  -> s++":"
   colorForCommodity = fromMaybe 0 . flip lookup commoditiesIndex
   commoditiesIndex = zip (map fst percommoditytxnreports) [0..] :: [(Commodity,Int)]
   simpleMixedAmountQuantity = maybe 0 aquantity . headMay . amounts
   shownull c = if null c then " " else c
