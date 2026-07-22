-- | /register handlers.

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Hledger.Web.Handler.RegisterR where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.List (nub, partition)
import Data.Text qualified as T
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Safe (tailSafe)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.Import
import Hledger.Web.WebOptions
import Hledger.Web.Widget.AddForm (addModal)
import Hledger.Web.Widget.Common
             (accountQuery, mixedAmountAsHtml,
              transactionFragment, removeInacct, replaceInacct, removeDates)

-- | The main journal/account register view, with accounts sidebar.
getRegisterR :: Handler Html
getRegisterR = do
  checkServerSideUiEnabled
  VD{perms, j, q, opts, qparam, qopts, today} <- getViewData
  require ViewPermission

  let (a,inclsubs) = fromMaybe ("all accounts",True) $ inAccount qopts
      s1 = if inclsubs then "" else " (excluding subaccounts)"
      s2 = if q /= Any then ", filtered" else ""
      header = a <> s1 <> s2

  let rspec = reportspec_ (cliopts_ opts)
      acctQuery = fromMaybe Any (inAccountQuery qopts)
      acctlink acc = (RegisterR, [("q", replaceInacct qparam $ accountQuery acc)])
      otherTransAccounts =
          map (\(acct,(name,comma)) -> (acct, (T.pack name, T.pack comma))) .
          undecorateLinks . elideRightDecorated 40 . decorateLinks .
          addCommas . preferReal . otherTransactionAccounts q acctQuery
      addCommas xs =
          zip xs $
          zip (map (T.unpack . accountSummarisedName . paccount) xs) $
          tailSafe (", "<$xs) ++ [""]
      items =
        styleAmounts (journalCommodityStylesWith HardRounding j) $
        accountTransactionsReport rspec{_rsQuery=q} j acctQuery
      balancelabel :: String
      balancelabel
        | isJust (inAccount qopts), balanceaccum_ (_rsReportOpts rspec) == Historical = "Historical Total"
        | isJust (inAccount qopts) = "Period Total"
        | otherwise                = "Total"
      transactionFrag = transactionFragment j
  defaultLayout $ do
    setTitle "register - hledger-web"
    $(widgetFile "register")

-- cf. Hledger.Reports.AccountTransactionsReport.accountTransactionsReportItems
otherTransactionAccounts :: Query -> Query -> Transaction -> [Posting]
otherTransactionAccounts reportq thisacctq torig
    -- no current account ? summarise all matched postings
    | thisacctq == None  = reportps
    -- only postings to current account ? summarise those
    | null otheraccts    = thisacctps
    -- summarise matched postings to other account(s)
    | otherwise          = otheracctps
    where
      reportps = tpostings $ filterTransactionPostings reportq torig
      (thisacctps, otheracctps) = partition (matchesPosting thisacctq) reportps
      otheraccts = nub $ map paccount otheracctps

-- cf. Hledger.Reports.AccountTransactionsReport.summarisePostingAccounts
preferReal :: [Posting] -> [Posting]
preferReal ps
    | null realps = ps
    | otherwise   = realps
    where realps = filter isReal ps

elideRightDecorated :: Int -> [(Maybe d, Char)] -> [(Maybe d, Char)]
elideRightDecorated width s =
    if length s > width
        then take (width - 2) s ++ map (Nothing,) ".."
        else s

undecorateLinks :: [(Maybe acct, char)] -> [(acct, ([char], [char]))]
undecorateLinks [] = []
undecorateLinks xs0@(x:_) =
    case x of
        (Just acct, _) ->
            let (link, xs1) = span (isJust . fst) xs0
                (comma, xs2) = span (isNothing . fst) xs1
            in (acct, (map snd link, map snd comma)) : undecorateLinks xs2
        _ -> error' "link name not decorated with account"  -- PARTIAL:

decorateLinks :: [(acct, ([char], [char]))] -> [(Maybe acct, char)]
decorateLinks = concatMap $ \(acct, (name, comma)) ->
    map (Just acct,) name ++ map (Nothing,) comma

-- | Generate javascript/html for a register balance line chart based on
-- the provided "AccountTransactionsReportItem"s.
registerChartHtml :: Text -> String -> [(CommoditySymbol, [AccountTransactionsReportItem])] -> Widget
registerChartHtml q title percommoditytxnreports = do
  let chartData = map (\(commodity, items) -> 
        object ["label" .= commodity
               ,"data" .= map (\item -> 
                  object ["x" .= dayToUtcNoonTimestamp (triDate item)
                        ,"y" .= (realToFrac (simpleMixedAmountQuantity (triAmount item)) :: Double)]) items
               ,"borderColor" .= ("hsl(" ++ show (colorForCommodity commodity * 60) ++ ", 70%, 50%)")
               ,"backgroundColor" .= ("hsl(" ++ show (colorForCommodity commodity * 60) ++ ", 70%, 50%, 0.1)")
               ,"tension" .= (0.1 :: Double)
               ,"fill" .= False
               ,"pointRadius" .= (5 :: Int)
               ,"pointHoverRadius" .= (7 :: Int)
               ]) percommoditytxnreports
      chartDataJson = Aeson.encode chartData
      chartDataStr = T.unpack $ TL.toStrict $ TLE.decodeUtf8With lenientDecode chartDataJson
  $(whamletFile "templates/chart.hamlet")
  toWidgetHead [julius|
    // Wait for Chart.js to be loaded
    document.addEventListener('DOMContentLoaded', function() {
      if (typeof Chart !== 'undefined') {
        (function() {
          var ctx = document.getElementById('balanceChart');
          if (!ctx) return;
          
          var chartDataStr = ctx.getAttribute('data-chart');
          var datasets = JSON.parse(chartDataStr);
          
          console.log('Chart datasets:', datasets);
          
          // For single data points, use scatter instead of line
          var hasMultiplePoints = datasets.some(function(ds) { return ds.data.length > 1; });
          var chartType = hasMultiplePoints ? 'line' : 'scatter';
          
          var chart = new Chart(ctx, {
            type: chartType,
            data: {
              datasets: datasets
            },
            options: {
              responsive: true,
              maintainAspectRatio: false,
              scales: {
                x: {
                  type: 'time',
                  time: {
                    unit: 'day'
                  },
                  title: {
                    display: true,
                    text: 'Date'
                  }
                },
                y: {
                  beginAtZero: true,
                  title: {
                    display: true,
                    text: #{charttitle}
                  }
                }
              },
              plugins: {
                legend: {
                  display: true,
                  position: 'top'
                },
                tooltip: {
                  mode: 'index',
                  intersect: false
                }
              }
            }
          });
        })();
      }
    });
  |]
 -- have to make sure plot is not called when our container (maincontent)
 -- is hidden, eg with add form toggled
 where
   charttitle = if null title then "" else title ++ ":"
   colorForCommodity = fromMaybe 0 . flip lookup commoditiesIndex
   commoditiesIndex = zip (map fst percommoditytxnreports) [0..] :: [(CommoditySymbol,Int)]
   simpleMixedAmountQuantity = maybe 0 aquantity . listToMaybe . amounts . mixedAmountStripCosts

-- | Makes a unix timestamp (milliseconds since epoch) corresponding to noon on the given date in UTC.
dayToUtcNoonTimestamp :: Day -> Integer
dayToUtcNoonTimestamp d =
  read (formatTime defaultTimeLocale "%s" t) * 1000 -- XXX read
  where
    t = UTCTime d (secondsToDiffTime $ 12 * 60 * 60)
