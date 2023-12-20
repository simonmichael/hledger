-- | /register handlers.

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Hledger.Web.Handler.RegisterR where

import Data.List (intersperse, nub, partition, sortOn)
import qualified Data.Text as T
import Text.Hamlet (hamletFile)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.Import
import Hledger.Web.WebOptions
import Hledger.Web.Widget.AddForm (addModal)
import Hledger.Web.Widget.Common
             (accountQuery, mixedAmountAsHtml,
              transactionFragment, removeDates, removeInacct, replaceInacct)

-- | The main journal/account register view, with accounts sidebar.
getRegisterR :: Handler Html
getRegisterR = do
  checkServerSideUiEnabled
  VD{caps, j, q, opts, qparam, qopts, today} <- getViewData
  when (CapView `notElem` caps) (permissionDenied "Missing the 'view' capability")

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
          tail $ (", "<$xs) ++ [""]
      items = accountTransactionsReport rspec{_rsQuery=q} j acctQuery
      balancelabel
        | isJust (inAccount qopts), balanceaccum_ (_rsReportOpts rspec) == Historical = "Historical Total"
        | isJust (inAccount qopts) = "Period Total"
        | otherwise                = "Total"
      transactionFrag = transactionFragment j
      (chartQuery, depth) =
          case (,) <$> inAccount qopts <*> inAccountQuery qopts of
              Nothing -> (Any, 0)
              Just ((an, _), aq) -> (aq, accountNameLevel an)
      rspecWithQuery = rspec { rsQuery = (And [chartQuery, Depth $ depth + 1, m]) }
      balancereport = balanceReport rspecWithQuery j
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
        _ -> error "link name not decorated with account"  -- PARTIAL:

decorateLinks :: [(acct, ([char], [char]))] -> [(Maybe acct, char)]
decorateLinks = concatMap $ \(acct, (name, comma)) ->
    map (Just acct,) name ++ map (Nothing,) comma

-- | Generate javascript/html for a register balance line chart based on
-- the provided "AccountTransactionsReportItem"s.
registerChartHtml :: Text -> String -> [(CommoditySymbol, [AccountTransactionsReportItem])] -> HtmlUrl AppRoute
registerChartHtml q title percommoditytxnreports = $(hamletFile "templates/chart.hamlet")
 -- have to make sure plot is not called when our container (maincontent)
 -- is hidden, eg with add form toggled
 where
   charttitle = if null title then "" else title ++ ":"
   colorForCommodity = fromMaybe 0 . flip lookup commoditiesIndex
   commoditiesIndex = zip (map fst percommoditytxnreports) [0..] :: [(CommoditySymbol,Int)]
   simpleMixedAmountQuantity = maybe 0 aquantity . listToMaybe . amounts . mixedAmountStripPrices
   showZeroCommodity = wbUnpack . showMixedAmountB oneLine{displayPrice=False,displayZeroCommodity=True}
   shownull c = if null c then " " else c
   nodatelink = (RegisterR, [("q", T.unwords $ removeDates q)])

data PieHalf = Positive | Negative

moreThanOne :: Eq a => [a] -> Bool
moreThanOne [] = False
moreThanOne (x : xs) = rec xs
  where
    rec [] = False
    rec (y : ys) = x /= y || (rec ys)

-- | Generate javascript/html for a mockup pie chart
registerPieChartHtml :: PieHalf -> Text -> BalanceReport -> HtmlUrl AppRoute
registerPieChartHtml half q (items, _) = $(hamletFile "templates/piechart.hamlet")
  where
    labelData =
      reverse $
      sortOn (\(_, quant, _) -> quant) $
      filter (\(_, quant, _) -> case half of Positive -> quant >= 0
                                             Negative -> quant < 0) $
      flip concatMap items $ \(accname, _, _, Mixed as) ->
        flip map as $ \a -> (accname, aquantity a, acommodity a)
    moreThanOneCommodity = moreThanOne $ map (\(_, _, com) -> com) labelData
    showChart = if (moreThanOneCommodity) then "false" else "true" :: String
    noacctlink = (RegisterR, [("q", T.unwords $ removeInacct q)])
    chartId = case half of Positive -> "postive" :: String
                           Negative -> "negative" :: String
    legendPosition = case half of Positive -> "nw" :: String
                                  Negative -> "ne" :: String

dayToJsTimestamp :: Day -> Integer
dayToJsTimestamp d =
  read (formatTime defaultTimeLocale "%s" t) * 1000 -- XXX read
  where
    t = UTCTime d (secondsToDiffTime 0)

