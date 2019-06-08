-- | /register handlers.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Web.Handler.RegisterR where

import Data.List (intersperse, nub, partition)
import qualified Data.Text as T
import Text.Hamlet (hamletFile)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.Import
import Hledger.Web.WebOptions
import Hledger.Web.Widget.AddForm (addModal)
import Hledger.Web.Widget.Common (accountQuery, mixedAmountAsHtml)

-- | The main journal/account register view, with accounts sidebar.
getRegisterR :: Handler Html
getRegisterR = do
  VD{caps, j, m, opts, qopts, today} <- getViewData
  when (CapView `notElem` caps) (permissionDenied "Missing the 'view' capability")

  let (a,inclsubs) = fromMaybe ("all accounts",True) $ inAccount qopts
      s1 = if inclsubs then "" else " (excluding subaccounts)"
      s2 = if m /= Any then ", filtered" else ""
      header = a <> s1 <> s2

  let ropts = reportopts_ (cliopts_ opts)
      acctQuery = fromMaybe Any (inAccountQuery qopts)
      acctlink acc = (RegisterR, [("q", accountQuery acc)])
      otherTransAccounts =
          addCommas . preferReal . otherTransactionAccounts m acctQuery
      addCommas xs = zip xs $ tail $ (","<$xs) ++ [T.empty]
      r@(balancelabel,items) = accountTransactionsReport ropts j m acctQuery
      balancelabel' = if isJust (inAccount qopts) then balancelabel else "Total"
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

-- | Generate javascript/html for a register balance line chart based on
-- the provided "TransactionsReportItem"s.
registerChartHtml :: [(CommoditySymbol, (String, [TransactionsReportItem]))] -> HtmlUrl AppRoute
registerChartHtml percommoditytxnreports = $(hamletFile "templates/chart.hamlet")
 -- have to make sure plot is not called when our container (maincontent)
 -- is hidden, eg with add form toggled
 where
   charttitle = case maybe "" (fst . snd) $ listToMaybe percommoditytxnreports of
     "" -> ""
     s  -> s <> ":"
   colorForCommodity = fromMaybe 0 . flip lookup commoditiesIndex
   commoditiesIndex = zip (map fst percommoditytxnreports) [0..] :: [(CommoditySymbol,Int)]
   simpleMixedAmountQuantity = maybe 0 aquantity . listToMaybe . amounts
   shownull c = if null c then " " else c

dayToJsTimestamp :: Day -> Integer
dayToJsTimestamp d =
  read (formatTime defaultTimeLocale "%s" t) * 1000 -- XXX read
  where
    t = UTCTime d (secondsToDiffTime 0)

