-- | /register handlers.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.RegisterR where

import Import

import Data.List (intersperse)
import qualified Data.Text as T
import Text.Hamlet (hamletFile)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.WebOptions
import Widget.AddForm (addForm)
import Widget.Common (mixedAmountAsHtml, numberTransactionsReportItems)

-- | The main journal/account register view, with accounts sidebar.
getRegisterR :: Handler Html
getRegisterR = do
  VD{j, m, opts, qopts, today} <- getViewData
  let (a,inclsubs) = fromMaybe ("all accounts",True) $ inAccount qopts
      s1 = if inclsubs then "" else " (excluding subaccounts)"
      s2 = if m /= Any then ", filtered" else ""
      header = a <> s1 <> s2

  let r@(balancelabel,items) = accountTransactionsReport (reportopts_ $ cliopts_ opts) j m $ fromMaybe Any $ inAccountQuery qopts
      balancelabel' = if isJust (inAccount qopts) then balancelabel else "Total"
      evenodd x = if even x then "even" else "odd" :: Text
      datetransition newm newd
        | newm = "newmonth"
        | newd = "newday"
        | otherwise = "" :: Text

  (addView, addEnctype) <- generateFormPost (addForm j today)
  defaultLayout $ do
    setTitle "register - hledger-web"
    $(widgetFile "register")

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
