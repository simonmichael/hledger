-- | /journal/entries handlers.

module Handler.JournalEntriesR where

import Import

import Handler.Common
import Handler.Post
import Handler.Utils

import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Cli.Options
import Hledger.Web.Options


-- | The journal entries view, with sidebar.
getJournalEntriesR :: Handler Html
getJournalEntriesR = do
  vd@VD{..} <- getViewData
  staticRootUrl <- (staticRoot . settings) <$> getYesod
  let
      sidecontent = sidebar vd
      title = "Journal entries" ++ if m /= Any then ", filtered" else "" :: String
      maincontent = entriesReportAsHtml opts vd $ entriesReport (reportopts_ $ cliopts_ opts) Any $ filterJournalTransactions m j
  defaultLayout $ do
      setTitle "hledger-web journal"
      toWidget [hamlet|
^{topbar vd}
<div#content>
 <div#sidebar>
  ^{sidecontent}
 <div#main.journal>
  <div#maincontent>
   ^{searchform vd}
   <h2#contenttitle>#{title}
   ^{maincontent}
  ^{addform staticRootUrl vd}
  ^{editform vd}
  ^{importform}
|]

postJournalEntriesR :: Handler Html
postJournalEntriesR = handlePost

-- | Render an "EntriesReport" as html for the journal entries view.
entriesReportAsHtml :: WebOpts -> ViewData -> EntriesReport -> HtmlUrl AppRoute
entriesReportAsHtml _ vd items = [hamlet|
<table.entriesreport>
 $forall i <- numbered items
  ^{itemAsHtml vd i}
 |]
 where
   itemAsHtml :: ViewData -> (Int, EntriesReportItem) -> HtmlUrl AppRoute
   itemAsHtml _ (n, t) = [hamlet|
<tr.item.#{evenodd}>
 <td.transaction>
  <pre>#{txn}
 |]
     where
       evenodd = if even n then "even" else "odd" :: String
       txn = trimnl $ showTransaction t where trimnl = reverse . dropWhile (=='\n') . reverse

