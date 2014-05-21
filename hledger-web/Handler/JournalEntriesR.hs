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
   <h2#contenttitle>#{title}
   ^{searchform vd}
   ^{maincontent}
  ^{addform staticRootUrl vd}
  ^{editform vd}
  ^{importform}
|]

postJournalEntriesR :: Handler Html
postJournalEntriesR = handlePost

