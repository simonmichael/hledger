-- | /journal handlers.

module Handler.JournalR where

import Import

import Handler.Common
import Handler.Post
import Handler.Utils

import Hledger.Query
import Hledger.Reports
import Hledger.Cli.Options
import Hledger.Web.Options

-- | The formatted journal view, with sidebar.
getJournalR :: Handler RepHtml
getJournalR = do
  vd@VD{..} <- getViewData
  staticRootUrl <- (staticRoot . settings) <$> getYesod
  let sidecontent = sidebar vd
      -- XXX like registerReportAsHtml
      inacct = inAccount qopts
      -- injournal = isNothing inacct
      filtering = m /= Any
      -- showlastcolumn = if injournal && not filtering then False else True
      title = case inacct of
                Nothing       -> "Journal"++s2
                Just (a,inclsubs) -> "Transactions in "++a++s1++s2
                                      where s1 = if inclsubs then " (and subaccounts)" else ""
                where
                  s2 = if filtering then ", filtered" else ""
      maincontent = journalTransactionsReportAsHtml opts vd $ journalTransactionsReport (reportopts_ $ cliopts_ opts) j m
  defaultLayout $ do
      setTitle "hledger-web journal"
      toWidget [hamlet|
^{topbar vd}
<div#content>
 <div#sidebar>
  ^{sidecontent}
 <div#main.register>
  <div#maincontent>
   <h2#contenttitle>#{title}
   ^{searchform vd}
   ^{maincontent}
  ^{addform staticRootUrl vd}
  ^{editform vd}
  ^{importform}
|]

postJournalR :: Handler RepHtml
postJournalR = handlePost

