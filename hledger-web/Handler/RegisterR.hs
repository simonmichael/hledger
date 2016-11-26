-- | /register handlers.

module Handler.RegisterR where

import Import

import Data.Maybe

import Handler.Common
import Handler.Post
import Handler.Utils

import Hledger.Query
import Hledger.Reports
import Hledger.Cli.Options
import Hledger.Web.Options

-- | The main journal/account register view, with accounts sidebar.
getRegisterR :: Handler Html
getRegisterR = do
  vd@VD{..} <- getViewData
  staticRootUrl <- (staticRoot . settings) <$> getYesod
  let sidecontent = sidebar vd
      -- injournal = isNothing inacct
      filtering = m /= Any
      title = "Transactions in "++a++s1++s2
               where
                 (a,inclsubs) = fromMaybe ("all accounts",False) $ inAccount qopts
                 s1 = if inclsubs then " (and subaccounts)" else ""
                 s2 = if filtering then ", filtered" else ""
      maincontent = registerReportHtml opts vd $ accountTransactionsReport (reportopts_ $ cliopts_ opts) j m $ fromMaybe Any $ inAccountQuery qopts
  defaultLayout $ do
      setTitle "hledger-web register"
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

postRegisterR :: Handler Html
postRegisterR = handlePost
