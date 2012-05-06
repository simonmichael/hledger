{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, RecordWildCards #-}
{-

hledger-web's request handlers, and helpers.

-}

module Hledger.Web.Handlers
(
  -- * GET handlers
  getRootR,
  getJournalR,
  getJournalEntriesR,
  getJournalEditR,
  getRegisterR,
  -- ** helpers
  -- sidebar,
  -- accountsReportAsHtml,
  -- accountQuery,
  -- accountOnlyQuery,
  -- accountUrl,
  -- entriesReportAsHtml,
  -- journalTransactionsReportAsHtml,
  -- registerReportHtml,
  -- registerItemsHtml,
  -- registerChartHtml,
  -- stringIfLongerThan,
  -- numberTransactionsReportItems,
  -- mixedAmountAsHtml,
  -- * POST handlers
  postJournalR,
  postJournalEntriesR,
  postJournalEditR,
  postRegisterR,
  -- * Common page components
  -- * Utilities
  ViewData(..),
  nullviewdata,
)
where

import Prelude
import Control.Applicative ((<$>))
import Data.Either (lefts,rights)
import Data.List
import Data.Maybe
import Data.Text(Text,pack,unpack)
import qualified Data.Text (null)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.FilePath (takeFileName)
import System.IO.Storage (putValue, getValue)
import System.Locale (defaultTimeLocale)
import Text.Blaze (preEscapedString, toHtml)
import Text.Hamlet hiding (hamlet)
import Text.Printf
import Yesod.Core
-- import Yesod.Json

import Hledger hiding (today)
import Hledger.Cli hiding (version)
import Hledger.Web.Foundation
import Hledger.Web.Options
import Hledger.Web.Settings

-- routes:
-- /static          StaticR         Static getStatic
-- -- /favicon.ico     FaviconR        GET
-- /robots.txt      RobotsR         GET
-- /                RootR           GET
-- /journal         JournalR        GET POST
-- /journal/entries JournalEntriesR GET POST
-- /journal/edit    JournalEditR    GET POST
-- /register        RegisterR       GET POST
-- -- /accounts        AccountsR       GET
-- -- /api/accounts    AccountsJsonR   GET

----------------------------------------------------------------------
-- GET handlers

getRootR :: Handler RepHtml
getRootR = redirect defaultroute where defaultroute = RegisterR

-- | The formatted journal view, with sidebar.
getJournalR :: Handler RepHtml
getJournalR = do
  vd@VD{..} <- getViewData
  let sidecontent = sidebar vd
      -- XXX like registerReportAsHtml
      inacct = inAccount qopts
      -- injournal = isNothing inacct
      filtering = m /= Any
      -- showlastcolumn = if injournal && not filtering then False else True
      title = case inacct of
                Nothing       -> "Journal"++s2
                Just (a,subs) -> "Transactions in "++a++s1++s2
                                  where s1 = if subs then " (and subaccounts)" else ""
                where
                  s2 = if filtering then ", filtered" else ""
      maincontent = journalTransactionsReportAsHtml opts vd $ journalTransactionsReport (reportopts_ $ cliopts_ opts) j m
  defaultLayout $ do
      setTitle "hledger-web journal"
      addHamlet [hamlet|
^{topbar vd}
<div#content
 <div#sidebar
  ^{sidecontent}
 <div#main.register
  <div#maincontent
   <h2#contenttitle>#{title}
   ^{searchform vd}
   ^{maincontent}
  ^{addform vd}
  ^{editform vd}
  ^{importform}
|]

-- | The journal entries view, with sidebar.
getJournalEntriesR :: Handler RepHtml
getJournalEntriesR = do
  vd@VD{..} <- getViewData
  let
      sidecontent = sidebar vd
      title = "Journal entries" ++ if m /= Any then ", filtered" else "" :: String
      maincontent = entriesReportAsHtml opts vd $ entriesReport (reportopts_ $ cliopts_ opts) nullfilterspec $ filterJournalTransactions2 m j
  defaultLayout $ do
      setTitle "hledger-web journal"
      addHamlet [hamlet|
^{topbar vd}
<div#content
 <div#sidebar
  ^{sidecontent}
 <div#main.journal
  <div#maincontent
   <h2#contenttitle>#{title}
   ^{searchform vd}
   ^{maincontent}
  ^{addform vd}
  ^{editform vd}
  ^{importform}
|]

-- | The journal editform, no sidebar.
getJournalEditR :: Handler RepHtml
getJournalEditR = do
  vd <- getViewData
  defaultLayout $ do
      setTitle "hledger-web journal edit form"
      addHamlet $ editform vd

-- -- | The journal entries view, no sidebar.
-- getJournalOnlyR :: Handler RepHtml
-- getJournalOnlyR = do
--   vd@VD{..} <- getViewData
--   defaultLayout $ do
--       setTitle "hledger-web journal only"
--       addHamlet $ entriesReportAsHtml opts vd $ entriesReport (reportopts_ $ cliopts_ opts) nullfilterspec $ filterJournalTransactions2 m j

-- | The main journal/account register view, with accounts sidebar.
getRegisterR :: Handler RepHtml
getRegisterR = do
  vd@VD{..} <- getViewData
  let sidecontent = sidebar vd
      -- injournal = isNothing inacct
      filtering = m /= Any
      title = "Transactions in "++a++s1++s2
               where
                 (a,subs) = fromMaybe ("all accounts",False) $ inAccount qopts
                 s1 = if subs then " (and subaccounts)" else ""
                 s2 = if filtering then ", filtered" else ""
      maincontent = registerReportHtml opts vd $ accountTransactionsReport (reportopts_ $ cliopts_ opts) j m $ fromMaybe Any $ inAccountQuery qopts
  defaultLayout $ do
      setTitle "hledger-web register"
      addHamlet [hamlet|
^{topbar vd}
<div#content
 <div#sidebar
  ^{sidecontent}
 <div#main.register
  <div#maincontent
   <h2#contenttitle>#{title}
   ^{searchform vd}
   ^{maincontent}
  ^{addform vd}
  ^{editform vd}
  ^{importform}
|]

-- -- | The register view, no sidebar.
-- getRegisterOnlyR :: Handler RepHtml
-- getRegisterOnlyR = do
--   vd@VD{..} <- getViewData
--   defaultLayout $ do
--       setTitle "hledger-web register only"
--       addHamlet $
--           case inAccountQuery qopts of Just m' -> registerReportHtml opts vd $ accountTransactionsReport (reportopts_ $ cliopts_ opts) j m m'
--                                          Nothing -> registerReportHtml opts vd $ journalTransactionsReport (reportopts_ $ cliopts_ opts) j m

{-
-- | A simple accounts view. This one is json-capable, returning the chart
-- of accounts as json if the Accept header specifies json.
getAccountsR :: Handler RepHtmlJson
getAccountsR = do
  vd@VD{..} <- getViewData
  let j' = filterJournalPostings2 m j
      html = do
        setTitle "hledger-web accounts"
        addHamlet $ accountsReportAsHtml opts vd $ accountsReport2 (reportopts_ $ cliopts_ opts) am j'
      json = jsonMap [("accounts", toJSON $ journalAccountNames j')]
  defaultLayoutJson html json

-- | A json-only version of "getAccountsR", does not require the special Accept header.
getAccountsJsonR :: Handler RepJson
getAccountsJsonR = do
  VD{..} <- getViewData
  let j' = filterJournalPostings2 m j
  jsonToRepJson $ jsonMap [("accounts", toJSON $ journalAccountNames j')]
-}

-- helpers

-- | Render the sidebar used on most views.
sidebar :: ViewData -> HtmlUrl AppRoute
sidebar vd@VD{..} = accountsReportAsHtml opts vd $ accountsReport2 (reportopts_ $ cliopts_ opts) am j

-- | Render an "AccountsReport" as html.
accountsReportAsHtml :: WebOpts -> ViewData -> AccountsReport -> HtmlUrl AppRoute
accountsReportAsHtml _ vd@VD{..} (items',total) =
 [hamlet|
<div#accountsheading
 <a#accounts-toggle-link.togglelink href="#" title="Toggle sidebar">[+]
<div#accounts
 <table.balancereport>
  <tr
   <td.add colspan=3
    <br>
    <a#addformlink href="#" onclick="return addformToggle(event)" title="Add a new transaction to the journal">Add a transaction..

  <tr.item :allaccts:.inacct
   <td.journal colspan=3
    <br>
    <a href=@{JournalR} title="Show all transactions in journal format">Journal
    <span.hoverlinks
     &nbsp;
     <a href=@{JournalEntriesR} title="Show journal entries">entries
     &nbsp;
     <a#editformlink href="#" onclick="return editformToggle(event)" title="Edit the journal">
      edit

  <tr
   <td colspan=3
    <br>
    Accounts

  $forall i <- items
   ^{itemAsHtml vd i}

  <tr.totalrule>
   <td colspan=3>
  <tr>
   <td>
   <td.balance align=right>#{mixedAmountAsHtml total}
   <td>
|]
 where
   l = journalToLedger nullfilterspec j
   inacctmatcher = inAccountQuery qopts
   allaccts = isNothing inacctmatcher
   items = items' -- maybe items' (\m -> filter (matchesAccount m . \(a,_,_,_)->a) items') showacctmatcher
   itemAsHtml :: ViewData -> AccountsReportItem -> HtmlUrl AppRoute
   itemAsHtml _ (acct, adisplay, aindent, abal) = [hamlet|
<tr.item.#{inacctclass}
 <td.account.#{depthclass}
  #{indent}
  <a href="@?{acctquery}" title="Show transactions in this account, including subaccounts">#{adisplay}
  <span.hoverlinks
   $if hassubs
    &nbsp;
    <a href="@?{acctonlyquery}" title="Show transactions in this account only">only
   <!--
    &nbsp;
    <a href="@?{acctsonlyquery}" title="Focus on this account and sub-accounts and hide others">-others -->

 <td.balance align=right>#{mixedAmountAsHtml abal}
 <td.numpostings align=right title="#{numpostings} transactions in this account">(#{numpostings})
|]
     where
       hassubs = not $ null $ ledgerSubAccounts l $ ledgerAccount l acct
       numpostings = length $ apostings $ ledgerAccount l acct
       depthclass = "depth"++show aindent
       inacctclass = case inacctmatcher of
                       Just m' -> if m' `matchesAccount` acct then "inacct" else "notinacct"
                       Nothing -> "" :: String
       indent = preEscapedString $ concat $ replicate (2 * (1+aindent)) "&nbsp;"
       acctquery = (RegisterR, [("q", pack $ accountQuery acct)])
       acctonlyquery = (RegisterR, [("q", pack $ accountOnlyQuery acct)])

accountQuery :: AccountName -> String
accountQuery a = "inacct:" ++ quoteIfSpaced a -- (accountNameToAccountRegex a)

accountOnlyQuery :: AccountName -> String
accountOnlyQuery a = "inacctonly:" ++ quoteIfSpaced a -- (accountNameToAccountRegex a)

accountUrl :: AppRoute -> AccountName -> (AppRoute, [(Text, Text)])
accountUrl r a = (r, [("q", pack $ accountQuery a)])

-- | Render an "EntriesReport" as html for the journal entries view.
entriesReportAsHtml :: WebOpts -> ViewData -> EntriesReport -> HtmlUrl AppRoute
entriesReportAsHtml _ vd items = [hamlet|
<table.journalreport>
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

-- | Render a "TransactionsReport" as html for the formatted journal view.
journalTransactionsReportAsHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
journalTransactionsReportAsHtml _ vd (_,items) = [hamlet|
<table.journalreport
 <tr.headings
  <th.date align=left>Date
  <th.description align=left>Description
  <th.account align=left>Accounts
  <th.amount align=right>Amount
 $forall i <- numberTransactionsReportItems items
  ^{itemAsHtml vd i}
 |]
 where
-- .#{datetransition}
   itemAsHtml :: ViewData -> (Int, Bool, Bool, Bool, TransactionsReportItem) -> HtmlUrl AppRoute
   itemAsHtml VD{..} (n, _, _, _, (t, _, split, _, amt, _)) = [hamlet|
<tr.item.#{evenodd}.#{firstposting}
 <td.date>#{date}
 <td.description colspan=2 title="#{show t}">#{elideRight 60 desc}
 <td.amount align=right>
  $if showamt
   #{mixedAmountAsHtml amt}
$forall p' <- tpostings t
  <tr.item.#{evenodd}.posting
   <td.date
   <td.description
   <td.account>&nbsp;<a href="@?{accountUrl here $ paccount p'}" title="Show transactions in #{paccount p'}">#{elideRight 40 $ paccount p'}
   <td.amount align=right>#{mixedAmountAsHtml $ pamount p'}
|]
     where
       evenodd = if even n then "even" else "odd" :: String
       -- datetransition | newm = "newmonth"
       --                | newd = "newday"
       --                | otherwise = "" :: String
       (firstposting, date, desc) = (False, show $ tdate t, tdescription t)
       -- acctquery = (here, [("q", pack $ accountQuery acct)])
       showamt = not split || not (isZeroMixedAmount amt)

-- Generate html for an account register, including a balance chart and transaction list.
registerReportHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
registerReportHtml opts vd r@(_,items) = [hamlet|
 ^{registerChartHtml items}
 ^{registerItemsHtml opts vd r}
|]

-- Generate html for a transaction list from an "TransactionsReport".
registerItemsHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
registerItemsHtml _ vd (balancelabel,items) = [hamlet|
<table.registerreport
 <tr.headings
  <th.date align=left>Date
  <th.description align=left>Description
  <th.account align=left>To/From Account
    <!-- \ #
    <a#all-postings-toggle-link.togglelink href="#" title="Toggle all split postings">[+] -->
  <th.amount align=right>Amount
  <th.balance align=right>#{balancelabel}

 $forall i <- numberTransactionsReportItems items
  ^{itemAsHtml vd i}
 |]
 where
   -- inacct = inAccount qopts
   -- filtering = m /= Any
   itemAsHtml :: ViewData -> (Int, Bool, Bool, Bool, TransactionsReportItem) -> HtmlUrl AppRoute
   itemAsHtml VD{..} (n, newd, newm, _, (t, _, split, acct, amt, bal)) = [hamlet|
<tr.item.#{evenodd}.#{firstposting}.#{datetransition}
 <td.date>#{date}
 <td.description title="#{show t}">#{elideRight 30 desc}
 <td.account title="#{show t}"
  <a
   #{elideRight 40 acct}
  &nbsp;
  <a.postings-toggle-link.togglelink href="#" title="Toggle all postings"
   [+]
 <td.amount align=right>
  $if showamt
   #{mixedAmountAsHtml amt}
 <td.balance align=right>#{mixedAmountAsHtml bal}
$forall p' <- tpostings t
 <tr.item.#{evenodd}.posting style=#{postingsdisplaystyle}
   <td.date
   <td.description
   <td.account>&nbsp;<a href="@?{accountUrl here $ paccount p'}" title="Show transactions in #{paccount p'}">#{elideRight 40 $ paccount p'}
   <td.amount align=right>#{mixedAmountAsHtml $ pamount p'}
   <td.balance align=right>
|]
     where
       evenodd = if even n then "even" else "odd" :: String
       datetransition | newm = "newmonth"
                      | newd = "newday"
                      | otherwise = "" :: String
       (firstposting, date, desc) = (False, show $ tdate t, tdescription t)
       -- acctquery = (here, [("q", pack $ accountQuery acct)])
       showamt = not split || not (isZeroMixedAmount amt)
       postingsdisplaystyle = if showpostings then "" else "display:none;" :: String

-- | Generate javascript/html for a register balance line chart based on
-- the provided "TransactionsReportItem"s.
               -- registerChartHtml :: forall t (t1 :: * -> *) t2 t3 t4 t5.
               --                      Data.Foldable.Foldable t1 =>
               --                      t1 (Transaction, t2, t3, t4, t5, MixedAmount)
               --                      -> t -> Text.Blaze.Internal.HtmlM ()
registerChartHtml :: [TransactionsReportItem] -> HtmlUrl AppRoute
registerChartHtml items =
 -- have to make sure plot is not called when our container (maincontent)
 -- is hidden, eg with add form toggled
 [hamlet|
<script type=text/javascript>
 if (document.getElementById('maincontent').style.display != 'none')
  \$(document).ready(function() {
    /* render chart */
      \$.plot($('#register-chart'),
             [
              [
               $forall i <- items
                [#{dayToJsTimestamp $ triDate i}, #{triBalance i}],
              ]
             ],
             {
               xaxis: {
                mode: "time",
                timeformat: "%y/%m/%d"
               }
             }
             );
  });
<div#register-chart style="width:600px;height:100px; margin-bottom:1em;"
|]

-- stringIfLongerThan :: Int -> String -> String
-- stringIfLongerThan n s = if length s > n then s else ""

numberTransactionsReportItems :: [TransactionsReportItem] -> [(Int,Bool,Bool,Bool,TransactionsReportItem)]
numberTransactionsReportItems [] = []
numberTransactionsReportItems items = number 0 nulldate items
  where
    number :: Int -> Day -> [TransactionsReportItem] -> [(Int,Bool,Bool,Bool,TransactionsReportItem)]
    number _ _ [] = []
    number n prevd (i@(Transaction{tdate=d},_,_,_,_,_):is)  = (n+1,newday,newmonth,newyear,i):(number (n+1) d is)
        where
          newday = d/=prevd
          newmonth = dm/=prevdm || dy/=prevdy
          newyear = dy/=prevdy
          (dy,dm,_) = toGregorian d
          (prevdy,prevdm,_) = toGregorian prevd

mixedAmountAsHtml :: MixedAmount -> Html
mixedAmountAsHtml b = preEscapedString $ addclass $ intercalate "<br>" $ lines $ show b
    where addclass = printf "<span class=\"%s\">%s</span>" (c :: String)
          c = case isNegativeMixedAmount b of Just True -> "negative amount"
                                              _         -> "positive amount"

-------------------------------------------------------------------------------
-- POST handlers

postJournalR :: Handler RepHtml
postJournalR = handlePost

postJournalEntriesR :: Handler RepHtml
postJournalEntriesR = handlePost

postJournalEditR :: Handler RepHtml
postJournalEditR = handlePost

postRegisterR :: Handler RepHtml
postRegisterR = handlePost

-- | Handle a post from any of the edit forms.
handlePost :: Handler RepHtml
handlePost = do
  action <- lookupPostParam  "action"
  case action of Just "add"    -> handleAdd
                 Just "edit"   -> handleEdit
                 Just "import" -> handleImport
                 _             -> invalidArgs [pack "invalid action"]

-- | Handle a post from the transaction add form.
handleAdd :: Handler RepHtml
handleAdd = do
  VD{..} <- getViewData
  -- get form input values. M means a Maybe value.
  dateM <- lookupPostParam  "date"
  descM <- lookupPostParam  "description"
  acct1M <- lookupPostParam  "account1"
  amt1M <- lookupPostParam  "amount1"
  acct2M <- lookupPostParam  "account2"
  amt2M <- lookupPostParam  "amount2"
  journalM <- lookupPostParam  "journal"
  -- supply defaults and parse date and amounts, or get errors.
  let dateE = maybe (Left "date required") (either (\e -> Left $ showDateParseError e) Right . fixSmartDateStrEither today . unpack) dateM
      descE = Right $ maybe "" unpack descM
      maybeNonNull = maybe Nothing (\t -> if Data.Text.null t then Nothing else Just t)
      acct1E = maybe (Left "to account required") (Right . unpack) $ maybeNonNull acct1M
      acct2E = maybe (Left "from account required") (Right . unpack) $ maybeNonNull acct2M
      amt1E = maybe (Left "amount required") (either (const $ Left "could not parse amount") Right . parseWithCtx nullctx amount . unpack) amt1M
      amt2E = maybe (Right missingamt)       (either (const $ Left "could not parse amount") Right . parseWithCtx nullctx amount . unpack) amt2M
      journalE = maybe (Right $ journalFilePath j)
                       (\f -> let f' = unpack f in
                              if f' `elem` journalFilePaths j
                              then Right f'
                              else Left $ "unrecognised journal file path: " ++ f'
                              )
                       journalM
      strEs = [dateE, descE, acct1E, acct2E, journalE]
      amtEs = [amt1E, amt2E]
      errs = lefts strEs ++ lefts amtEs
      [date,desc,acct1,acct2,journalpath] = rights strEs
      [amt1,amt2] = rights amtEs
      -- if no errors so far, generate a transaction and balance it or get the error.
      tE | not $ null errs = Left errs
         | otherwise = either (\e -> Left ["unbalanced postings: " ++ (head $ lines e)]) Right
                        (balanceTransaction Nothing $ nulltransaction { -- imprecise balancing
                           tdate=parsedate date
                          ,tdescription=desc
                          ,tpostings=[
                            Posting False acct1 amt1 "" RegularPosting [] Nothing
                           ,Posting False acct2 amt2 "" RegularPosting [] Nothing
                           ]
                          })
  -- display errors or add transaction
  case tE of
   Left errs' -> do
    -- save current form values in session
    -- setMessage $ toHtml $ intercalate "; " errs
    setMessage [shamlet|
                 Errors:<br>
                 $forall e<-errs'
                  #{e}<br>
               |]
   Right t -> do
    let t' = txnTieKnot t -- XXX move into balanceTransaction
    liftIO $ do ensureJournalFileExists journalpath
                appendToJournalFileOrStdout journalpath $ showTransaction t'
    -- setMessage $ toHtml $ (printf "Added transaction:\n%s" (show t') :: String)
    setMessage [shamlet|<span>Added transaction:<small><pre>#{chomp $ show t'}</pre></small>|]

  redirect (RegisterR, [("add","1")])

chomp :: String -> String
chomp = reverse . dropWhile (`elem` "\r\n") . reverse

-- | Handle a post from the journal edit form.
handleEdit :: Handler RepHtml
handleEdit = do
  VD{..} <- getViewData
  -- get form input values, or validation errors.
  -- getRequest >>= liftIO (reqRequestBody req) >>= mtrace
  textM <- lookupPostParam "text"
  journalM <- lookupPostParam "journal"
  let textE = maybe (Left "No value provided") (Right . unpack) textM
      journalE = maybe (Right $ journalFilePath j)
                       (\f -> let f' = unpack f in
                              if f' `elem` journalFilePaths j
                              then Right f'
                              else Left "unrecognised journal file path")
                       journalM
      strEs = [textE, journalE]
      errs = lefts strEs
      [text,journalpath] = rights strEs
  -- display errors or perform edit
  if not $ null errs
   then do
    setMessage $ toHtml (intercalate "; " errs :: String)
    redirect JournalR

   else do
    -- try to avoid unnecessary backups or saving invalid data
    filechanged' <- liftIO $ journalSpecifiedFileIsNewer j journalpath
    told <- liftIO $ readFileStrictly journalpath
    let tnew = filter (/= '\r') text
        changed = tnew /= told || filechanged'
    if not changed
     then do
       setMessage "No change"
       redirect JournalR
     else do
      jE <- liftIO $ readJournal Nothing Nothing (Just journalpath) tnew
      either
       (\e -> do
          setMessage $ toHtml e
          redirect JournalR)
       (const $ do
          liftIO $ writeFileWithBackup journalpath tnew
          setMessage $ toHtml (printf "Saved journal %s\n" (show journalpath) :: String)
          redirect JournalR)
       jE

-- | Handle a post from the journal import form.
handleImport :: Handler RepHtml
handleImport = do
  setMessage "can't handle file upload yet"
  redirect JournalR
  -- -- get form input values, or basic validation errors. E means an Either value.
  -- fileM <- runFormPost $ maybeFileInput "file"
  -- let fileE = maybe (Left "No file provided") Right fileM
  -- -- display errors or import transactions
  -- case fileE of
  --  Left errs -> do
  --   setMessage errs
  --   redirect JournalR

  --  Right s -> do
  --    setMessage s
  --    redirect JournalR

----------------------------------------------------------------------
-- Common page components.

-- | Global toolbar/heading area.
topbar :: ViewData -> HtmlUrl AppRoute
topbar VD{..} = [hamlet|
<div#topbar
 <a.topleftlink href=#{hledgerorgurl} title="More about hledger"
  hledger-web
  <br />
  #{version}
 <a.toprightlink href=#{manualurl} target=hledgerhelp title="User manual">manual
 <h1>#{title}
$maybe m' <- msg
 <div#message>#{m'}
|]
  where
    title = takeFileName $ journalFilePath j

-- -- | Navigation link, preserving parameters and possibly highlighted.
-- navlink :: ViewData -> String -> AppRoute -> String -> HtmlUrl AppRoute
-- navlink VD{..} s dest title = [hamlet|
-- <a##{s}link.#{style} href=@?{u'} title="#{title}">#{s}
-- |]
--   where u' = (dest, if null q then [] else [("q", pack q)])
--         style | dest == here = "navlinkcurrent"
--               | otherwise    = "navlink" :: Text

-- -- | Links to the various journal editing forms.
-- editlinks :: HtmlUrl AppRoute
-- editlinks = [hamlet|
-- <a#editformlink href="#" onclick="return editformToggle(event)" title="Toggle journal edit form">edit
-- \ | #
-- <a#addformlink href="#" onclick="return addformToggle(event)" title="Toggle transaction add form">add
-- <a#importformlink href="#" onclick="return importformToggle(event)" style="display:none;">import transactions
-- |]

-- | Link to a topic in the manual.
helplink :: String -> String -> HtmlUrl AppRoute
helplink topic label = [hamlet|
<a href=#{u} target=hledgerhelp>#{label}
|]
    where u = manualurl ++ if null topic then "" else '#':topic

-- | Search form for entering custom queries to filter journal data.
searchform :: ViewData -> HtmlUrl AppRoute
searchform VD{..} = [hamlet|
<div#searchformdiv
 <form#searchform.form method=GET
  <table
   <tr
    <td
     Search:
     \ #
    <td
     <input name=q size=70 value=#{q}
     <input type=submit value="Search"
     $if filtering
      \ #
      <span.showall
       <a href=@{here}>clear search
     \ #
     <a#search-help-link href="#" title="Toggle search help">help
   <tr
    <td
    <td
     <div#search-help.help style="display:none;"
      Leave blank to see journal (all transactions), or click account links to see transactions under that account.
      <br>
      Transactions/postings may additionally be filtered by:
      <br>
      acct:REGEXP (target account), #
      desc:REGEXP (description), #
      date:PERIODEXP (date), #
      edate:PERIODEXP (effective date), #
      <br>
      status:BOOL (cleared status), #
      real:BOOL (real/virtual-ness), #
      empty:BOOL (posting amount = 0).
      <br>
      not: to negate, enclose space-containing patterns in quotes, multiple filters are AND'ed.
|]
 where
  filtering = not $ null q

-- | Add transaction form.
addform :: ViewData -> HtmlUrl AppRoute
addform vd@VD{..} = [hamlet|
<script type=text/javascript>
 \$(document).ready(function() {
    /* dhtmlxcombo setup */
    window.dhx_globalImgPath="../static/";
    var desccombo  = new dhtmlXCombo("description");
    var acct1combo = new dhtmlXCombo("account1");
    var acct2combo = new dhtmlXCombo("account2");
    desccombo.enableFilteringMode(true);
    acct1combo.enableFilteringMode(true);
    acct2combo.enableFilteringMode(true);
    desccombo.setSize(300);
    acct1combo.setSize(300);
    acct2combo.setSize(300);
    /* desccombo.enableOptionAutoHeight(true, 20); */
    /* desccombo.setOptionHeight(200); */
 });

<form#addform method=POST style=display:none;
  <h2#contenttitle>#{title}
  <table.form
   <tr
    <td colspan=4
     <table
      <tr#descriptionrow
       <td
        Date:
       <td
        <input.textinput size=15 name=date value=#{date}
       <td style=padding-left:1em;
        Description:
       <td
        <select id=description name=description
         <option
         $forall d <- descriptions
          <option value=#{d}>#{d}
      <tr.helprow
       <td
       <td
        <span.help>#{datehelp} #
       <td
       <td
        <span.help>#{deschelp}
   ^{postingfields vd 1}
   ^{postingfields vd 2}
   <tr#addbuttonrow
    <td colspan=4
     <input type=hidden name=action value=add
     <input type=submit name=submit value="add transaction"
     $if manyfiles
      \ to: ^{journalselect $ files j}
     \ or #
     <a href="#" onclick="return addformToggle(event)">cancel
|]
 where
  title = "Add transaction" :: String
  datehelp = "eg: 2010/7/20" :: String
  deschelp = "eg: supermarket (optional)" :: String
  date = "today" :: String
  descriptions = sort $ nub $ map tdescription $ jtxns j
  manyfiles = (length $ files j) > 1
  postingfields :: ViewData -> Int -> HtmlUrl AppRoute
  postingfields _ n = [hamlet|
<tr#postingrow
 <td align=right>#{acctlabel}:
 <td
  <select id=#{acctvar} name=#{acctvar}
   <option
   $forall a <- acctnames
    <option value=#{a} :shouldselect a:selected>#{a}
 ^{amtfield}
<tr.helprow
 <td
 <td
  <span.help>#{accthelp}
 <td
 <td
  <span.help>#{amthelp}
|]
   where
    shouldselect a = n == 2 && maybe False ((a==).fst) (inAccount qopts)
    withnumber = (++ show n)
    acctvar = withnumber "account"
    amtvar = withnumber "amount"
    acctnames = sort $ journalAccountNamesUsed j
    (acctlabel, accthelp, amtfield, amthelp)
       | n == 1     = ("To account"
                     ,"eg: expenses:food"
                     ,[hamlet|
<td style=padding-left:1em;
 Amount:
<td
 <input.textinput size=15 name=#{amtvar} value=""
|]
                     ,"eg: $6"
                     )
       | otherwise = ("From account" :: String
                     ,"eg: assets:bank:checking" :: String
                     ,nulltemplate
                     ,"" :: String
                     )

-- | Edit journal form.
editform :: ViewData -> HtmlUrl AppRoute
editform VD{..} = [hamlet|
<form#editform method=POST style=display:none;
 <h2#contenttitle>#{title}
 <table.form
  $if manyfiles
   <tr
    <td colspan=2
     Editing ^{journalselect $ files j}
  <tr
   <td colspan=2
    <!-- XXX textarea ids are unquoted journal file paths here, not valid html -->
    $forall f <- files j
     <textarea id=#{fst f}_textarea name=text rows=25 cols=80 style=display:none; disabled=disabled
      #{snd f}
  <tr#addbuttonrow
   <td
    <span.help>^{formathelp}
   <td align=right
    <span.help
     Are you sure ? This will overwrite the journal. #
    <input type=hidden name=action value=edit
    <input type=submit name=submit value="save journal"
    \ or #
    <a href="#" onclick="return editformToggle(event)">cancel
|]
  where
    title = "Edit journal" :: String
    manyfiles = (length $ files j) > 1
    formathelp = helplink "file-format" "file format help"

-- | Import journal form.
importform :: HtmlUrl AppRoute
importform = [hamlet|
<form#importform method=POST style=display:none;
 <table.form
  <tr
   <td
    <input type=file name=file
    <input type=hidden name=action value=import
    <input type=submit name=submit value="import from file"
    \ or #
    <a href="#" onclick="return importformToggle(event)" cancel
|]

journalselect :: [(FilePath,String)] -> HtmlUrl AppRoute
journalselect journalfiles = [hamlet|
<select id=journalselect name=journal onchange="editformJournalSelect(event)"
 $forall f <- journalfiles
  <option value=#{fst f}>#{fst f}
|]

nulltemplate :: HtmlUrl AppRoute
nulltemplate = [hamlet||]

----------------------------------------------------------------------
-- Utilities

-- | A bundle of data useful for hledger-web request handlers and templates.
data ViewData = VD {
     opts         :: WebOpts    -- ^ the command-line options at startup
    ,here         :: AppRoute   -- ^ the current route
    ,msg          :: Maybe Html -- ^ the current UI message if any, possibly from the current request
    ,today        :: Day        -- ^ today's date (for queries containing relative dates)
    ,j            :: Journal    -- ^ the up-to-date parsed unfiltered journal
    ,q            :: String     -- ^ the current q parameter, the main query expression
    ,m            :: Query    -- ^ a query parsed from the q parameter
    ,qopts        :: [QueryOpt] -- ^ query options parsed from the q parameter
    ,am           :: Query    -- ^ a query parsed from the accounts sidebar query expr ("a" parameter)
    ,aopts        :: [QueryOpt] -- ^ query options parsed from the accounts sidebar query expr
    ,showpostings :: Bool       -- ^ current p parameter, 1 or 0 shows/hides all postings where applicable
    }

-- | Make a default ViewData, using day 0 as today's date.
nullviewdata :: ViewData
nullviewdata = viewdataWithDateAndParams nulldate "" "" ""

-- | Make a ViewData using the given date and request parameters, and defaults elsewhere.
viewdataWithDateAndParams :: Day -> String -> String -> String -> ViewData
viewdataWithDateAndParams d q a p =
    let (querymatcher,queryopts) = parseQuery d q
        (acctsmatcher,acctsopts) = parseQuery d a
    in VD {
           opts         = defwebopts
          ,j            = nulljournal
          ,here         = RootR
          ,msg          = Nothing
          ,today        = d
          ,q            = q
          ,m            = querymatcher
          ,qopts        = queryopts
          ,am           = acctsmatcher
          ,aopts        = acctsopts
          ,showpostings = p == "1"
          }

-- | Gather data used by handlers and templates in the current request.
getViewData :: Handler ViewData
getViewData = do
  app        <- getYesod
  let opts@WebOpts{cliopts_=copts@CliOpts{reportopts_=ropts}} = appOpts app
  (j, err)   <- getCurrentJournal $ copts{reportopts_=ropts{no_elide_=True}}
  msg        <- getMessageOr err
  Just here  <- getCurrentRoute
  today      <- liftIO getCurrentDay
  q          <- getParameterOrNull "q"
  a          <- getParameterOrNull "a"
  p          <- getParameterOrNull "p"
  return (viewdataWithDateAndParams today q a p){
               opts=opts
              ,msg=msg
              ,here=here
              ,today=today
              ,j=j
              }
    where
      -- | Update our copy of the journal if the file changed. If there is an
      -- error while reloading, keep the old one and return the error, and set a
      -- ui message.
      getCurrentJournal :: CliOpts -> Handler (Journal, Maybe String)
      getCurrentJournal opts = do
        j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
        (jE, changed) <- liftIO $ journalReloadIfChanged opts j
        if not changed
         then return (j,Nothing)
         else case jE of
                Right j' -> do liftIO $ putValue "hledger" "journal" j'
                               return (j',Nothing)
                Left e  -> do setMessage $ "error while reading" {- ++ ": " ++ e-}
                              return (j, Just e)

      -- | Get the named request parameter, or the empty string if not present.
      getParameterOrNull :: String -> Handler String
      getParameterOrNull p = unpack `fmap` fromMaybe "" <$> lookupGetParam (pack p)

-- | Get the message set by the last request, or the newer message provided, if any.
getMessageOr :: Maybe String -> Handler (Maybe Html)
getMessageOr mnewmsg = do
  oldmsg <- getMessage
  return $ maybe oldmsg (Just . toHtml) mnewmsg

numbered :: [a] -> [(Int,a)]
numbered = zip [1..]

dayToJsTimestamp :: Day -> Integer
dayToJsTimestamp d = read (formatTime defaultTimeLocale "%s" t) * 1000
                     where t = UTCTime d (secondsToDiffTime 0)
