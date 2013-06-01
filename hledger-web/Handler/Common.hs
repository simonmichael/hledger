-- | Common page components and rendering helpers.
-- For global page layout, see Application.hs.

module Handler.Common where

import Import

import Data.List
import Data.Maybe
import Data.Text(pack)
import Data.Time.Calendar
import System.FilePath (takeFileName)
#if BLAZE_HTML_0_4
import Text.Blaze (preEscapedString)
#else
import Text.Blaze.Internal (preEscapedString)
#endif
import Text.Printf
import Text.JSON

import Hledger.Utils
import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Cli.Options
import Hledger.Web.Options

import Handler.Utils

-------------------------------------------------------------------------------
-- Page components

-- | Global toolbar/heading area.
topbar :: ViewData -> HtmlUrl AppRoute
topbar VD{..} = [hamlet|
<div#topbar>
 <a.topleftlink href=#{hledgerorgurl} title="More about hledger">
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

-- | The sidebar used on most views.
sidebar :: ViewData -> HtmlUrl AppRoute
sidebar vd@VD{..} = accountsReportAsHtml opts vd $ accountsReport (reportopts_ $ cliopts_ opts) am j

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

-- | Search form for entering custom queries to filter journal data.
searchform :: ViewData -> HtmlUrl AppRoute
searchform VD{..} = [hamlet|
<div#searchformdiv>
 <form#searchform.form method=GET>
  <table width="100%">
   <tr>
    <td width="99%">
     <input name=q value=#{q} style="width:98%;">
    <td width="1%">
     <input type=submit value="Search">
   <tr valign=top>
    <td colspan=2 style="text-align:right;">
     $if filtering
      \ #
      <span.showall>
       <a href=@{here}>clear
     \ #
     <a#search-help-link href="#" title="Toggle search help">help
   <tr>
    <td colspan=2>
     <div#search-help.help style="display:none;">
      Leave blank to see journal (all transactions), or click account links to see transactions under that account.
      <br>
      Transactions/postings may additionally be filtered by
      acct:REGEXP (target account), #
      code:REGEXP (transaction code), #
      desc:REGEXP (description), #
      date:PERIODEXP (date), #
      date2:PERIODEXP (secondary date), #
      tag:TAG[=REGEX] (tag and optionally tag value), #
      depth:N (accounts at or above this depth), #
      status:*, status:!, status:  (cleared status), #
      real:BOOL (real/virtual-ness), #
      empty:BOOL (is amount zero), #
      amt:<N, amt:=N, amt:>N (test magnitude of single-commodity amount).
      <br>
      Prepend not: to negate, enclose multi-word patterns in quotes, multiple search terms are AND'ed.
|]
 where
  filtering = not $ null q

-- | Add transaction form.
addform :: Text -> ViewData -> HtmlUrl AppRoute
addform _ vd@VD{..} = [hamlet|
<script type=text/javascript>
 \$(document).ready(function() {
    /* select2 setup */
    var param = {
      "width": "250px",
      "openOnEnter": false,
      // createSearchChoice allows to create new values not in the options
      "createSearchChoice":function(term, data) {
        if ( $(data).filter( function() {
                return this.text.localeCompare(term)===0;
                }).length===0) {
          return {text:term};
        }
      },
      // id is what is passed during post
      "id": function(object) {
        return object.text;
      }
    };
    \$("#description").select2($.extend({}, param, {data: #{toSelectData descriptions} }));
    var accountData = $.extend({}, param, {data: #{toSelectData acctnames} });
    \$("#account1").select2(accountData);
    \$("#account2").select2(accountData);
 });

<form#addform method=POST style=display:none;>
  <h2#contenttitle>#{title}
  <table.form>
   <tr>
    <td colspan=4>
     <table>
      <tr#descriptionrow>
       <td>
        Date:
       <td>
        <input.textinput size=15 name=date value=#{date}>
       <td style=padding-left:1em;>
        Description:
       <td>
        <input type=hidden id=description name=description>
      <tr.helprow>
       <td>
       <td>
        <span.help>#{datehelp} #
       <td>
       <td>
        <span.help>#{deschelp}
   ^{postingfields vd 1}
   ^{postingfields vd 2}
   <tr#addbuttonrow>
    <td colspan=4>
     <input type=hidden name=action value=add>
     <input type=submit name=submit value="add transaction">
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
  acctnames = sort $ journalAccountNamesUsed j
  -- Construct data for select2. Text must be quoted in a json string.
  toSelectData as  = preEscapedString $ encode $ JSArray $ map (\a -> JSObject $ toJSObject [("text", showJSON a)]) as
  manyfiles = (length $ files j) > 1
  postingfields :: ViewData -> Int -> HtmlUrl AppRoute
  postingfields _ n = [hamlet|
<tr#postingrow>
 <td align=right>#{acctlabel}:
 <td>
  <input type=hidden id=#{acctvar} name=#{acctvar}>
 ^{amtfield}
<tr.helprow>
 <td>
 <td>
  <span.help>#{accthelp}
 <td>
 <td>
  <span.help>#{amthelp}
|]
   where
    withnumber = (++ show n)
    acctvar = withnumber "account"
    amtvar = withnumber "amount"
    (acctlabel, accthelp, amtfield, amthelp)
       | n == 1     = ("To account"
                     ,"eg: expenses:food"
                     ,[hamlet|
<td style=padding-left:1em;>
 Amount:
<td>
 <input.textinput size=15 name=#{amtvar} value="">
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
<form#editform method=POST style=display:none;>
 <h2#contenttitle>#{title}>
 <table.form>
  $if manyfiles
   <tr>
    <td colspan=2>
     Editing ^{journalselect $ files j}
  <tr>
   <td colspan=2>
    <!-- XXX textarea ids are unquoted journal file paths here, not valid html -->
    $forall f <- files j
     <textarea id=#{fst f}_textarea name=text rows=25 cols=80 style=display:none; disabled=disabled>
      #{snd f}
  <tr#addbuttonrow>
   <td>
    <span.help>^{formathelp}
   <td align=right>
    <span.help>
     Are you sure ? This will overwrite the journal. #
    <input type=hidden name=action value=edit>
    <input type=submit name=submit value="save journal">
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
<form#importform method=POST style=display:none;>
 <table.form>
  <tr>
   <td>
    <input type=file name=file>
    <input type=hidden name=action value=import>
    <input type=submit name=submit value="import from file">
    \ or #
    <a href="#" onclick="return importformToggle(event)">cancel
|]

journalselect :: [(FilePath,String)] -> HtmlUrl AppRoute
journalselect journalfiles = [hamlet|
<select id=journalselect name=journal onchange="editformJournalSelect(event)">
 $forall f <- journalfiles
  <option value=#{fst f}>#{fst f}
|]

-- | Link to a topic in the manual.
helplink :: String -> String -> HtmlUrl AppRoute
helplink topic label = [hamlet|
<a href=#{u} target=hledgerhelp>#{label}
|]
    where u = manualurl ++ if null topic then "" else '#':topic

nulltemplate :: HtmlUrl AppRoute
nulltemplate = [hamlet||]


----------------------------------------------------------------------
-- hledger report renderers

-- | Render an "AccountsReport" as html.
accountsReportAsHtml :: WebOpts -> ViewData -> AccountsReport -> HtmlUrl AppRoute
accountsReportAsHtml _ vd@VD{..} (items',total) =
 [hamlet|
<div#accountsheading>
 <a#accounts-toggle-link.togglelink href="#" title="Toggle sidebar">[+]
<div#accounts>
 <table.balancereport>
  <tr>
   <td.add colspan=3>
    <br>
    <a#addformlink href="#" onclick="return addformToggle(event)" title="Add a new transaction to the journal">Add a transaction..

  <tr.item :allaccts:.inacct>
   <td.journal colspan=3>
    <br>
    <a href=@{JournalR} title="Show all transactions in journal format">Journal
    <span.hoverlinks>
     &nbsp;
     <a href=@{JournalEntriesR} title="Show journal entries">entries
     &nbsp;
     <a#editformlink href="#" onclick="return editformToggle(event)" title="Edit the journal">
      edit

  <tr>
   <td colspan=3>
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
   l = ledgerFromJournal Any j
   inacctmatcher = inAccountQuery qopts
   allaccts = isNothing inacctmatcher
   items = items' -- maybe items' (\m -> filter (matchesAccount m . \(a,_,_,_)->a) items') showacctmatcher
   itemAsHtml :: ViewData -> AccountsReportItem -> HtmlUrl AppRoute
   itemAsHtml _ (acct, adisplay, aindent, abal) = [hamlet|
<tr.item.#{inacctclass}>
 <td.account.#{depthclass}>
  #{indent}
  <a href="@?{acctquery}" title="Show transactions in this account, including subaccounts">#{adisplay}
  <span.hoverlinks>
   $if hassubs
    &nbsp;
    <a href="@?{acctonlyquery}" title="Show transactions in this account only">only
   <!--
    &nbsp;
    <a href="@?{acctsonlyquery}" title="Focus on this account and sub-accounts and hide others">-others -->

 <td.balance align=right>#{mixedAmountAsHtml abal}
|]
     where
       hassubs = not $ maybe False (null.asubs) $ ledgerAccount l acct
 -- <td.numpostings align=right title="#{numpostings} transactions in this account">(#{numpostings})
       -- numpostings = maybe 0 (length.apostings) $ ledgerAccount l acct
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

-- | Render a "TransactionsReport" as html for the formatted journal view.
journalTransactionsReportAsHtml :: WebOpts -> ViewData -> TransactionsReport -> HtmlUrl AppRoute
journalTransactionsReportAsHtml _ vd (_,items) = [hamlet|
<table.transactionsreport>
 <tr.headings>
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
<tr.item.#{evenodd}.#{firstposting}>
 <td.date>#{date}
 <td.description colspan=2 title="#{show t}">#{elideRight 60 desc}
 <td.amount align=right>
  $if showamt
   #{mixedAmountAsHtml amt}
$forall p' <- tpostings t
  <tr.item.#{evenodd}.posting>
   <td.date>
   <td.description>
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
<table.registerreport>
 <tr.headings>
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
<tr.item.#{evenodd}.#{firstposting}.#{datetransition}>
 <td.date>#{date}
 <td.description title="#{show t}">#{elideRight 30 desc}
 <td.account title="#{show t}">
  <a>
   #{elideRight 40 acct}
  &nbsp;
  <a.postings-toggle-link.togglelink href="#" title="Toggle all postings">
   [+]
 <td.amount align=right>
  $if showamt
   #{mixedAmountAsHtml amt}
 <td.balance align=right>#{mixedAmountAsHtml bal}
$forall p' <- tpostings t
 <tr.item.#{evenodd}.posting style=#{postingsdisplaystyle}>
   <td.date>
   <td.description>
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
<div#register-chart style="width:600px;height:100px; margin-bottom:1em;">
<script type=text/javascript>
 \$(document).ready(function() {
   /* render chart with flot, if visible */
   var chartdiv = $('#register-chart');
   if (chartdiv.is(':visible'))
     \$.plot(chartdiv,
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
|]

-- stringIfLongerThan :: Int -> String -> String
-- stringIfLongerThan n s = if length s > n then s else ""

numberTransactionsReportItems :: [TransactionsReportItem] -> [(Int,Bool,Bool,Bool,TransactionsReportItem)]
numberTransactionsReportItems [] = []
numberTransactionsReportItems items = number 0 nulldate items
  where
    number :: Int -> Day -> [TransactionsReportItem] -> [(Int,Bool,Bool,Bool,TransactionsReportItem)]
    number _ _ [] = []
    number n prevd (i@(Transaction{tdate=d},_,_,_,_,_):rest)  = (n+1,newday,newmonth,newyear,i):(number (n+1) d rest)
        where
          newday = d/=prevd
          newmonth = dm/=prevdm || dy/=prevdy
          newyear = dy/=prevdy
          (dy,dm,_) = toGregorian d
          (prevdy,prevdm,_) = toGregorian prevd

mixedAmountAsHtml :: MixedAmount -> Html
mixedAmountAsHtml b = preEscapedString $ addclass $ intercalate "<br>" $ lines $ showMixedAmountWithoutPrice b
    where addclass = printf "<span class=\"%s\">%s</span>" (c :: String)
          c = case isNegativeMixedAmount b of Just True -> "negative amount"
                                              _         -> "positive amount"

