-- | Common page components and rendering helpers.
-- For global page layout, see Application.hs.

module Handler.Common where

import Import

import Data.List
import Data.Text(pack)
import Data.Time.Calendar
import System.FilePath (takeFileName)
#if BLAZE_HTML_0_4
import Text.Blaze (preEscapedString)
#else
import Text.Blaze.Internal (preEscapedString)
#endif
import Text.Printf

import Hledger.Utils
import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Cli.Options
import Hledger.Web.Options

-------------------------------------------------------------------------------
-- Common page layout

-- | Standard hledger-web page layout.
hledgerLayout :: ViewData -> String -> HtmlUrl AppRoute -> HandlerT App IO Html
hledgerLayout vd title content = do
  defaultLayout $ do
      setTitle $ toHtml $ title ++ " - hledger-web"
      toWidget [hamlet|
        <div#content>
         $if showsidebar vd
          <div#sidebar>
           <div#sidebar-spacer>
           <div#sidebar-body>
            ^{sidebar vd}
         $else
          <div#sidebar style="display:none;">
           <div#sidebar-spacer>
           <div#sidebar-body>
         <div#main>
          ^{topbar vd}
          <div#maincontent>
           ^{searchform vd}
           ^{content}
      |]

-- | Global toolbar/heading area.
topbar :: ViewData -> HtmlUrl AppRoute
topbar VD{..} = [hamlet|
<nav class="navbar" role="navigation">
 <div#topbar>
  <h1>#{title}
|]
  where
    title = takeFileName $ journalFilePath j

-- | The sidebar used on most views.
sidebar :: ViewData -> HtmlUrl AppRoute
sidebar vd@VD{..} =
 [hamlet|
 <a href=@{JournalR} title="Go back to top">
  hledger-web

 <p>
 <!--
 <a#sidebartogglebtn role="button" style="cursor:pointer;" onclick="sidebarToggle()" title="Show/hide sidebar">
  <span class="glyphicon glyphicon-expand"></span>
 -->
 <br>
 <div#sidebar-content>
  <p style="margin-top:1em;">
   <a href=@{JournalR} .#{journalcurrent} title="Show general journal entries, most recent first" style="white-space:nowrap;">Journal
  <div#accounts style="margin-top:1em;">
   ^{accounts}
|]
 where
  journalcurrent = if here == JournalR then "current" else "" :: String
  accounts = balanceReportAsHtml opts vd $ balanceReport (reportopts_ $ cliopts_ opts){empty_=True} am j

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
    <td width="99%" style="position:relative;">
     $if filtering
      <a role=button .btn .close style="position:absolute; right:0; padding-right:.1em; padding-left:.1em; margin-right:.1em; margin-left:.1em; font-size:24px;" href="@{here}" title="Clear search terms">&times;
     <input .form-control style="font-size:18px; padding-bottom:2px;" name=q value=#{q} title="Enter hledger search patterns to filter the data below">
    <td width="1%" style="white-space:nowrap;">
     <button .btn style="font-size:18px;" type=submit title="Apply search terms">Search
     <button .btn style="font-size:18px;" type=button data-toggle="modal" data-target="#helpmodal" title="Show search and general help">?
|]
 where
  filtering = not $ null q

-- -- | Edit journal form.
-- editform :: ViewData -> HtmlUrl AppRoute
-- editform VD{..} = [hamlet|
-- <form#editform method=POST style=display:none;>
--  <h2#contenttitle>#{title}>
--  <table.form>
--   $if manyfiles
--    <tr>
--     <td colspan=2>
--      Editing ^{journalselect $ files j}
--   <tr>
--    <td colspan=2>
--     <!-- XXX textarea ids are unquoted journal file paths here, not valid html -->
--     $forall f <- files j
--      <textarea id=#{fst f}_textarea name=text rows=25 cols=80 style=display:none; disabled=disabled>
--       \#{snd f}
--   <tr#addbuttonrow>
--    <td>
--     <span.help>^{formathelp}
--    <td align=right>
--     <span.help>
--      Are you sure ? This will overwrite the journal. #
--     <input type=hidden name=action value=edit>
--     <input type=submit name=submit value="save journal">
--     \ or #
--     <a href="#" onclick="return editformToggle(event)">cancel
-- |]
--   where
--     title = "Edit journal" :: String
--     manyfiles = length (files j) > 1
--     formathelp = helplink "file-format" "file format help"

-- -- | Import journal form.
-- importform :: HtmlUrl AppRoute
-- importform = [hamlet|
-- <form#importform method=POST style=display:none;>
--  <table.form>
--   <tr>
--    <td>
--     <input type=file name=file>
--     <input type=hidden name=action value=import>
--     <input type=submit name=submit value="import from file">
--     \ or #
--     <a href="#" onclick="return importformToggle(event)">cancel
-- |]

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

-- | Render a "BalanceReport" as html.
balanceReportAsHtml :: WebOpts -> ViewData -> BalanceReport -> HtmlUrl AppRoute
balanceReportAsHtml _ vd@VD{..} (items',total) =
 [hamlet|
 <table.balancereport>
  <tr>
   <td>Account
   <td style="padding-left:1em; text-align:right;">Balance
  $forall i <- items
   ^{itemAsHtml vd i}
  <tr.totalrule>
   <td colspan=2>
  <tr>
   <td>
   <td.balance align=right>#{mixedAmountAsHtml total}
|]
 where
   l = ledgerFromJournal Any j
   inacctmatcher = inAccountQuery qopts
   items = items' -- maybe items' (\m -> filter (matchesAccount m . \(a,_,_,_)->a) items') showacctmatcher
   itemAsHtml :: ViewData -> BalanceReportItem -> HtmlUrl AppRoute
   itemAsHtml _ ((acct, adisplay, aindent), abal) = [hamlet|
<tr.item.#{inacctclass}>
 <td.account.#{depthclass}>
  \#{indent}
   <a href="@?{acctquery}" title="Show transactions affecting this account and subaccounts">#{adisplay}
   <span.hoverlinks>
    $if hassubs
     &nbsp;
     <a href="@?{acctonlyquery}" title="Show transactions affecting this account but not subaccounts">only

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

-- stringIfLongerThan :: Int -> String -> String
-- stringIfLongerThan n s = if length s > n then s else ""

numberTransactionsReportItems :: [TransactionsReportItem] -> [(Int,Bool,Bool,Bool,TransactionsReportItem)]
numberTransactionsReportItems [] = []
numberTransactionsReportItems items = number 0 nulldate items
  where
    number :: Int -> Day -> [TransactionsReportItem] -> [(Int,Bool,Bool,Bool,TransactionsReportItem)]
    number _ _ [] = []
    number n prevd (i@(Transaction{tdate=d},_,_,_,_,_):rest)  = (n+1,newday,newmonth,newyear,i): number (n+1) d rest
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

