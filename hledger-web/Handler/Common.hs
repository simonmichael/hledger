{-# LANGUAGE CPP, OverloadedStrings, QuasiQuotes, RecordWildCards #-}
-- | Common page components and rendering helpers.
-- For global page layout, see Application.hs.

module Handler.Common where

import Import

import Data.List
-- import Data.Text (Text)
import qualified Data.Text as T
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
import Hledger.Cli.CliOptions
import Hledger.Web.WebOptions

-------------------------------------------------------------------------------
-- Common page layout

-- | Standard hledger-web page layout.
hledgerLayout :: ViewData -> String -> HtmlUrl AppRoute -> HandlerT App IO Html
hledgerLayout vd title content = do
  defaultLayout $ do
      setTitle $ toHtml $ title ++ " - hledger-web"
      toWidget [hamlet|
         ^{topbar vd}
         ^{sidebar vd}
         <div #main-content .col-xs-12 .#{showmd} .#{showsm}>
          ^{searchform vd}
          ^{content}
      |]
  where
    showmd = if showsidebar vd then "col-md-8" else "col-md-12" :: String
    showsm = if showsidebar vd then "col-sm-8" else "col-sm-12" :: String

-- | Global toolbar/heading area.
topbar :: ViewData -> HtmlUrl AppRoute
topbar VD{..} = [hamlet|
<div#spacer .col-md-4 .col-sm-4 .col-xs-2>
 <h1>
  <button .visible-xs .btn .btn-default type="button" data-toggle="offcanvas">
   <span .glyphicon .glyphicon-align-left .tgl-icon>
<div#topbar .col-md-8 .col-sm-8 .col-xs-10>
 <h1>#{title}

|]
  where
    title = takeFileName $ journalFilePath j

-- | The sidebar used on most views.
sidebar :: ViewData -> HtmlUrl AppRoute
sidebar vd@VD{..} =
 [hamlet|
 <div #sidebar-menu .#{showmd} .#{showsm} .sidebar-offcanvas>
  <ul .main-menu .nav .nav-stacked .affix-top>
   <li .top>
    <a href=@{JournalR} .#{journalcurrent} title="Show general journal entries, most recent first">Journal
   ^{accounts}
|]
 where
  journalcurrent = if here == JournalR then "inacct" else "" :: String
  accounts = balanceReportAsHtml opts vd $ balanceReport (reportopts_ $ cliopts_ opts){empty_=True} am j
  showmd = if showsidebar then "col-md-4" else "col-any-0" :: String
  showsm = if showsidebar then "col-sm-4" else "" :: String

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
<div#searchformdiv .row>
 <form#searchform .form-inline method=GET>
  <div .form-group .col-md-12 .col-sm-12 .col-xs-12>
    <div #searchbar .input-group>
     <input .form-control name=q value=#{q} title="Enter hledger search patterns to filter the data below" placeholder="Search">
     <div .input-group-btn>
      $if filtering
       <a href=@{here} .btn .btn-default title="Clear search terms">
        <span .glyphicon .glyphicon-remove-circle>
      <button .btn .btn-default type=submit title="Apply search terms">
       <span .glyphicon .glyphicon-search>
      <button .btn .btn-default type=button data-toggle="modal" data-target="#helpmodal" title="Show search and general help">?
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
--    <td>
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
  $forall i <- items
   ^{itemAsHtml vd i}
  <li .total>
    <span .balance>#{mixedAmountAsHtml total}
|]
 where
   l = ledgerFromJournal Any j
   inacctmatcher = inAccountQuery qopts
   items = items' -- maybe items' (\m -> filter (matchesAccount m . \(a,_,_,_)->a) items') showacctmatcher
   itemAsHtml :: ViewData -> BalanceReportItem -> HtmlUrl AppRoute
   itemAsHtml _ (acct, adisplay, aindent, abal) = [hamlet|
<li>
 \#{indent}
 <a href="@?{acctquery}" .#{inacctclass} title="Show transactions affecting this account and subaccounts">#{adisplay}
 $if hassubs
  <a href="@?{acctonlyquery}" .only .hidden-sm .hidden-xs title="Show transactions affecting this account but not subaccounts">only
 <span .balance>#{mixedAmountAsHtml abal}
|]
     where
       hassubs = not $ maybe False (null.asubs) $ ledgerAccount l acct
       inacctclass = case inacctmatcher of
                       Just m' -> if m' `matchesAccount` acct then "inacct" else ""
                       Nothing -> "" :: String
       indent = preEscapedString $ concat $ replicate (2 * (1+aindent)) "&nbsp;"
       acctquery = (RegisterR, [("q", T.pack $ accountQuery acct)])
       acctonlyquery = (RegisterR, [("q", T.pack $ accountOnlyQuery acct)])

accountQuery :: AccountName -> String
accountQuery a = "inacct:" ++ T.unpack (quoteIfSpaced a) -- (accountNameToAccountRegex a)

accountOnlyQuery :: AccountName -> String
accountOnlyQuery a = "inacctonly:" ++ T.unpack (quoteIfSpaced a ) -- (accountNameToAccountRegex a)

accountUrl :: AppRoute -> AccountName -> (AppRoute, [(Text, Text)])
accountUrl r a = (r, [("q", T.pack $ accountQuery a)])

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

