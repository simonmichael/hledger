{-# LANGUAGE CPP, OverloadedStrings, QuasiQuotes, NamedFieldPuns #-}
-- | Common page components and rendering helpers.
-- For global page layout, see Application.hs.

module Handler.Common where

import Import

import qualified Data.Text as T
import Data.Time.Calendar (Day, toGregorian)
import System.FilePath (takeFileName)
import Text.Blaze (ToMarkup)
import Text.Blaze.Internal (preEscapedString)
import Text.Printf (printf)

import Hledger.Cli.CliOptions
import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Utils
import Hledger.Web.WebOptions

-------------------------------------------------------------------------------
-- Common page layout

-- | Standard hledger-web page layout.
#if MIN_VERSION_yesod(1,6,0)
hledgerLayout :: ViewData -> Text -> HtmlUrl AppRoute -> HandlerFor App Html
#else
hledgerLayout :: ViewData -> Text -> HtmlUrl AppRoute -> HandlerT App IO Html
#endif
hledgerLayout vd title content = do
  defaultLayout $ do
      setTitle $ toHtml $ title <> " - hledger-web"
      toWidget [hamlet|
         ^{topbar vd}
         ^{sidebar vd}
         <div #main-content .col-xs-12 .#{showmd} .#{showsm}>
          ^{searchform vd}
          ^{content}
      |]
  where
    showmd = if showsidebar vd then "col-md-8" else "col-md-12" :: Text
    showsm = if showsidebar vd then "col-sm-8" else "col-sm-12" :: Text

-- | Global toolbar/heading area.
topbar :: ViewData -> HtmlUrl AppRoute
topbar VD{j, showsidebar} = [hamlet|
<div#spacer .#{showsm} .#{showmd} .col-xs-2>
 <h1>
  <button .visible-xs .btn .btn-default type="button" data-toggle="offcanvas">
   <span .glyphicon .glyphicon-align-left .tgl-icon>
<div#topbar .col-md-8 .col-sm-8 .col-xs-10>
 <h1>#{title}
|]
  where
    title = takeFileName $ journalFilePath j
    showmd = if showsidebar then "col-md-4" else "col-any-0" :: Text
    showsm = if showsidebar then "col-sm-4" else "" :: Text

-- | The sidebar used on most views.
sidebar :: ViewData -> HtmlUrl AppRoute
sidebar vd@VD{am, here, j, opts, showsidebar} =
 [hamlet|
 <div #sidebar-menu .#{showmd} .#{showsm} .sidebar-offcanvas>
  <table .main-menu .table>
   <tr .#{journalcurrent}>
    <td .top .acct>
     <a href=@{JournalR} .#{journalcurrent} title="Show general journal entries, most recent first">Journal
    <td .top>
   ^{accounts}
|]
 where
  journalcurrent = if here == JournalR then "inacct" else "" :: Text
  ropts = reportopts_ $ cliopts_ opts
  -- flip the default for items with zero amounts, show them by default
  ropts' = ropts{empty_=not $ empty_ ropts}
  accounts = balanceReportAsHtml vd $ balanceReport ropts' am j
  showmd = if showsidebar then "col-md-4" else "col-any-0" :: Text
  showsm = if showsidebar then "col-sm-4" else "" :: Text

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
searchform VD{q, here} = [hamlet|
<div#searchformdiv .row>
 <form#searchform .form-inline method=GET>
  <div .form-group .col-md-12 .col-sm-12 .col-xs-12>
    <div #searchbar .input-group>
     <input .form-control name=q value=#{q} title="Enter hledger search patterns to filter the data below" placeholder="Search">
     <div .input-group-btn>
      $if not (T.null q)
       <a href=@{here} .btn .btn-default title="Clear search terms">
        <span .glyphicon .glyphicon-remove-circle>
      <button .btn .btn-default type=submit title="Apply search terms">
       <span .glyphicon .glyphicon-search>
      <button .btn .btn-default type=button data-toggle="modal" data-target="#helpmodal" title="Show search and general help">?
|]

-- | Link to a topic in the manual.
helplink :: Text -> Text -> HtmlUrl AppRoute
helplink topic label = [hamlet|
<a href=#{u} target=hledgerhelp>#{label}
|]
    where u = manualurl <> if T.null topic then "" else T.cons '#' topic

----------------------------------------------------------------------
-- hledger report renderers

-- | Render a "BalanceReport" as html.
balanceReportAsHtml :: ViewData -> BalanceReport -> HtmlUrl AppRoute
balanceReportAsHtml VD{j, qopts} (items, total) =
 [hamlet|
  $forall i <- items
   ^{itemAsHtml i}
  <tr .total>
   <td>
   <td>
    #{mixedAmountAsHtml total}
|]
 where
   l = ledgerFromJournal Any j
   inacctmatcher = inAccountQuery qopts
   itemAsHtml :: BalanceReportItem -> HtmlUrl AppRoute
   itemAsHtml (acct, adisplay, aindent, abal) = [hamlet|
<tr .#{inacctclass}>
 <td .acct>
  <div .ff-wrapper>
   \#{indent}
   <a href="@?{acctquery}" .#{inacctclass} title="Show transactions affecting this account and subaccounts">#{adisplay}
   $if hassubs
    <a href="@?{acctonlyquery}" .only .hidden-sm .hidden-xs title="Show transactions affecting this account but not subaccounts">only
 <td>
  #{mixedAmountAsHtml abal}
|]
     where
       hassubs = not $ maybe False (null.asubs) $ ledgerAccount l acct
       inacctclass = case inacctmatcher of
         Just m' -> if m' `matchesAccount` acct then "inacct" else ""
         Nothing -> "" :: Text
       indent = preEscapedString $ concat $ replicate (2 * (1+aindent)) "&nbsp;"
       acctquery = (RegisterR, [("q", accountQuery acct)])
       acctonlyquery = (RegisterR, [("q", accountOnlyQuery acct)])

accountQuery :: AccountName -> Text
accountQuery = ("inacct:" <>) .  quoteIfSpaced

accountOnlyQuery :: AccountName -> Text
accountOnlyQuery = ("inacctonly:" <>) . quoteIfSpaced

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
mixedAmountAsHtml b = preEscapedString $ unlines $ map addclass $ lines $ showMixedAmountWithoutPrice b
    where addclass = printf "<span class=\"%s\">%s</span><br/>" (c :: Text)
          c = case isNegativeMixedAmount b of
            Just True -> "negative amount"
            _         -> "positive amount"

showErrors :: ToMarkup a => [a] -> Handler ()
showErrors errs = setMessage [shamlet|
Errors:<br>
$forall e<-errs
  \#{e}<br>
|]
