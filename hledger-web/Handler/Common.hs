-- | Common page components.

module Handler.Common where

import Import

import Data.List (sort, nub)
import System.FilePath (takeFileName)

import Handler.Utils
import Hledger.Data
import Hledger.Query
import Hledger.Reports
import Hledger.Cli.Options
import Hledger.Web.Options


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
  <table>
   <tr>
    <td>
     Search:
     \ #
    <td>
     <input name=q size=70 value=#{q}>
     <input type=submit value="Search">
     $if filtering
      \ #
      <span.showall>
       <a href=@{here}>clear search
     \ #
     <a#search-help-link href="#" title="Toggle search help">help
   <tr>
    <td>
    <td>
     <div#search-help.help style="display:none;">
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
        <select id=description name=description>
         <option>
         $forall d <- descriptions
          <option value=#{d}>#{d}
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
  manyfiles = (length $ files j) > 1
  postingfields :: ViewData -> Int -> HtmlUrl AppRoute
  postingfields _ n = [hamlet|
<tr#postingrow>
 <td align=right>#{acctlabel}:
 <td>
  <select id=#{acctvar} name=#{acctvar}>
   <option>
   $forall a <- acctnames
    <option value=#{a} :shouldselect a:selected>#{a}
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
    shouldselect a = n == 2 && maybe False ((a==).fst) (inAccount qopts)
    withnumber = (++ show n)
    acctvar = withnumber "account"
    amtvar = withnumber "amount"
    acctnames = sort $ journalAccountNamesUsed j
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

nulltemplate :: HtmlUrl AppRoute
nulltemplate = [hamlet||]

