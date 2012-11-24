-- | Web utilities and rendering helpers.

module Handler.Utils where

import Prelude
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.List
import Data.Maybe
import Data.Text(Text,pack,unpack)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.IO.Storage (putValue, getValue)
import System.Locale (defaultTimeLocale)
#if BLAZE_HTML_0_5
import Text.Blaze.Internal (preEscapedString)
import Text.Blaze.Html (toHtml)
#else
import Text.Blaze (preEscapedString, toHtml)
#endif
import Text.Hamlet -- hiding (hamlet)
import Text.Printf
import Yesod.Core
-- import Yesod.Json

import Foundation
import Settings

import Hledger hiding (is)
import Hledger.Cli hiding (version)
import Hledger.Web.Options


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
dayToJsTimestamp d = read (formatTime defaultTimeLocale "%s" t) * 1000 -- XXX read
                     where t = UTCTime d (secondsToDiffTime 0)

chomp :: String -> String
chomp = reverse . dropWhile (`elem` "\r\n") . reverse


----------------------------------------------------------------------
-- Rendering helpers

-- | Link to a topic in the manual.
helplink :: String -> String -> HtmlUrl AppRoute
helplink topic label = [hamlet|
<a href=#{u} target=hledgerhelp>#{label}
|]
    where u = manualurl ++ if null topic then "" else '#':topic

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
<table.journalreport>
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
    number n prevd (i@(Transaction{tdate=d},_,_,_,_,_):is)  = (n+1,newday,newmonth,newyear,i):(number (n+1) d is)
        where
          newday = d/=prevd
          newmonth = dm/=prevdm || dy/=prevdy
          newyear = dy/=prevdy
          (dy,dm,_) = toGregorian d
          (prevdy,prevdm,_) = toGregorian prevd

mixedAmountAsHtml :: MixedAmount -> Html
mixedAmountAsHtml b = preEscapedString $ addclass $ intercalate "<br>" $ lines $ showMixedAmount b
    where addclass = printf "<span class=\"%s\">%s</span>" (c :: String)
          c = case isNegativeMixedAmount b of Just True -> "negative amount"
                                              _         -> "positive amount"

