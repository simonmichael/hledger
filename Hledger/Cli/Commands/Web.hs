{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, TemplateHaskell #-}
{-| 
A web-based UI.
-}

module Hledger.Cli.Commands.Web
where
import Control.Concurrent (forkIO, threadDelay)
import Control.Applicative ((<$>), (<*>))
import Data.Either
import System.FilePath ((</>))
import System.IO.Storage (withStore, putValue, getValue)
import Text.ParserCombinators.Parsec (parse)
import Yesod

import Hledger.Cli.Commands.Add (journalAddTransaction)
import Hledger.Cli.Commands.Balance
import Hledger.Cli.Commands.Print
import Hledger.Cli.Commands.Register
import Hledger.Cli.Options hiding (value)
import Hledger.Cli.Utils
import Hledger.Cli.Version (version)
import Hledger.Data
import Hledger.Read (journalFromPathAndString)
import Hledger.Read.Journal (someamount)
#ifdef MAKE
import Paths_hledger_make (getDataFileName)
#else
import Paths_hledger (getDataFileName)
#endif


defhost = "localhost"
defport = 5000
defbaseurl = printf "http://%s:%d" defhost defport :: String
browserstartdelay = 100000 -- microseconds
hledgerurl = "http://hledger.org"
manualurl = hledgerurl++"/MANUAL.html"

data HledgerWebApp = HledgerWebApp {
      appRoot    :: String
     ,appWebdir  :: FilePath
     ,appOpts    :: [Opt]
     ,appArgs    :: [String]
     ,appJournal :: Journal
     }

mkYesod "HledgerWebApp" [$parseRoutes|
/             IndexPage        GET
/journal      JournalPage      GET POST
/register     RegisterPage     GET
/balance      BalancePage      GET
/ledger       LedgerPage       GET
/style.css    StyleCss         GET
|]

instance Yesod HledgerWebApp where approot = appRoot

-- defaultroute = LedgerPage
defaultroute = JournalPage

-- | A bundle of useful data passed to templates.
data TemplateData = TD {
     here         :: HledgerWebAppRoute -- ^ the current page's route
    ,title        :: String             -- ^ page's title
    ,msg          :: Maybe (Html ())     -- ^ transient message
    ,a            :: String             -- ^ a (filter pattern) parameter
    ,p            :: String             -- ^ p (period expression) parameter
    }

mktd = TD {
      here = IndexPage
     ,title = "hledger"
     ,msg = Nothing
     ,a = ""
     ,p = ""
     }

-- | The web command.
web :: [Opt] -> [String] -> Journal -> IO ()
web opts args j = do
  let baseurl = fromMaybe defbaseurl $ baseUrlFromOpts opts
      port = fromMaybe defport $ portFromOpts opts
  unless (Debug `elem` opts) $ forkIO (browser baseurl) >> return ()
  server baseurl port opts args j

browser :: String -> IO ()
browser baseurl = do
  putStrLn "starting web browser"
  threadDelay browserstartdelay
  openBrowserOn baseurl
  return ()

server :: String -> Int -> [Opt] -> [String] -> Journal -> IO ()
server baseurl port opts args j = do
    printf "starting web server on port %d with base url %s\n" port baseurl
    fp <- getDataFileName "web"
    let app = HledgerWebApp{
               appRoot=baseurl
              ,appWebdir=fp
              ,appOpts=opts
              ,appArgs=args
              ,appJournal=j
              }
    withStore "hledger" $ do
     putValue "hledger" "journal" j
     basicHandler' port Nothing app

----------------------------------------------------------------------
-- handlers & templates

getStyleCss :: Handler HledgerWebApp ()
getStyleCss = do
    app <- getYesod
    let dir = appWebdir app
    sendFile "text/css" $ dir </> "style.css"

getIndexPage :: Handler HledgerWebApp ()
getIndexPage = redirect RedirectTemporary defaultroute

----------------------------------------------------------------------

-- | A basic journal view, like hledger print, with editing.
getJournalPage :: Handler HledgerWebApp RepHtml
getJournalPage = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  let td = mktd{here=here, title="hledger", msg=msg, a=a, p=p}
      editform' = editform td $ jtext j
      txns = journalReportAsHtml opts td $ journalReport opts fspec j
  hamletToRepHtml $ pageLayout td [$hamlet|
%div.journal
 ^journalScripts^
 %div.nav2
  %a#addformlink!href!onclick="return addformToggle()" add one transaction
  \ | $
  %a#editformlink!href!onclick="return editformToggle()" edit the whole journal
 ^addform^
 ^editform'^
 #transactions ^txns^
|]

-- | Render a journal report as HTML.
journalReportAsHtml :: [Opt] -> TemplateData -> JournalReport -> Hamlet HledgerWebAppRoute
journalReportAsHtml _ td items = [$hamlet|
%table.journalreport
 $forall number.items i
  ^itemAsHtml' i^
|]
 where
   number = zip [1..]
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> (Int, JournalReportItem) -> Hamlet HledgerWebAppRoute
   itemAsHtml _ (n, t) = [$hamlet|
     %tr.item.$evenodd$
      %td.transaction
       %pre $txn$
     |] where
       evenodd = if even n then "even" else "odd"
       txn = trimnl $ showTransaction t where trimnl = reverse . dropWhile (=='\n') . reverse

journalScripts = [$hamlet|
<script type="text/javascript">

 function addformToggle() {
  a = document.getElementById('addform');
  e = document.getElementById('editform');
  t = document.getElementById('transactions');
  alink = document.getElementById('addformlink');
  elink = document.getElementById('editformlink');
  if (a.style.display == 'none') {
   alink.style['font-weight'] = 'bold';
   elink.style['font-weight'] = 'normal';
   a.style.display = 'block';
   e.style.display = 'none';
   t.style.display = 'block';
  } else {
   alink.style['font-weight'] = 'normal';
   elink.style['font-weight'] = 'normal';
   a.style.display = 'none';
   e.style.display = 'none';
   t.style.display = 'block';
  }
  return false;
 }

 function editformToggle() {
  a = document.getElementById('addform');
  e = document.getElementById('editform');
  t = document.getElementById('transactions');
  alink = document.getElementById('addformlink');
  elink = document.getElementById('editformlink');
  if (e.style.display == 'none') {
   alink.style['font-weight'] = 'normal';
   elink.style['font-weight'] = 'bold';
   a.style.display = 'none';
   e.style.display = 'block';
   t.style.display = 'none';
  } else {
   alink.style['font-weight'] = 'normal';
   elink.style['font-weight'] = 'normal';
   a.style.display = 'none';
   e.style.display = 'none';
   t.style.display = 'block';
  }
  return false;
 }

</script>
|]

postJournalPage :: Handler HledgerWebApp RepPlain
postJournalPage = do
  edit <- runFormPost' $ maybeStringInput "edit"
  if isJust edit then postEditForm else postAddForm

-- | Handle a journal add form post.
postAddForm :: Handler HledgerWebApp RepPlain
postAddForm = do
  (_, _, opts, _, _, _, _) <- getHandlerParameters
  today <- liftIO getCurrentDay
  -- get form input values. M means a Maybe value.
  (dateM, descM, acct1M, amt1M, acct2M, amt2M) <- runFormPost'
    $ (,,,,,)
    <$> maybeStringInput "date"
    <*> maybeStringInput "description"
    <*> maybeStringInput "accountname1"
    <*> maybeStringInput "amount1"
    <*> maybeStringInput "accountname2"
    <*> maybeStringInput "amount2"
  -- supply defaults and parse date and amounts, or get errors.
  let dateE = maybe (Left "date required") (either (\e -> Left $ showDateParseError e) Right . fixSmartDateStrEither today) dateM
      descE = Right $ fromMaybe "" descM
      acct1E = maybe (Left "to account required") Right acct1M
      acct2E = maybe (Left "from account required") Right acct2M
      amt1E = maybe (Left "amount required") (either (const $ Left "could not parse amount") Right . parse someamount "") amt1M
      amt2E = maybe (Right missingamt)       (either (const $ Left "could not parse amount") Right . parse someamount "") amt2M
      strEs = [dateE, descE, acct1E, acct2E]
      amtEs = [amt1E, amt2E]
      [date,desc,acct1,acct2] = rights strEs
      [amt1,amt2] = rights amtEs
      errs = lefts strEs ++ lefts amtEs
      -- if no errors so far, generate a transaction and balance it or get the error.
      tE | not $ null errs = Left errs
         | otherwise = either (\e -> Left ["unbalanced postings: " ++ (head $ lines e)]) Right
                        (balanceTransaction $ nulltransaction {
                           tdate=parsedate date
                          ,teffectivedate=Nothing
                          ,tstatus=False
                          ,tcode=""
                          ,tdescription=desc
                          ,tcomment=""
                          ,tpostings=[
                            Posting False acct1 amt1 "" RegularPosting Nothing
                           ,Posting False acct2 amt2 "" RegularPosting Nothing
                           ]
                          ,tpreceding_comment_lines=""
                          })
  -- display errors or add transaction
  case tE of
   Left errs -> do
    -- save current form values in session
    setMessage $ string $ intercalate "; " errs
    redirect RedirectTemporary JournalPage

   Right t -> do
    let t' = txnTieKnot t -- XXX move into balanceTransaction
    j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
    liftIO $ journalAddTransaction j opts t'
    setMessage $ string $ printf "Added transaction:\n%s" (show t')
    redirect RedirectTemporary JournalPage

-- | Handle a journal edit form post.
postEditForm :: Handler HledgerWebApp RepPlain
postEditForm = do
  -- get form input values, or basic validation errors. E means an Either value.
  textM  <- runFormPost' $ maybeStringInput "text"
  let textE = maybe (Left "No value provided") Right textM
  -- display errors or add transaction
  case textE of
   Left errs -> do
    -- XXX should save current form values in session
    setMessage $ string errs
    redirect RedirectTemporary JournalPage

   Right t' -> do
    -- try to avoid unnecessary backups or saving invalid data
    j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
    filechanged' <- liftIO $ journalFileIsNewer j
    let f = filepath j
        told = jtext j
        tnew = filter (/= '\r') t'
        changed = tnew /= told || filechanged'
--    changed <- liftIO $ writeFileWithBackupIfChanged f t''
    if not changed
     then do
       setMessage $ string $ "No change"
       redirect RedirectTemporary JournalPage
     else do
      jE <- liftIO $ journalFromPathAndString Nothing f tnew
      either
       (\e -> do
          setMessage $ string e
          redirect RedirectTemporary JournalPage)
       (const $ do
          liftIO $ writeFileWithBackup f tnew
          setMessage $ string $ printf "Saved journal %s\n" (show f)
          redirect RedirectTemporary JournalPage)
       jE

addform :: Hamlet HledgerWebAppRoute
addform = [$hamlet|
 %form#addform!method=POST!style=display:none;
  %table.form!cellpadding=0!cellspacing=0!border=0
   %tr
    %td!colspan=4
     %table!cellpadding=0!cellspacing=0!border=0
      %tr#descriptionrow
       %td
        Date:
       %td
        %input!size=15!name=date!value=$date$
       %td
        Description:
       %td
        %input!size=35!name=description!value=$desc$
      %tr.helprow
       %td
       %td
        .help $datehelp$ ^datehelplink^ $
       %td
       %td
        .help $deschelp$
   ^transactionfields1^
   ^transactionfields2^
   %tr#addbuttonrow
    %td!colspan=4
     %input!type=hidden!name=add!value=1
     %input!type=submit!name=submit!value="add transaction"
|]
 where
  datehelplink = helplink "dates" "..."
  datehelp = "eg: 7/20, 2010/1/1, "
  deschelp = "eg: supermarket (optional)"
  date = "today"
  desc = ""
  transactionfields1 = transactionfields 1
  transactionfields2 = transactionfields 2

transactionfields :: Int -> Hamlet HledgerWebAppRoute
transactionfields n = [$hamlet|
 %tr#postingrow
  %td!align=right
   $label$:
  %td
   %input!size=35!name=$acctvar$!value=$acct$
  ^amtfield^
 %tr.helprow
  %td
  %td
   .help $accthelp$
  %td
  %td
   .help $amthelp$
|]
 where
  label | n == 1    = "To account"
        | otherwise = "From account"
  accthelp | n == 1    = "eg: expenses:food"
           | otherwise = "eg: assets:bank:checking"
  amtfield | n == 1 = [$hamlet|
                       %td
                        Amount:
                       %td
                        %input!size=15!name=$amtvar$!value=$amt$
                       |]
           | otherwise = nulltemplate
  amthelp | n == 1    = "eg: 5, $6, €7.01"
          | otherwise = ""
  acct = ""
  amt = ""
  numbered = (++ show n)
  acctvar = numbered "accountname"
  amtvar = numbered "amount"

editform :: TemplateData -> String -> Hamlet HledgerWebAppRoute
editform _ content = [$hamlet|
 %form#editform!method=POST!style=display:none;
  %table.form#editform!cellpadding=0!cellspacing=0!border=0
   %tr
    %td!colspan=2
     %textarea!name=text!rows=30!cols=80
      $content$
   %tr#addbuttonrow
    %td
     %span.help ^formathelp^
    %td!align=right
     %span.help Are you sure ? Your journal will be overwritten. $
     %input!type=hidden!name=edit!value=1
     %input!type=submit!name=submit!value="save journal"
     \ or $
     %a!href!onclick="return editformToggle()" cancel
|]
  where
    formathelp = helplink "file-format" "file format help"

----------------------------------------------------------------------

-- | A combined accounts and postings view, like hledger balance + hledger register.
getLedgerPage :: Handler HledgerWebApp RepHtml
getLedgerPage = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  -- in this view, balance report is filtered only by period, not account/description filters
  app <- getYesod
  t <- liftIO $ getCurrentLocalTime
  let args = appArgs app
      fspec' = optsToFilterSpec opts args t
      br = balanceReportAsHtml opts td $ balanceReport opts fspec' j
      rr = registerReportAsHtml opts td $ registerReport opts fspec j
      td = mktd{here=here, title="hledger", msg=msg, a=a, p=p}
  hamletToRepHtml $ pageLayout td [$hamlet|
%div.ledger
 %div.accounts!style=float:left;  ^br^
 %div.register ^rr^
|]

----------------------------------------------------------------------

-- | An accounts and balances view, like hledger balance.
getBalancePage :: Handler HledgerWebApp RepHtml
getBalancePage = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  let td = mktd{here=here, title="hledger", msg=msg, a=a, p=p}
  hamletToRepHtml $ pageLayout td $ balanceReportAsHtml opts td $ balanceReport opts fspec j

-- | Render a balance report as HTML.
balanceReportAsHtml :: [Opt] -> TemplateData -> BalanceReport -> Hamlet HledgerWebAppRoute
balanceReportAsHtml _ td (items,total) = [$hamlet|
%table.balancereport
 $forall items i
  ^itemAsHtml' i^
 %tr.totalrule
  %td!colspan=2
 %tr
  %td
  %td!align=right $mixedAmountAsHtml.total$
|]
 where
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> BalanceReportItem -> Hamlet HledgerWebAppRoute
   itemAsHtml TD{a=a,p=p} (acct, adisplay, adepth, abal) = [$hamlet|
     %tr.item.$current$
      %td.account
       $indent$
       %a!href=$aurl$ $adisplay$
      %td.balance!align=right $mixedAmountAsHtml.abal$
     |] where
       current = if not (null a) && containsRegex a acct then "current" else ""
       indent = preEscapedString $ concat $ replicate (2 * adepth) "&nbsp;"
       aurl = printf "../ledger?a=^%s%s" acct p' :: String
       p' = if null p then "" else printf "&p=%s" p

----------------------------------------------------------------------

-- | A postings view, like hledger register.
getRegisterPage :: Handler HledgerWebApp RepHtml
getRegisterPage = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  let td = mktd{here=here, title="hledger", msg=msg, a=a, p=p}
  hamletToRepHtml $ pageLayout td $ registerReportAsHtml opts td $ registerReport opts fspec j

-- | Render a register report as HTML.
registerReportAsHtml :: [Opt] -> TemplateData -> RegisterReport -> Hamlet HledgerWebAppRoute
registerReportAsHtml _ td items = [$hamlet|
%table.registerreport
 $forall number.items i
  ^itemAsHtml' i^
|]
 where
   number = zip [1..]
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> (Int, RegisterReportItem) -> Hamlet HledgerWebAppRoute
   itemAsHtml TD{p=p} (n, (ds, posting, b)) = [$hamlet|
     %tr.item.$evenodd$.$firstposting$
      %td.date $date$
      %td.description $desc$
      %td.account
       %a!href=$aurl$ $acct$
      %td.amount!align=right $mixedAmountAsHtml.pamount.posting$
      %td.balance!align=right $mixedAmountAsHtml.b$
     |] where
       evenodd = if even n then "even" else "odd"
       (firstposting, date, desc) = case ds of Just (da, de) -> ("firstposting", show da, de)
                                               Nothing -> ("", "", "")
       acct = paccount posting
       aurl = printf "../ledger?a=^%s%s" acct p' :: String
       p' = if null p then "" else printf "&p=%s" p

--mixedAmountAsHtml = intercalate ", " . lines . show
mixedAmountAsHtml = preEscapedString . intercalate "<br>" . lines . show

----------------------------------------------------------------------

-- | A standalone journal edit form page.
getEditPage :: Handler HledgerWebApp RepHtml
getEditPage = do
  (a, p, _, _, _, msg, here) <- getHandlerParameters
  -- reload journal's text without parsing, if changed
  j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
  changed <- liftIO $ journalFileIsNewer j
  s <- liftIO $ if changed then readFile (filepath j) else return (jtext j) -- XXX readFile may throw an error
  let td = mktd{here=here, title="hledger", msg=msg, a=a, p=p}
  hamletToRepHtml $ pageLayout td $ editform td s

----------------------------------------------------------------------

-- | Gather all the stuff we want for a typical hledger web request handler.
getHandlerParameters :: Handler HledgerWebApp
                       (String, String, [Opt], FilterSpec, Journal, Maybe (Html ()), HledgerWebAppRoute)
getHandlerParameters = do
  Just here <- getCurrentRoute
  (a, p, opts, fspec) <- getReportParameters
  (j, err) <- getLatestJournal opts
  msg <- getMessage' err
  return (a, p, opts, fspec, j, msg, here)
    where
      -- | Get current report parameters for this request.
      getReportParameters :: Handler HledgerWebApp (String, String, [Opt], FilterSpec)
      getReportParameters = do
          app <- getYesod
          t <- liftIO $ getCurrentLocalTime
          a <- fromMaybe "" <$> lookupGetParam "a"
          p <- fromMaybe "" <$> lookupGetParam "p"
          let opts = appOpts app ++ [Period p]
              args = appArgs app ++ [a]
              fspec = optsToFilterSpec opts args t
          return (a, p, opts, fspec)

      -- | Update our copy of the journal if the file changed. If there is an
      -- error while reloading, keep the old one and return the error, and set a
      -- ui message.
      getLatestJournal :: [Opt] -> Handler HledgerWebApp (Journal, Maybe String)
      getLatestJournal opts = do
        j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
        (jE, changed) <- liftIO $ journalReloadIfChanged opts j
        if not changed
         then return (j,Nothing)
         else case jE of
                Right j' -> do liftIO $ putValue "hledger" "journal" j'
                               return (j',Nothing)
                Left e  -> do setMessage $ string "error while reading" {- ++ ": " ++ e-}
                              return (j, Just e)

      -- | Helper to work around a yesod feature (can't set and get a message in the same request.)
      getMessage' :: Maybe String -> Handler HledgerWebApp (Maybe (Html ()))
      getMessage' newmsgstr = do
        oldmsg <- getMessage
        return $ maybe oldmsg (Just . string) newmsgstr

pageLayout :: TemplateData -> Hamlet HledgerWebAppRoute -> Hamlet HledgerWebAppRoute
pageLayout td@TD{title=title, msg=msg} content = [$hamlet|
!!!
%html
 %head
  %title $title$
  %meta!http-equiv=Content-Type!content=$metacontent$
  %link!rel=stylesheet!type=text/css!href=@StyleCss@!media=all
 %body
  ^navbar.td^
  #messages $m$
  #content
   ^content^
|]
 where m = fromMaybe (string "") msg
       metacontent = "text/html; charset=utf-8"

navbar :: TemplateData -> Hamlet HledgerWebAppRoute
navbar td = [$hamlet|
 #navbar
  %a.toprightlink!href=$hledgerurl$ hledger $version$
  \ $
  %a.toprightlink!href=$manualurl$ manual
  \ $
  ^navlinks.td^
  ^filterform.td^
|]

navlinks :: TemplateData -> Hamlet HledgerWebAppRoute
navlinks td = [$hamlet|
 #navlinks
  ^journallink^ $
  | ^ledgerlink^ $
|]
 where
  journallink  = navlink td "journal" JournalPage
  ledgerlink   = navlink td "ledger" LedgerPage
  -- | ^balancelink^ $
  -- | ^registerlink^ $
  -- balancelink  = navlink td "balance" BalancePage
  -- registerlink = navlink td "register" RegisterPage

navlink :: TemplateData -> String -> HledgerWebAppRoute -> Hamlet HledgerWebAppRoute
navlink TD{here=here,a=a,p=p} s dest = [$hamlet|%a.$style$!href=@?u@ $s$|]
 where u = (dest, concat [(if null a then [] else [("a", a)])
                         ,(if null p then [] else [("p", p)])])
       style | dest == here = "navlinkcurrent"
             | otherwise    = "navlink"

filterform :: TemplateData -> Hamlet HledgerWebAppRoute
filterform TD{here=here,a=a,p=p} = [$hamlet|
 %form#filterform.$filtering$!method=GET
  %span!style=white-space:nowrap;
   ^filterformlabel^ $
   %input!name=a!size=30!value=$a$
   ^ahelp^ $
   in period: $
   %input!name=p!size=30!value=$p$
   ^phelp^ $
   %input!type=submit!value=filter
|]
 where
  ahelp = helplink "filter-patterns" "?"
  phelp = helplink "period-expressions" "?"
  (filtering, filterformlabel)
   | null a && null p = ("", [$hamlet|filter by: $|])
   | otherwise        = ("filtering", [$hamlet|
%a#stopfilterlink!href=@here@ stop filtering
\ $
by $
|])

helplink :: String -> String -> Hamlet HledgerWebAppRoute
helplink topic label = [$hamlet|%a!href=$u$!target=hledgerhelp $label$|]
    where u = manualurl ++ if null topic then "" else '#':topic

nulltemplate = [$hamlet||]

