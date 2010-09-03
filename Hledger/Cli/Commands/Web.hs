{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-| 
A web-based UI.
-}

module Hledger.Cli.Commands.Web
where
import Control.Concurrent (forkIO, threadDelay)
import Control.Applicative ((<$>), (<*>))
import Control.Failure
import Data.Either
import System.FilePath ((</>), takeFileName)
import System.IO.Storage (withStore, putValue, getValue)
import Text.ParserCombinators.Parsec (parse)
import Yesod
import Yesod.Helpers.Static
import Text.Hamlet
import Text.Hamlet.RT

import Hledger.Cli.Commands.Add (journalAddTransaction)
import Hledger.Cli.Commands.Balance
import Hledger.Cli.Commands.Print
import Hledger.Cli.Commands.Register
import Hledger.Cli.Options hiding (value)
import Hledger.Cli.Utils
import Hledger.Cli.Version (version)
import Hledger.Data hiding (today)
import Hledger.Read (journalFromPathAndString)
import Hledger.Read.Journal (someamount)
#ifdef MAKE
import Paths_hledger_make (getDataFileName)
#else
import Paths_hledger (getDataFileName)
#endif


defhost           = "localhost"
defport           = 5000
browserstartdelay = 100000 -- microseconds
hledgerorgurl     = "http://hledger.org"
manualurl         = hledgerorgurl++"/MANUAL.html"

data HledgerWebApp = HledgerWebApp {
      appRoot    :: String
     ,appDir     :: FilePath
     ,appOpts    :: [Opt]
     ,appArgs    :: [String]
     ,appJournal :: Journal
     ,appStatic  :: Static
     }

type Handler = GHandler HledgerWebApp HledgerWebApp

mkYesod "HledgerWebApp" [$parseRoutes|
/static          StaticR           Static appStatic
/                IndexR            GET
/journalonly     JournalOnlyR      GET POST
/registeronly    RegisterOnlyR     GET
/accounts        AccountsOnlyR     GET
/journal         JournalR          GET POST
/register        RegisterR         GET POST
/addformrt       AddformRTR        GET
|]

style_css       = StaticRoute ["style.css"] []
hledger_js      = StaticRoute ["hledger.js"] []
jquery_js       = StaticRoute ["jquery.js"] []
jquery_url_js   = StaticRoute ["jquery.url.js"] []
dhtmlxcommon_js = StaticRoute ["dhtmlxcommon.js"] []
dhtmlxcombo_js  = StaticRoute ["dhtmlxcombo.js"] []

instance Yesod HledgerWebApp where approot = appRoot

defaultroute = JournalR

-- | A bundle of useful data passed to templates.
data TemplateData = TD {
     here         :: HledgerWebAppRoute -- ^ the current page's route
    ,title        :: String             -- ^ page's title
    ,msg          :: Maybe Html         -- ^ transient message
    ,a            :: String             -- ^ a (acct/desc filter pattern) parameter
    ,p            :: String             -- ^ p (period expression) parameter
    ,j            :: Journal            -- ^ the current journal
    ,today        :: Day                -- ^ the current day
    }

mktd = TD {
      here = IndexR
     ,title = "hledger"
     ,msg = Nothing
     ,a = ""
     ,p = ""
     ,j = nulljournal
     ,today = ModifiedJulianDay 0
     }

-- | The web command.
web :: [Opt] -> [String] -> Journal -> IO ()
web opts args j = do
  let host    = defhost
      port    = fromMaybe defport $ portFromOpts opts
      baseurl = fromMaybe (printf "http://%s:%d" host port) $ baseUrlFromOpts opts
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
    dir <- getDataFileName "web"
    let app = HledgerWebApp{
               appRoot=baseurl
              ,appDir=dir
              ,appStatic=fileLookupDir (dir </> "static") $ typeByExt -- ++[("hamlet","text/plain")]
              ,appOpts=opts
              ,appArgs=args
              ,appJournal=j
              }
    withStore "hledger" $ do
     putValue "hledger" "journal" j
     basicHandler' port Nothing app

-- | Gather all the stuff we want for a typical hledger web request handler.
getHandlerParameters :: Handler
                       (String, String, [Opt], FilterSpec, Journal, Maybe Html, HledgerWebAppRoute)
getHandlerParameters = do
  Just here <- getCurrentRoute
  (a, p, opts, fspec) <- getReportParameters
  (j, err) <- getLatestJournal opts
  msg <- getMessage' err
  return (a, p, opts, fspec, j, msg, here)
    where
      -- | Get current report parameters for this request.
      getReportParameters :: Handler (String, String, [Opt], FilterSpec)
      getReportParameters = do
          app <- getYesod
          t <- liftIO $ getCurrentLocalTime
          a <- fromMaybe "" <$> lookupGetParam "x"
          p <- fromMaybe "" <$> lookupGetParam "p"
          let opts = appOpts app ++ [Period p]
              args = appArgs app ++ [a]
              fspec = optsToFilterSpec opts args t
          return (a, p, opts, fspec)

      -- | Update our copy of the journal if the file changed. If there is an
      -- error while reloading, keep the old one and return the error, and set a
      -- ui message.
      getLatestJournal :: [Opt] -> Handler (Journal, Maybe String)
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
      getMessage' :: Maybe String -> Handler (Maybe Html)
      getMessage' newmsgstr = do
        oldmsg <- getMessage
        return $ maybe oldmsg (Just . string) newmsgstr

----------------------------------------------------------------------
-- handlers & templates

getIndexR :: Handler ()
getIndexR = redirect RedirectTemporary defaultroute

----------------------------------------------------------------------

-- | A combined accounts and journal view.
getJournalR :: Handler RepHtml
getJournalR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  today <- liftIO getCurrentDay
  -- app <- getYesod
  -- t <- liftIO $ getCurrentLocalTime
  let -- args = appArgs app
      -- fspec' = optsToFilterSpec opts args t
      br = balanceReportAsHtml opts td $ balanceReport opts fspec j
      jr = journalReportAsHtml opts td $ journalReport opts fspec j
      td = mktd{here=here, title="hledger", msg=msg, a=a, p=p, j=j, today=today}
      editform' = editform td $ jtext j
  hamletToRepHtml $ pageLayout td [$hamlet|
%div.ledger
 %div.accounts!style=float:left;  ^br^
 ^navlinks.td^
 ^addform.td^
 ^editform'^
 ^importform^
 %div#transactions.journal
  ^filterform.td^
  ^jr^
|]

postJournalR :: Handler RepPlain
postJournalR = postJournalOnlyR

----------------------------------------------------------------------

-- | A combined accounts and register view.
getRegisterR :: Handler RepHtml
getRegisterR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  today <- liftIO getCurrentDay
  -- app <- getYesod
  -- t <- liftIO $ getCurrentLocalTime
  let -- args = appArgs app
      -- opts' = Empty:opts
      -- fspec' = optsToFilterSpec opts' args t
      br = balanceReportAsHtml opts td $ balanceReport opts fspec j
      rr = registerReportAsHtml opts td $ registerReport opts fspec j
      td = mktd{here=here, title="hledger", msg=msg, a=a, p=p, j=j, today=today}
      editform' = editform td $ jtext j
  hamletToRepHtml $ pageLayout td [$hamlet|
%div.ledger
 %div.accounts!style=float:left;  ^br^
 ^navlinks.td^
 ^addform.td^
 ^editform'^
 ^importform^
 %div#transactions.register
  ^filterform.td^
  ^rr^
|]

postRegisterR :: Handler RepPlain
postRegisterR = postJournalOnlyR

----------------------------------------------------------------------

-- | A simple accounts and balances view like hledger balance.
getAccountsOnlyR :: Handler RepHtml
getAccountsOnlyR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger", msg=msg, a=a, p=p, j=j, today=today}
  hamletToRepHtml $ pageLayout td $ balanceReportAsHtml opts td $ balanceReport opts fspec j

-- | Render a balance report as HTML.
balanceReportAsHtml :: [Opt] -> TemplateData -> BalanceReport -> Hamlet HledgerWebAppRoute
balanceReportAsHtml _ td@TD{here=here,a=a,p=p} (items,total) = [$hamlet|
%table.balancereport
 ^accountsheading^
 $forall items i
  ^itemAsHtml' i^
 %tr.totalrule
  %td!colspan=2
 %tr
  %td
  %td!align=right $mixedAmountAsHtml.total$
|]
 where
   accountsheading = [$hamlet|
                      accounts
                      \ $
                      %span#showmoreaccounts ^showmore^ ^showall^
                      <br />
                      <br />
                      |]
       where
         filteringaccts = not $ null a
         showmore = case (filteringaccts, items) of
                      -- cunning parent account logic
                      (True, ((acct, _, _, _):_)) ->
                          let a' = if isAccountRegex a then a else acct
                              a'' = accountNameToAccountRegex $ parentAccountName $ accountRegexToAccountName a'
                              parenturl = (here, [("y",a''), ("p",p)])
                          in [$hamlet|
                              \ | $
                              %a!href=@?parenturl@ show more &uarr;
                              |]
                      _ -> nulltemplate
         showall = if filteringaccts
                    then [$hamlet|
                          \ | $
                          %a!href=@?allurl@ show all
                          |]
                    else nulltemplate
             where allurl = (here, [("p",p)])
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> BalanceReportItem -> Hamlet HledgerWebAppRoute
   itemAsHtml TD{p=p} (acct, adisplay, adepth, abal) = [$hamlet|
     %tr.item.$current$
      %td.account
       $indent$
       %a!href=$aurl$ $adisplay$
      %td.balance!align=right $mixedAmountAsHtml.abal$
     |] where
       current = "" -- if not (null a) && containsRegex a acct then "current" else ""
       indent = preEscapedString $ concat $ replicate (2 * adepth) "&nbsp;"
       aurl = printf ".?a=%s%s" (accountNameToAccountRegex acct) p' :: String
       p' = if null p then "" else printf "&p=%s" p

accountNameToAccountRegex :: String -> String
accountNameToAccountRegex "" = ""
accountNameToAccountRegex a = printf "^%s(:|$)" a

accountRegexToAccountName :: String -> String
accountRegexToAccountName = gsubRegexPR "^\\^(.*?)\\(:\\|\\$\\)$" "\\1"

isAccountRegex  :: String -> Bool
isAccountRegex s = take 1 s == "^" && (take 5 $ reverse s) == ")$|:("

----------------------------------------------------------------------

-- | A basic journal view, like hledger print, with editing.
getJournalOnlyR :: Handler RepHtml
getJournalOnlyR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger", msg=msg, a=a, p=p, j=j, today=today}
      editform' = editform td $ jtext j
      txns = journalReportAsHtml opts td $ journalReport opts fspec j
  hamletToRepHtml $ pageLayout td [$hamlet|
%div.journal
 %div.nav2
  %a#addformlink!href!onclick="return addformToggle()" add one transaction
  \ | $
  %a#editformlink!href!onclick="return editformToggle()" edit the whole journal
 ^addform.td^
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

addform :: TemplateData -> Hamlet HledgerWebAppRoute
addform td = [$hamlet|
%script!type=text/javascript
 $$(document).ready(function() {
    /* dhtmlxcombo setup */
    window.dhx_globalImgPath="../static/images/";
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
%form#addform!method=POST!style=display:none;
  %table.form
   %tr
    %td!colspan=4
     %table
      %tr#descriptionrow
       %td
        Date:
       %td
        %input.textinput!size=15!name=date!value=$date$
       %td!style=padding-left:1em;
        Description:
       %td
        %select!id=description!name=description
         %option
         $forall descriptions d
          %option!value=$d$ $d$
      %tr.helprow
       %td
       %td
        .help $datehelp$ $
       %td
       %td
        .help $deschelp$
   ^postingsfields.td^
   %tr#addbuttonrow
    %td!colspan=4
     %input!type=hidden!name=action!value=add
     %input!type=submit!name=submit!value="add transaction"
|]
 where
  -- datehelplink = helplink "dates" "..."
  datehelp = "eg: 2010/7/20"
  deschelp = "eg: supermarket (optional)"
  date = "today"
  descriptions = sort $ nub $ map tdescription $ jtxns $ j td

postingsfields :: TemplateData -> Hamlet HledgerWebAppRoute
postingsfields td = [$hamlet|
 ^p1^
 ^p2^
|]
  where
    p1 = postingfields td 1
    p2 = postingfields td 2

postingfields :: TemplateData -> Int -> Hamlet HledgerWebAppRoute
postingfields td n = [$hamlet|
 %tr#postingrow
  %td!align=right $acctlabel$:
  %td
   %select!id=$acctvar$!name=$acctvar$
    %option
    $forall acctnames a
     %option!value=$a$ $a$
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
  numbered = (++ show n)
  acctvar = numbered "account"
  amtvar = numbered "amount"
  acctnames = sort $ journalAccountNamesUsed $ j td
  (acctlabel, accthelp, amtfield, amthelp)
       | n == 1     = ("To account"
                     ,"eg: expenses:food"
                     ,[$hamlet|
                       %td!style=padding-left:1em;
                        Amount:
                       %td
                        %input.textinput!size=15!name=$amtvar$!value=""
                       |]
                     ,"eg: $6"
                     )
       | otherwise = ("From account"
                     ,"eg: assets:bank:checking"
                     ,nulltemplate
                     ,""
                     )

editform :: TemplateData -> String -> Hamlet HledgerWebAppRoute
editform _ content = [$hamlet|
 %form#editform!method=POST!style=display:none;
  %table.form#editform
   %tr
    %td!colspan=2
     %textarea!name=text!rows=30!cols=80
      $content$
   %tr#addbuttonrow
    %td
     %span.help ^formathelp^
    %td!align=right
     %span.help Are you sure ? This will overwrite the journal. $
     %input!type=hidden!name=action!value=edit
     %input!type=submit!name=submit!value="save journal"
     \ or $
     %a!href!onclick="return editformToggle()" cancel
|]
  where
    formathelp = helplink "file-format" "file format help"

importform :: Hamlet HledgerWebAppRoute
importform = [$hamlet|
 %form#importform!method=POST!style=display:none;
  %table.form
   %tr
    %td
     %input!type=file!name=file
     %input!type=hidden!name=action!value=import
     %input!type=submit!name=submit!value="import from file"
     \ or $
     %a!href!onclick="return importformToggle()" cancel
|]

postJournalOnlyR :: Handler RepPlain
postJournalOnlyR = do
  action <- runFormPost' $ maybeStringInput "action"
  case action of Just "edit"   -> postEditForm
                 Just "import" -> postImportForm
                 _             -> postAddForm

-- | Handle a journal add form post.
postAddForm :: Handler RepPlain
postAddForm = do
  (_, _, opts, _, _, _, _) <- getHandlerParameters
  today <- liftIO getCurrentDay
  -- get form input values. M means a Maybe value.
  (dateM, descM, acct1M, amt1M, acct2M, amt2M) <- runFormPost'
    $ (,,,,,)
    <$> maybeStringInput "date"
    <*> maybeStringInput "description"
    <*> maybeStringInput "account1"
    <*> maybeStringInput "amount1"
    <*> maybeStringInput "account2"
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
    redirect RedirectTemporary RegisterR

   Right t -> do
    let t' = txnTieKnot t -- XXX move into balanceTransaction
    j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
    liftIO $ journalAddTransaction j opts t'
    setMessage $ string $ printf "Added transaction:\n%s" (show t')
    redirect RedirectTemporary RegisterR

-- | Handle a journal edit form post.
postEditForm :: Handler RepPlain
postEditForm = do
  -- get form input values, or basic validation errors. E means an Either value.
  textM  <- runFormPost' $ maybeStringInput "text"
  let textE = maybe (Left "No value provided") Right textM
  -- display errors or add transaction
  case textE of
   Left errs -> do
    -- XXX should save current form values in session
    setMessage $ string errs
    redirect RedirectTemporary JournalR

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
       redirect RedirectTemporary JournalR
     else do
      jE <- liftIO $ journalFromPathAndString Nothing f tnew
      either
       (\e -> do
          setMessage $ string e
          redirect RedirectTemporary JournalR)
       (const $ do
          liftIO $ writeFileWithBackup f tnew
          setMessage $ string $ printf "Saved journal %s\n" (show f)
          redirect RedirectTemporary JournalR)
       jE

-- | Handle an import page post.
postImportForm :: Handler RepPlain
postImportForm = do
  setMessage $ string $ "can't handle file upload yet"
  redirect RedirectTemporary JournalR
  -- -- get form input values, or basic validation errors. E means an Either value.
  -- fileM <- runFormPost' $ maybeFileInput "file"
  -- let fileE = maybe (Left "No file provided") Right fileM
  -- -- display errors or import transactions
  -- case fileE of
  --  Left errs -> do
  --   setMessage $ string errs
  --   redirect RedirectTemporary JournalR

  --  Right s -> do
  --    setMessage $ string $ s
  --    redirect RedirectTemporary JournalR

----------------------------------------------------------------------

-- | A simple postings view like hledger register.
getRegisterOnlyR :: Handler RepHtml
getRegisterOnlyR = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger", msg=msg, a=a, p=p, j=j, today=today}
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
       aurl = printf ".?a=%s%s" (accountNameToAccountRegex acct) p' :: String
       p' = if null p then "" else printf "&p=%s" p

mixedAmountAsHtml b = preEscapedString $ addclass $ intercalate "<br>" $ lines $ show b
    where addclass = printf "<span class=\"%s\">%s</span>" c
          c = case isNegativeMixedAmount b of Just True -> "negative amount"
                                              _         -> "positive amount"

----------------------------------------------------------------------

-- | A standalone journal edit form page.
getEditR :: Handler RepHtml
getEditR = do
  (a, p, _, _, _, msg, here) <- getHandlerParameters
  today <- liftIO getCurrentDay
  -- reload journal's text without parsing, if changed     -- XXX are we doing this right ?
  j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
  changed <- liftIO $ journalFileIsNewer j
  s <- liftIO $ if changed then readFile (filepath j) else return (jtext j) -- XXX readFile may throw an error
  let td = mktd{here=here, title="hledger", msg=msg, a=a, p=p, j=j, today=today}
  hamletToRepHtml $ pageLayout td $ editform td s

----------------------------------------------------------------------

-- | Get the add form from template files reloaded at run-time.
getAddformRTR :: Handler RepHtml
getAddformRTR = do
  (a, p, _, _, j, msg, here) <- getHandlerParameters
  today <- liftIO getCurrentDay
  let td = mktd{here=here, title="hledger add transaction", msg=msg, a=a, p=p, j=j, today=today}
      descriptions = sort $ nub $ map tdescription $ jtxns j
      acctnames = sort $ journalAccountNamesUsed j
      postingData n = [
                       (["acctlabel"], hdstring acctlabel)
                      ,(["acctvar"],   hdstring acctvar)
                      ,(["acctnames"], hdstringlist acctnames)
                      ,(["amtfield"],  HDHtml $ renderHamlet' amtfield)
                      ,(["accthelp"],  hdstring accthelp)
                      ,(["amthelp"],   hdstring amthelp)
                      ] :: HamletMap HledgerWebAppRoute
          where
            numbered = (++ show n)
            acctvar = numbered "account"
            amtvar = numbered "amount"
            (acctlabel, accthelp, amtfield, amthelp)
                | n == 1     = ("To account"
                              ,"eg: expenses:food"
                              ,[$hamlet|
                                %td!style=padding-left:1em;
                                 Amount:
                                %td
                                 %input.textinput!size=15!name=$amtvar$!value=""
                                |]
                              ,"eg: $6"
                              )
                | otherwise = ("From account"
                              ,"eg: assets:bank:checking"
                              ,nulltemplate
                              ,""
                              )
  pfields1 <- renderHamletFile "addformpostingfields.hamlet" (postingData 1)
  pfields2 <- renderHamletFile "addformpostingfields.hamlet" (postingData 2)
  addform  <- renderHamletFile "addform.hamlet" ([
                                                 (["date"], hdstring "today")
                                                ,(["desc"], hdstring "")
                                                ,(["descriptions"], hdstringlist descriptions)
                                                ,(["datehelp"], hdstring "eg: 2010/7/20")
                                                ,(["deschelp"], hdstring "eg: supermarket (optional)")
                                                ,(["postingfields1"], HDHtml pfields1)
                                                ,(["postingfields2"], HDHtml pfields2)
                                                ] :: HamletMap HledgerWebAppRoute)
  hamletToRepHtml $ pageLayout td $ htmlAsHamlet addform

-- | Convert a string to a hamlet HDHtml data item.
hdstring :: String -> HamletData HledgerWebAppRoute
hdstring = HDHtml . string

-- | Convert a simple list of strings to hamlet's complicated HDList type.
hdstringlist :: [String] -> HamletData HledgerWebAppRoute
hdstringlist ss = HDList [ [([], hdstring s)] | s <- ss ]

instance Failure HamletException Handler
    where failure = error . show

renderHamletFile :: FilePath -> HamletMap HledgerWebAppRoute -> Handler Html
renderHamletFile hfile hmap = do
  hrt <- readHamletFile hfile >>= parseHamletRT defaultHamletSettings
  renderHamletRT hrt hmap renderurlwithparams

renderurlwithparams u [] = show u
renderurlwithparams u ps = show u ++ "?" ++ intercalate "&" [k++"="++v | (k,v) <- ps]

readHamletFile :: FilePath -> Handler String
readHamletFile hfile = do
  dir <- ((</> "templates") . appDir) `fmap` getYesod
  liftIO $ readFile $ dir </> hfile

htmlAsHamlet :: Html -> Hamlet HledgerWebAppRoute
htmlAsHamlet h = [$hamlet|$h$|]

parseHamletRT' :: Failure HamletException m => String -> m HamletRT
parseHamletRT' s = parseHamletRT defaultHamletSettings s

renderHamletRT' :: Failure HamletException m => HamletMap HledgerWebAppRoute -> HamletRT -> m Html
renderHamletRT' m h = renderHamletRT h m renderurlwithparams

renderHamlet' :: Hamlet HledgerWebAppRoute -> Html
renderHamlet' h = h renderurlwithparams

-- hamletToHamletRT ::  Failure HamletException m => Hamlet HledgerWebAppRoute -> m HamletRT
-- hamletToHamletRT h = stringToHamletRT $ show $ unsafeByteString $ renderHamlet show h

----------------------------------------------------------------------

-- | Wrap a template with the standard hledger web ui page layout.
pageLayout :: TemplateData -> Hamlet HledgerWebAppRoute -> Hamlet HledgerWebAppRoute
pageLayout td@TD{title=title, msg=msg} content = [$hamlet|
!!!
%html
 %head
  %title $title$
  %meta!http-equiv=Content-Type!content=$metacontent$
  %script!type=text/javascript!src=@StaticR.jquery_js@
  %script!type=text/javascript!src=@StaticR.jquery_url_js@
  %script!type=text/javascript!src=@StaticR.dhtmlxcommon_js@
  %script!type=text/javascript!src=@StaticR.dhtmlxcombo_js@
  %script!type=text/javascript!src=@StaticR.hledger_js@
  %link!rel=stylesheet!type=text/css!media=all!href=@StaticR.style_css@
 %body
  ^navbar.td^
  #messages $m$
  #content
   ^content^
|]
 where m = fromMaybe (string "") msg
       metacontent = "text/html; charset=utf-8"

navbar :: TemplateData -> Hamlet HledgerWebAppRoute
navbar TD{p=p,j=j,today=today} = [$hamlet|
 #navbar
  %a.topleftlink!href=$hledgerorgurl$
   hledger
   <br />
   $version$
  %a.toprightlink!href=$manualurl$!target=hledgerhelp manual
  %h1 $journaltitle$
  \ $
  %span#journalinfo $journalinfo$
|]
  where
    journaltitle = printf "%s" (takeFileName $ filepath j) :: String
    journalinfo  = printf "%s" (showspan span) :: String
    showspan (DateSpan Nothing Nothing) = ""
    showspan s = " (" ++ dateSpanAsText s ++ ")"
    span = either (const $ DateSpan Nothing Nothing) snd (parsePeriodExpr today p)

navlinks :: TemplateData -> Hamlet HledgerWebAppRoute
navlinks td = [$hamlet|
 #navlinks
  ^accountsjournallink^
  \ | $
  ^accountsregisterlink^
  \ | $
  %a#addformlink!href!onclick="return addformToggle()" add transaction
  %a#importformlink!href!onclick="return importformToggle()"!style=display:none; import transactions
  \ | $
  %a#editformlink!href!onclick="return editformToggle()" edit journal
|]
--  \ | $
 where
   accountsjournallink  = navlink td "journal" JournalR
   accountsregisterlink = navlink td "register" RegisterR

navlink :: TemplateData -> String -> HledgerWebAppRoute -> Hamlet HledgerWebAppRoute
navlink TD{here=here,a=a,p=p} s dest = [$hamlet|%a#$s$link.$style$!href=@?u@ $s$|]
 where u = (dest, concat [(if null a then [] else [("z", a)])
                         ,(if null p then [] else [("p", p)])])
       style | dest == here = "navlinkcurrent"
             | otherwise    = "navlink"

filterform :: TemplateData -> Hamlet HledgerWebAppRoute
filterform TD{here=here,a=a,p=p} = [$hamlet|
 #filterformdiv
  %form#filterform.form!method=GET!style=display:$visible$;
   %table.form
    %tr.$filteringperiodclass$
     %td
      filter by period:
      \ $
     %td
      %input!name=p!size=60!value=$p$
      ^phelp^
      \ $
     %td!align=right
      ^stopfilteringperiod^
    %tr.$filteringclass$
     %td
      filter by account/description:
      \ $
     %td
      %input!name=a!size=60!value=$a$
      ^ahelp^
      \ $
      %input!type=submit!value=filter $
      \ $
     %td!align=right
      ^stopfiltering^
|]
 where
  ahelp = helplink "filter-patterns" "?"
  phelp = helplink "period-expressions" "?"
  filtering = not $ null a
  filteringperiod = not $ null p
  visible = "block"
  filteringclass = if filtering then "filtering" else ""
  filteringperiodclass = if filteringperiod then "filtering" else ""
  stopfiltering = if filtering then [$hamlet|%a#stopfilterlink!href=@?u@ stop filtering acct/desc|] else nulltemplate
      where u = (here, if filteringperiod then [("p", p)] else [])
  stopfilteringperiod = if filteringperiod then [$hamlet|%a#stopfilterlink!href=@?u@ stop filtering period|] else nulltemplate
      where u = (here, if filtering then [("q", a)] else [])

helplink :: String -> String -> Hamlet HledgerWebAppRoute
helplink topic label = [$hamlet|%a!href=$u$!target=hledgerhelp $label$|]
    where u = manualurl ++ if null topic then "" else '#':topic

nulltemplate = [$hamlet||]

