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
/edit         EditPage         GET POST
/register     RegisterPage     GET
/balance      BalancePage      GET
/style.css    StyleCss         GET
|]

instance Yesod HledgerWebApp where approot = appRoot

-- | A bundle of useful data passed to templates.
data TemplateData = TD {
     here         :: HledgerWebAppRoute -- ^ the current page's route
    ,title        :: String             -- ^ page's title
    ,msg          :: Maybe (Html ())     -- ^ transient message
    ,a            :: String             -- ^ a (filter pattern) parameter
    ,p            :: String             -- ^ p (period expression) parameter
    ,content      :: Html ()             -- ^ html for the content area
    ,contentplain :: String             -- ^ or plain text content
    }

td = TD {
      here = IndexPage
     ,title = "hledger"
     ,msg = Nothing
     ,a = ""
     ,p = ""
     ,content = nulltemplate id
     ,contentplain = ""
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
     basicHandler port app

-- handlers

getStyleCss :: Handler HledgerWebApp ()
getStyleCss = do
    app <- getYesod
    let dir = appWebdir app
    sendFile "text/css" $ dir </> "style.css"

getIndexPage :: Handler HledgerWebApp ()
getIndexPage = redirect RedirectTemporary BalancePage

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

-- renderLatestJournalWith :: ([Opt] -> FilterSpec -> Journal -> Html ()) -> Handler HledgerWebApp RepHtml
-- renderLatestJournalWith reportHtml = do
--   (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
--   let td' = td{here=here, title="hledger", msg=msg, a=a, p=p, content=reportHtml opts fspec j}
--   hamletToRepHtml $ pageLayout td'

getJournalPage :: Handler HledgerWebApp RepHtml
getJournalPage = do
  (a, p, _, fspec, j, msg, here) <- getHandlerParameters
  let td' = td{here=here, title="hledger", msg=msg, a=a, p=p, content=
                     stringToPre $ showTransactions fspec j
              }
  hamletToRepHtml $ pageLayout td'

getBalancePage :: Handler HledgerWebApp RepHtml
getBalancePage = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  let td' = td{here=here, title="hledger", msg=msg, a=a, p=p, content=
                     balanceReportAsHtml opts td' $ balanceReport opts fspec j
              }
  hamletToRepHtml $ pageLayout td'

-- | Render a balance report as HTML.
balanceReportAsHtml :: [Opt] -> TemplateData -> BalanceReport -> Html ()
balanceReportAsHtml _ td (items,total) = [$hamlet|
%table.balancereport
 $forall items i
  %tr.itemrule
   %td!colspan=2
  ^itemAsHtml' i^
 %tr.totalrule
  %td!colspan=2
 %tr
  %td
  %td!align=right $mixedAmountAsHtml.total$
|] id
 where
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> BalanceReportItem -> Hamlet String
   itemAsHtml TD{p=p} (a, adisplay, adepth, abal) = [$hamlet|
     %tr.item
      %td.account
       $indent$
       %a!href=$aurl$ $adisplay$
      %td.balance!align=right $mixedAmountAsHtml.abal$
     |] where
       indent = preEscapedString $ concat $ replicate (2 * adepth) "&nbsp;"
       aurl = printf "../register?a=^%s%s" a p' :: String
       p' = if null p then "" else printf "&p=%s" p

--mixedAmountAsHtml = intercalate ", " . lines . show
mixedAmountAsHtml = preEscapedString . intercalate "<br>" . lines . show

getRegisterPage :: Handler HledgerWebApp RepHtml
getRegisterPage = do
  (a, p, opts, fspec, j, msg, here) <- getHandlerParameters
  let td' = td{here=here, title="hledger", msg=msg, a=a, p=p, content=
                     registerReportAsHtml opts td' $ registerReport opts fspec j
              }
  hamletToRepHtml $ pageLayout td'

-- | Render a register report as HTML.
registerReportAsHtml :: [Opt] -> TemplateData -> RegisterReport -> Html ()
registerReportAsHtml _ td items = [$hamlet|
%table.registerreport
 $forall items i
  %tr.itemrule
   %td!colspan=5
  ^itemAsHtml' i^
|] id
 where
   itemAsHtml' = itemAsHtml td
   itemAsHtml :: TemplateData -> RegisterReportItem -> Hamlet String
   itemAsHtml TD{p=p} (ds, posting, b) = [$hamlet|
     %tr.item
      %td.date $date$
      %td.description $desc$
      %td.account
       %a!href=$aurl$ $acct$
      %td.amount!align=right $mixedAmountAsHtml.pamount.posting$
      %td.balance!align=right $mixedAmountAsHtml.b$
     |] where
       (date, desc) = case ds of Just (da, de) -> (show da, de)
                                 Nothing -> ("", "")
       acct = paccount posting
       aurl = printf "../register?a=^%s%s" acct p' :: String
       p' = if null p then "" else printf "&p=%s" p

queryStringFromAP a p = if null ap then "" else "?" ++ ap
    where
      ap = intercalate "&" [a',p']
      a' = if null a then "" else printf "&a=%s" a
      p' = if null p then "" else printf "&p=%s" p

getEditPage :: Handler HledgerWebApp RepHtml
getEditPage = do
  (a, p, _, _, _, msg, here) <- getHandlerParameters
  -- reload journal's text without parsing, if changed
  j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
  changed <- liftIO $ journalFileIsNewer j
  s <- liftIO $ if changed then readFile (filepath j) else return (jtext j) -- XXX readFile may throw an error
  let td' = td{here=here, title="hledger", msg=msg, a=a, p=p, 
                     content=(editform td') show, contentplain=s} -- XXX provide both to squeeze editform into pageLayout
  hamletToRepHtml $ pageLayout td'

postJournalPage :: Handler HledgerWebApp RepPlain
postJournalPage = do
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
    liftIO $ journalAddTransaction j t'
    setMessage $ string $ printf "Added transaction:\n%s" (show t')
    redirect RedirectTemporary JournalPage

postEditPage :: Handler HledgerWebApp RepPlain
postEditPage = do
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
       redirect RedirectTemporary EditPage
     else do
      jE <- liftIO $ journalFromPathAndString Nothing f tnew
      either
       (\e -> do
          setMessage $ string e
          redirect RedirectTemporary EditPage)
       (const $ do
          liftIO $ writeFileWithBackup f tnew
          setMessage $ string $ printf "Saved journal %s\n" (show f)
          redirect RedirectTemporary JournalPage)
       jE

-- templates

nulltemplate = [$hamlet||]

stringToPre :: String -> Html ()
stringToPre s = [$hamlet|%pre $s$|] id

pageLayout :: TemplateData -> Hamlet HledgerWebAppRoute
pageLayout td@TD{here=here, title=title, msg=msg, content=content} = [$hamlet|
!!!
%html
 %head
  %title $title$
  %meta!http-equiv=Content-Type!content=$metacontent$
  %link!rel=stylesheet!type=text/css!href=@stylesheet@!media=all
 %body
  ^navbar.td^
  #messages $m$
  ^addform'.here^
  #content
   $content$
|]
 where m = fromMaybe (string "") msg
       addform' JournalPage = addform
       addform' _           = nulltemplate
       stylesheet = StyleCss
       metacontent = "text/html; charset=utf-8"

navbar :: TemplateData -> Hamlet HledgerWebAppRoute
navbar td = [$hamlet|
 #navbar
  %a.toprightlink!href=$hledgerurl$ hledger.org
  \ $
  %a.toprightlink!href=$manualurl$ manual
  \ $
  ^navlinks.td^
  ^searchform.td^
|]

navlinks :: TemplateData -> Hamlet HledgerWebAppRoute
navlinks TD{here=here,a=a,p=p} = [$hamlet|
 #navlinks
  ^journallink^ $
  (^editlink^) $
  | ^balancelink^ $
  | ^registerlink^ $
|]
 where
  journallink = navlink here "journal" JournalPage
  editlink = navlink here "edit" EditPage
  registerlink = navlink here "register" RegisterPage
  balancelink = navlink here "balance" BalancePage
  navlink here s dest = [$hamlet|%a.$style$!href=@?u@ $s$|]
   where u = (dest, concat [(if null a then [] else [("a", a)])
                           ,(if null p then [] else [("p", p)])])
         style | here == dest = "navlinkcurrent"
               | otherwise    = "navlink"

searchform :: TemplateData -> Hamlet HledgerWebAppRoute
searchform TD{here=here,a=a,p=p} = [$hamlet|
 %form#searchform!method=GET
  ^resetlink^ $
  %span!style=white-space:nowrap;
   filter by: $
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
  resetlink
   | null a && null p = nulltemplate
   | otherwise        = [$hamlet|%span#resetlink!style=font-weight:bold; $
                                  %a!href=@here@ stop filtering|]

helplink topic label = [$hamlet|%a!href=$u$ $label$|]
    where u = manualurl ++ if null topic then "" else '#':topic

editform :: TemplateData -> Hamlet HledgerWebAppRoute
editform TD{contentplain=t} = [$hamlet|
 %form!method=POST
  %table.form#editform!cellpadding=0!cellspacing=0!border=0
   %tr.formheading
    %td!colspan=2
     %span!style=float:right; ^formhelp^
     %span#formheading Edit journal:
   %tr
    %td!colspan=2
     %textarea!name=text!rows=30!cols=80
      $t$
   %tr#addbuttonrow
    %td
     %a!href=@JournalPage@ cancel
    %td!align=right
     %input!type=submit!value=$submitlabel$
   %tr.helprow
    %td
    %td!align=right
     #help Are you sure ? All previous data will be replaced
|]
 where
  submitlabel = "save journal"
  formhelp = helplink "file-format" "file format help"

addform :: Hamlet HledgerWebAppRoute
addform = [$hamlet|
 %form!method=POST
  %table.form#addform!cellpadding=0!cellspacing=0!border=0
   %tr.formheading
    %td!colspan=4
     %span#formheading Add a transaction:
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
        #help $datehelp$ ^datehelplink^ $
       %td
       %td
        #help $deschelp$
   ^transactionfields1^
   ^transactionfields2^
   %tr#addbuttonrow
    %td!colspan=4
     %input!type=submit!value=$addlabel$
|]
 where
  datehelplink = helplink "dates" "..."
  datehelp = "eg: 7/20, 2010/1/1, "
  deschelp = "eg: supermarket (optional)"
  addlabel = "add transaction"
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
   #help $accthelp$
  %td
  %td
   #help $amthelp$
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

