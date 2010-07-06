{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, TemplateHaskell #-}
{-| 
A web-based UI.
-}

module Hledger.Cli.Commands.Web
where
import Control.Concurrent (forkIO, threadDelay)
import Data.Either
import Network.Wai.Handler.SimpleServer (run)
import System.FilePath ((</>))
import System.IO.Storage (withStore, putValue, getValue)
import Text.Hamlet
import Text.ParserCombinators.Parsec (parse)
import Yesod

import Hledger.Cli.Commands.Add (journalAddTransaction)
import Hledger.Cli.Commands.Balance
import Hledger.Cli.Commands.Print
import Hledger.Cli.Commands.Register
import Hledger.Cli.Options hiding (value)
import Hledger.Cli.Utils
import Hledger.Data
import Hledger.Read.Journal (someamount)
#ifdef MAKE
import Paths_hledger_make (getDataFileName)
#else
import Paths_hledger (getDataFileName)
#endif


defhost = "localhost"
defport = 5000
browserstartdelay = 100000 -- microseconds
hledgerurl = "http://hledger.org"
manualurl = hledgerurl++"/MANUAL.html"

web :: [Opt] -> [String] -> Journal -> IO ()
web opts args j = do
  let host = fromMaybe defhost $ hostFromOpts opts
      port = fromMaybe defport $ portFromOpts opts
      url = printf "http://%s:%d" host port :: String
  unless (Debug `elem` opts) $ forkIO (browser url) >> return ()
  server url port opts args j

browser :: String -> IO ()
browser url = putStrLn "starting web browser" >> threadDelay browserstartdelay >> openBrowserOn url >> return ()

server :: String -> Int -> [Opt] -> [String] -> Journal -> IO ()
server url port opts args j = do
    printf "starting web server at %s\n" url
    fp <- getDataFileName "web"
    let app = HledgerWebApp{
               appOpts=opts
              ,appArgs=args
              ,appJournal=j
              ,appWebdir=fp
              ,appRoot=url
              }
    withStore "hledger" $ do -- IO ()
     putValue "hledger" "journal" j
     toWaiApp app >>= run port

data HledgerWebApp = HledgerWebApp {
      appOpts::[Opt]
     ,appArgs::[String]
     ,appJournal::Journal
     ,appWebdir::FilePath
     ,appRoot::String
     }

instance Yesod HledgerWebApp where approot = appRoot

mkYesod "HledgerWebApp" [$parseRoutes|
/             IndexPage        GET
/style.css    StyleCss         GET
/transactions TransactionsPage GET POST
/register     RegisterPage     GET
/balance      BalancePage      GET
|]

getIndexPage :: Handler HledgerWebApp ()
getIndexPage = redirect RedirectTemporary TransactionsPage

getStyleCss :: Handler HledgerWebApp ()
getStyleCss = do
    app <- getYesod
    let dir = appWebdir app
    s <- liftIO $ readFile $ dir </> "style.css"
    header "Content-Type" "text/css"
    return $ RepPlain $ toContent s

getTransactionsPage :: Handler HledgerWebApp RepHtml
getTransactionsPage = withLatestJournalRender (const showTransactions)

getRegisterPage :: Handler HledgerWebApp RepHtml
getRegisterPage = withLatestJournalRender showRegisterReport

getBalancePage :: Handler HledgerWebApp RepHtml
getBalancePage = withLatestJournalRender showBalanceReport

withLatestJournalRender :: ([Opt] -> FilterSpec -> Journal -> String) -> Handler HledgerWebApp RepHtml
withLatestJournalRender reportfn = do
    app <- getYesod
    params <- getParams
    t <- liftIO $ getCurrentLocalTime
    let head' x = if null x then "" else head x
        a = head' $ params "a"
        p = head' $ params "p"
        opts = appOpts app ++ [Period p]
        args = appArgs app ++ [a]
        fspec = optsToFilterSpec opts args t
    -- reload journal if changed
    j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
    (changed, j') <- liftIO $ journalReloadIfChanged opts j
    when changed $ liftIO $ putValue "hledger" "journal" j'
    -- run the specified report using this request's params
    let s = reportfn opts fspec j'
    -- render the standard template
    msg <- getMessage
    Just here <- getRoute
    hamletToRepHtml $ template here msg a p "hledger" s

template :: HledgerWebAppRoutes -> Maybe (Html ()) -> String -> String
         -> String -> String -> Hamlet HledgerWebAppRoutes
template here msg a p title content = [$hamlet|
!!!
%html
 %head
  %title $string.title$
  %meta!http-equiv=Content-Type!content=$string.metacontent$
  %link!rel=stylesheet!type=text/css!href=@stylesheet@!media=all
 %body
  ^navbar'^
  #messages $m$
  ^addform^
  #content
   %pre $string.content$
|]
 where m = fromMaybe (string "") msg
       navbar' = navbar here a p
       stylesheet = StyleCss
       metacontent = "text/html; charset=utf-8"

navbar :: HledgerWebAppRoutes -> String -> String -> Hamlet HledgerWebAppRoutes
navbar here a p = [$hamlet|
 #navbar
  %a#hledgerorglink!href=$string.hledgerurl$ hledger.org
  ^navlinks'^
  ^searchform'^
  %a#helplink!href=$string.manualurl$ help
|]
 where navlinks' = navlinks a p
       searchform' = searchform here a p

navlinks :: String -> String -> Hamlet HledgerWebAppRoutes
navlinks a p = [$hamlet|
 #navlinks
  ^transactionslink^ | $
  ^registerlink^ | $
  ^balancelink^
|]
 where
  transactionslink = navlink "transactions" TransactionsPage
  registerlink = navlink "register" RegisterPage
  balancelink = navlink "balance" BalancePage
  navlink s dest = [$hamlet|%a.navlink!href=@?u@ $string.s$|]
   where u = (dest, [("a", a), ("p", p)])

searchform :: HledgerWebAppRoutes -> String -> String -> Hamlet HledgerWebAppRoutes
searchform here a p = [$hamlet|
 %form#searchform!action=$string.action$
  search for: $
  %input!name=a!size=20!value=$string.a$
  ^ahelp^ $
  in reporting period: $
  %input!name=p!size=20!value=$string.p$
  ^phelp^ $
  %input!name=submit!type=submit!value=filter!style=display:none;
  ^resetlink^
|]
 where
  action=""
  ahelp = helplink "filter-patterns"
  phelp = helplink "period-expressions"
  resetlink
   | null a && null p = [$hamlet||]
   | otherwise        = [$hamlet|%span#resetlink $
                                  %a!href=@here@ reset|]

helplink topic = [$hamlet|%a!href=$string.u$ ?|]
    where u = manualurl ++ if null topic then "" else '#':topic

addform :: Hamlet HledgerWebAppRoutes
addform = [$hamlet|
 %form#addform!action=$string.action$!method=POST
  %table!border=0
   %tr
    %td
     Date:
     %input!size=15!name=date!value=$string.date$
     ^datehelp^ $
     Description:
     %input!size=35!name=desc!value=$string.desc$ $
   ^transactionfields1^
   ^transactionfields2^
   %tr#addbuttonrow
    %td
     %input!type=submit!value=$string.addlabel$
     ^addhelp^
 <br clear="all" />
|]
 where
  datehelp = helplink "dates"
  addlabel = "add transaction"
  addhelp = helplink "file-format"
  action=""
  date = ""
  desc = ""
  transactionfields1 = transactionfields 1
  transactionfields2 = transactionfields 2

-- transactionfields :: Int -> Hamlet String
transactionfields n = [$hamlet|
 %tr
  %td
   &nbsp;&nbsp;
   Account:
   %input!size=35!name=$string.acctvar$!value=$string.acct$
   &nbsp;
   Amount:
   %input!size=15!name=$string.amtvar$!value=$string.amt$ $
|]
 where
  acct = ""
  amt = ""
  numbered = (++ show n)
  acctvar = numbered "acct"
  amtvar = numbered "amt"

postTransactionsPage :: Handler HledgerWebApp RepPlain
postTransactionsPage = do
  today <- liftIO getCurrentDay
  -- get form input values, or basic validation errors. E means an Either value.
  dateE  <- runFormPost $ catchFormError $ notEmpty $ required $ input "date"
  descE  <- runFormPost $ catchFormError $ required $ input "desc"
  acct1E <- runFormPost $ catchFormError $ notEmpty $ required $ input "acct1"
  amt1E  <- runFormPost $ catchFormError $ required $ input "amt1"
  acct2E <- runFormPost $ catchFormError $ notEmpty $ required $ input "acct2"
  amt2E  <- runFormPost $ catchFormError $ required $ input "amt2"
  -- supply defaults and parse date and amounts, or get errors.
  let dateE' = either Left (either (\e -> Left [("date", showDateParseError e)]) Right . fixSmartDateStrEither today) dateE
      amt1E' = either Left (either (const (Right missingamt)) Right . parse someamount "") amt1E  -- XXX missingamt only when missing/empty
      amt2E' = either Left (either (const (Right missingamt)) Right . parse someamount "") amt2E
      strEs = [dateE', descE, acct1E, acct2E]
      amtEs = [amt1E', amt2E']
      errs = lefts strEs ++ lefts amtEs
      [date,desc,acct1,acct2] = rights strEs
      [amt1,amt2] = rights amtEs
      -- if no errors so far, generate a transaction and balance it or get the error.
      tE | not $ null errs = Left errs
         | otherwise = either (\e -> Left [[("unbalanced postings", head $ lines e)]]) Right
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
    setMessage $ string $ intercalate ", " $ map (intercalate ", " . map (\(a,b) -> a++": "++b)) errs
    redirect RedirectTemporary TransactionsPage

   Right t -> do
    let t' = txnTieKnot t -- XXX move into balanceTransaction
    j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
    -- j' <- liftIO $ journalAddTransaction j t' >>= journalReload
    -- liftIO $ putValue "hledger" "journal" j'
    liftIO $ journalAddTransaction j t'
    setMessage $ string $ printf "Added transaction:\n%s" (show t')
    redirect RedirectTemporary TransactionsPage

