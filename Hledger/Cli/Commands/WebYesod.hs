{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, TemplateHaskell #-}
{-| 
A web-based UI.
-}

module Hledger.Cli.Commands.WebYesod
where
import Control.Concurrent -- (forkIO)
import qualified Data.ByteString.Char8 as B
import Data.Either
import qualified Network.Wai (Request(pathInfo))
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
     toWaiApp app >>= basicHandler port

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
/transactions TransactionsPage GET POST
/register     RegisterPage     GET
/balance      BalancePage      GET
/style.css    StyleCss         GET
/params       ParamsDebug      GET
|]

getParamsDebug = do
    r <- getRequest
    return $ RepHtml $ toContent $ show $ reqGetParams r

getIndexPage :: Handler HledgerWebApp ()
getIndexPage = redirect RedirectTemporary TransactionsPage

getTransactionsPage :: Handler HledgerWebApp RepHtml
getTransactionsPage = withLatestJournalRender (const showTransactions)

getRegisterPage :: Handler HledgerWebApp RepHtml
getRegisterPage = withLatestJournalRender showRegisterReport

getBalancePage :: Handler HledgerWebApp RepHtml
getBalancePage = withLatestJournalRender showBalanceReport

withLatestJournalRender :: ([Opt] -> FilterSpec -> Journal -> String) -> Handler HledgerWebApp RepHtml
withLatestJournalRender f = do
    app <- getYesod
    req <- getRequest
    params <- getParams
    msg <- getMessage
    t <- liftIO $ getCurrentLocalTime
    -- today <- liftIO $ liftM showDate $ getCurrentDay
    let as = params "a"
        ps = params "p"
        opts = appOpts app ++ [Period $ unwords ps]
        args = appArgs app ++ as
        fs = optsToFilterSpec opts args t
  --    date = fromMaybe (decodeString today) $ getParam "date"
  --    desc = fromMaybe "" $ getParam "desc"
  --    acct = fromMaybe "" $ getParam "acctvar"
  --    amt  = fromMaybe "" $ getParam "amtvar"
    j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
    (changed, j') <- liftIO $ journalReloadIfChanged opts j
    when changed $ liftIO $ putValue "hledger" "journal" j'
    let content = f opts fs j'
    return $ RepHtml $ toContent $ renderHamlet id $ template req msg as ps "hledger" content
    -- hamletToRepHtml $ template "" s

getStyleCss :: Handler HledgerWebApp RepPlain
getStyleCss = do
    app <- getYesod
    let dir = appWebdir app
    s <- liftIO $ readFile $ dir </> "style.css"
    header "Content-Type" "text/css"
    return $ RepPlain $ toContent s

template :: Request -> Maybe (Html ()) -> [String] -> [String] -> String -> String -> Hamlet String
template req msg as ps t s = [$hamlet|
!!!
%html
 %head
  %meta!http-equiv=Content-Type!content=$string.metacontent$
  %link!rel=stylesheet!type=text/css!href=@stylesheet@!media=all
  %title $string.t$
 %body
  ^navbar'^
  #messages $m$
  ^addform'^
  #content
   %pre $string.s$
|]
 where m = fromMaybe (string "") msg
       navbar' = navbar req as ps
       addform' = addform req as ps
       stylesheet = "/style.css"
       metacontent = "text/html; charset=utf-8"

navbar :: Request -> [String] -> [String] -> Hamlet String
navbar req as ps = [$hamlet|
 #navbar
  %a#hledgerorglink!href=@hledgerurl@ hledger.org
  ^navlinks'^
  ^searchform'^
  %a#helplink!href=@manualurl@ help
|]
 where navlinks' = navlinks req as ps
       searchform' = searchform req as ps

navlinks :: Request -> [String] -> [String] -> Hamlet String
navlinks _ as ps = [$hamlet|
 #navlinks
  ^transactionslink^ | $
  ^registerlink^ | $
  ^balancelink^
|]
 where
  transactionslink = navlink "transactions"
  registerlink = navlink "register"
  balancelink = navlink "balance"
  navlink s = [$hamlet|%a.navlink!href=@u@ $string.s$|]
   where u = printf "../%s?a=%s&p=%s" s (intercalate "+" as) (intercalate "+" ps)

searchform :: Request -> [String] -> [String] -> Hamlet String
searchform req as ps = [$hamlet|
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
  a = intercalate "+" as
  p = intercalate "+" ps
  ahelp = helplink "filter-patterns"
  phelp = helplink "period-expressions"
  resetlink
   | null a && null p = [$hamlet||]
   | otherwise        = [$hamlet|%span#resetlink $
                                  %a!href=@u@ reset|]
   where u = B.unpack $ Network.Wai.pathInfo $ waiRequest req

helplink topic = [$hamlet|%a!href=@u@ ?|]
    where u = manualurl ++ if null topic then "" else '#':topic

addform :: Request -> [String] -> [String] -> Hamlet String
addform _ _ _ = [$hamlet|
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

transactionfields :: Int -> Hamlet String
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
  -- get form input values, or basic validation errors. E suffix means an Either value.
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
    j' <- liftIO $ journalAddTransaction j t' >>= journalReload
    liftIO $ putValue "hledger" "journal" j'
    setMessage $ string $ printf "Added transaction:\n%s" (show t')
    redirect RedirectTemporary TransactionsPage

