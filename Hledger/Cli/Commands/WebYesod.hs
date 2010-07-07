{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, TemplateHaskell #-}
{-| 
A web-based UI.
-}

module Hledger.Cli.Commands.WebYesod
where

-- import Codec.Binary.UTF8.String (decodeString)
import Control.Concurrent -- (forkIO)
import qualified Network.Wai (Request(pathInfo))
import System.Directory (getModificationTime)
import System.FilePath ((</>))
import System.IO.Storage (withStore, putValue, getValue)
import System.Time (ClockTime, getClockTime, diffClockTimes, TimeDiff(TimeDiff))
import Text.Hamlet
-- import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as B
import Yesod
-- import Yesod.Helpers.Static

-- import Hledger.Cli.Commands.Add (journalAddTransaction)
import Hledger.Cli.Commands.Balance
-- import Hledger.Cli.Commands.Histogram
import Hledger.Cli.Commands.Print
import Hledger.Cli.Commands.Register

import Hledger.Cli.Options hiding (value)
import Hledger.Cli.Utils (openBrowserOn)
import Hledger.Data
import Hledger.Read
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

postTransactionsPage :: Handler HledgerWebApp RepHtml
postTransactionsPage = withLatestJournalRender (const showTransactions)

getRegisterPage :: Handler HledgerWebApp RepHtml
getRegisterPage = withLatestJournalRender showRegisterReport

getBalancePage :: Handler HledgerWebApp RepHtml
getBalancePage = withLatestJournalRender showBalanceReport

getStyleCss :: Handler HledgerWebApp RepPlain
getStyleCss = do
    app <- getYesod
    let dir = appWebdir app
    s <- liftIO $ readFile $ dir </> "style.css"
    header "Content-Type" "text/css"
    return $ RepPlain $ toContent s

withLatestJournalRender :: ([Opt] -> FilterSpec -> Journal -> String) -> Handler HledgerWebApp RepHtml
withLatestJournalRender f = do
    app <- getYesod
    req <- getRequest
    params <- getParams
    t <- liftIO $ getCurrentLocalTime
    let as = params "a"
        ps = params "p"
        opts = appOpts app ++ [Period $ unwords ps]
        args = appArgs app ++ as
        fs = optsToFilterSpec opts args t
    j <- liftIO $ fromJust `fmap` getValue "hledger" "journal"
    j' <- liftIO $ journalReloadIfChanged opts args j
    let content = f opts fs j'
    return $ RepHtml $ toContent $ renderHamlet id $ template req as ps "" content
    -- hamletToRepHtml $ template "" s

journalReloadIfChanged :: [Opt] -> [String] -> Journal -> IO Journal
journalReloadIfChanged opts _ j@Journal{filepath=f,filereadtime=tread} = do
  tmod <- journalFileModifiedTime j
  let newer = diffClockTimes tmod tread > (TimeDiff 0 0 0 0 0 0 0)
  -- when (Debug `elem` opts) $ printf "checking file, last modified %s, last read %s, %s\n" (show tmod) (show tread) (show newer)
  if newer
   then do
     when (Verbose `elem` opts) $ printf "%s has changed, reloading\n" f
     reload j
   else return j

journalFileModifiedTime :: Journal -> IO ClockTime
journalFileModifiedTime Journal{filepath=f}
    | null f = getClockTime
    | otherwise = getModificationTime f `Prelude.catch` \_ -> getClockTime

reload :: Journal -> IO Journal
reload Journal{filepath=f} = do
  j' <- readJournalFile Nothing f
  putValue "hledger" "journal" j'
  return j'
            
stylesheet = "/style.css"
-- stylesheet = StaticR "/style.css"
metacontent = "text/html; charset=utf-8"

template :: Request -> [String] -> [String] -> String -> String -> Hamlet String
template req as ps t s = [$hamlet|
!!!
%html
 %head
  %meta!http-equiv=Content-Type!content=$string.metacontent$
  %link!rel=stylesheet!type=text/css!href=@stylesheet@!media=all
  %title $string.t$
 %body
  ^navbar'^
  #messages $string.msgs$
  #content
   %pre $string.s$
|]
 where msgs = intercalate ", " []
       navbar' = navbar req as ps

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

{-
            
addform :: Hack.Env -> HSP XML
addform env = do
  today <- io $ liftM showDate $ getCurrentDay
  let inputs = Hack.Contrib.Request.inputs env
      date  = decodeString $ fromMaybe today $ lookup "date"  inputs
      desc  = decodeString $ fromMaybe "" $ lookup "desc"  inputs
  <div>
   <div id="addform">
   <form action="" method="POST">
    <table border="0">
      <tr>
        <td>
          Date: <input size="15" name="date" value=date /><% help "dates" %><% nbsp %>
          Description: <input size="35" name="desc" value=desc /><% nbsp %>
        </td>
      </tr>
      <% transactionfields 1 env %>
      <% transactionfields 2 env %>
      <tr id="addbuttonrow"><td><input type="submit" value="add transaction" 
      /><% help "file-format" %></td></tr>
    </table>
   </form>
   </div>
   <br clear="all" />
   </div>

transactionfields :: Int -> Hack.Env -> HSP XML
transactionfields n env = do
  let inputs = Hack.Contrib.Request.inputs env
      acct = decodeString $ fromMaybe "" $ lookup acctvar inputs
      amt  = decodeString $ fromMaybe "" $ lookup amtvar  inputs
  <tr>
    <td>
    <% nbsp %><% nbsp %>
      Account: <input size="35" name=acctvar value=acct /><% nbsp %>
      Amount: <input size="15" name=amtvar value=amt /><% nbsp %>
    </td>
   </tr>
    where
      numbered = (++ show n)
      acctvar = numbered "acct"
      amtvar = numbered "amt"

handleAddform :: Journal -> AppUnit
handleAddform j = do
  env <- getenv
  d <- io getCurrentDay
  t <- io getCurrentLocalTime
  handle t $ validate env d
  where
    validate :: Hack.Env -> Day -> Failing Transaction
    validate env today =
        let inputs = Hack.Contrib.Request.inputs env
            date  = decodeString $ fromMaybe "today" $ lookup "date"  inputs
            desc  = decodeString $ fromMaybe "" $ lookup "desc"  inputs
            acct1 = decodeString $ fromMaybe "" $ lookup "acct1" inputs
            amt1  = decodeString $ fromMaybe "" $ lookup "amt1"  inputs
            acct2 = decodeString $ fromMaybe "" $ lookup "acct2" inputs
            amt2  = decodeString $ fromMaybe "" $ lookup "amt2"  inputs
            validateDate ""  = ["missing date"]
            validateDate _   = []
            validateDesc ""  = ["missing description"]
            validateDesc _   = []
            validateAcct1 "" = ["missing account 1"]
            validateAcct1 _  = []
            validateAmt1 ""  = ["missing amount 1"]
            validateAmt1 _   = []
            validateAcct2 "" = ["missing account 2"]
            validateAcct2 _  = []
            validateAmt2 _   = []
            amt1' = either (const missingamt) id $ parse someamount "" amt1
            amt2' = either (const missingamt) id $ parse someamount "" amt2
            (date', dateparseerr) = case fixSmartDateStrEither today date of
                                      Right d -> (d, [])
                                      Left e -> ("1900/01/01", [showDateParseError e])
            t = Transaction {
                            tdate = parsedate date' -- date' must be parseable
                           ,teffectivedate=Nothing
                           ,tstatus=False
                           ,tcode=""
                           ,tdescription=desc
                           ,tcomment=""
                           ,tpostings=[
                             Posting False acct1 amt1' "" RegularPosting (Just t')
                            ,Posting False acct2 amt2' "" RegularPosting (Just t')
                            ]
                           ,tpreceding_comment_lines=""
                           }
            (t', balanceerr) = case balanceTransaction t of
                           Right t'' -> (t'', [])
                           Left e -> (t, [head $ lines e]) -- show just the error not the transaction
            errs = concat [
                    validateDate date
                   ,dateparseerr
                   ,validateDesc desc
                   ,validateAcct1 acct1
                   ,validateAmt1 amt1
                   ,validateAcct2 acct2
                   ,validateAmt2 amt2
                   ,balanceerr
                   ]
        in
        case null errs of
          False -> Failure errs
          True  -> Success t'

    handle :: LocalTime -> Failing Transaction -> AppUnit
    handle _ (Failure errs) = hsp errs addform
    handle ti (Success t)   = do
                    io $ journalAddTransaction j t >> reload j
                    ledgerpage [msg] j (showTransactions (optsToFilterSpec [] [] ti))
       where msg = printf "Added transaction:\n%s" (show t)

-}
