{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
{-| 
A web-based UI.
-}

module Hledger.Cli.Commands.Web
where
#if __GLASGOW_HASKELL__ <= 610
import Codec.Binary.UTF8.String (decodeString)
#endif
import Control.Applicative.Error (Failing(Success,Failure))
import Control.Concurrent
import Control.Monad.Reader (ask)
import Data.IORef (newIORef, atomicModifyIORef)
import Network.HTTP (urlEncode, urlDecode)
import System.Directory (getModificationTime)
import System.IO.Storage (withStore, putValue, getValue)
import System.Time (ClockTime, getClockTime, diffClockTimes, TimeDiff(TimeDiff))
import Text.ParserCombinators.Parsec (parse)

import Hack.Contrib.Constants (_TextHtmlUTF8)
import Hack.Contrib.Response (set_content_type)
import qualified Hack (Env, http)
import qualified Hack.Contrib.Request (inputs, params, path)
import qualified Hack.Contrib.Response (redirect)
#ifdef WEBHAPPSTACK
import System.Process (readProcess)
import Hack.Handler.Happstack (runWithConfig,ServerConf(ServerConf))
#else
import Hack.Handler.SimpleServer (run)
#endif

import Network.Loli (loli, io, get, post, html, text, public)
import Network.Loli.Type (AppUnit)
import Network.Loli.Utils (update)

import HSP hiding (Request,catch)
import qualified HSP (Request(..))

import Hledger.Cli.Commands.Add (ledgerAddTransaction)
import Hledger.Cli.Commands.Balance
import Hledger.Cli.Commands.Histogram
import Hledger.Cli.Commands.Print
import Hledger.Cli.Commands.Register
import Hledger.Data
import Hledger.Cli.Options hiding (value)
#ifdef MAKE
import Paths_hledger_make (getDataFileName)
#else
import Paths_hledger (getDataFileName)
#endif
import Hledger.Utils (openBrowserOn)

-- import Debug.Trace
-- strace :: Show a => a -> a
-- strace a = trace (show a) a

tcpport = 5000 :: Int
homeurl = printf "http://localhost:%d/" tcpport
browserdelay = 100000 -- microseconds

web :: [Opt] -> [String] -> Ledger -> IO ()
web opts args l = do
  unless (Debug `elem` opts) $ forkIO browser >> return ()
  server opts args l

browser :: IO ()
browser = putStrLn "starting web browser" >> threadDelay browserdelay >> openBrowserOn homeurl >> return ()

server :: [Opt] -> [String] -> Ledger -> IO ()
server opts args l =
  -- server initialisation
  withStore "hledger" $ do -- IO ()
    printf "starting web server on port %d\n" tcpport
    t <- getCurrentLocalTime
    webfiles <- getDataFileName "web"
    putValue "hledger" "ledger" l
#ifdef WEBHAPPSTACK
    hostname <- readProcess "hostname" [] "" `catch` \_ -> return "hostname"
    runWithConfig (ServerConf tcpport hostname) $            -- (Env -> IO Response) -> IO ()
#else
    run tcpport $            -- (Env -> IO Response) -> IO ()
#endif
      \env -> do -- IO Response
       -- general request handler
       let a = intercalate "+" $ reqparam env "a"
           p = intercalate "+" $ reqparam env "p"
           opts' = opts ++ [Period p]
           args' = args ++ (map urlDecode $ words a)
       l' <- fromJust `fmap` getValue "hledger" "ledger"
       l'' <- reloadIfChanged opts' args' l'
       -- declare path-specific request handlers
       let command :: [String] -> ([Opt] -> FilterSpec -> Ledger -> String) -> AppUnit
           command msgs f = string msgs $ f opts' (optsToFilterSpec opts' args' t) l''
       (loli $                                               -- State Loli () -> (Env -> IO Response)
         do
          get  "/balance"   $ command [] showBalanceReport  -- String -> ReaderT Env (StateT Response IO) () -> State Loli ()
          get  "/register"  $ command [] showRegisterReport
          get  "/histogram" $ command [] showHistogram
          get  "/transactions"   $ ledgerpage [] l'' (showTransactions (optsToFilterSpec opts' args' t))
          post "/transactions"   $ handleAddform l''
          get  "/env"       $ getenv >>= (text . show)
          get  "/params"    $ getenv >>= (text . show . Hack.Contrib.Request.params)
          get  "/inputs"    $ getenv >>= (text . show . Hack.Contrib.Request.inputs)
          public (Just webfiles) ["/style.css"]
          get  "/"          $ redirect ("transactions") Nothing
          ) env

getenv = ask
response = update
redirect u c = response $ Hack.Contrib.Response.redirect u c

reqparam :: Hack.Env -> String -> [String]
#if __GLASGOW_HASKELL__ <= 610
reqparam env p = map snd $ filter ((==p).fst) $ Hack.Contrib.Request.params env
#else
reqparam env p = map (decodeString.snd) $ filter ((==p).fst) $ Hack.Contrib.Request.params env
#endif

ledgerFileModifiedTime :: Ledger -> IO ClockTime
ledgerFileModifiedTime l
    | null path = getClockTime
    | otherwise = getModificationTime path `Prelude.catch` \_ -> getClockTime
    where path = filepath $ journal l

ledgerFileReadTime :: Ledger -> ClockTime
ledgerFileReadTime l = filereadtime $ journal l

reload :: Ledger -> IO Ledger
reload l = do
  l' <- readLedger (filepath $ journal l)
  putValue "hledger" "ledger" l'
  return l'
            
reloadIfChanged :: [Opt] -> [String] -> Ledger -> IO Ledger
reloadIfChanged opts _ l = do
  tmod <- ledgerFileModifiedTime l
  let tread = ledgerFileReadTime l
      newer = diffClockTimes tmod tread > (TimeDiff 0 0 0 0 0 0 0)
  -- when (Debug `elem` opts) $ printf "checking file, last modified %s, last read %s, %s\n" (show tmod) (show tread) (show newer)
  if newer
   then do
     when (Verbose `elem` opts) $ printf "%s has changed, reloading\n" (filepath $ journal l)
     reload l
   else return l

-- refilter :: [Opt] -> [String] -> Ledger -> LocalTime -> IO Ledger
-- refilter opts args l t = return $ filterAndCacheLedgerWithOpts opts args t (jtext $ journal l) (journal l)

ledgerpage :: [String] -> Ledger -> (Ledger -> String) -> AppUnit
ledgerpage msgs l f = do
  env <- getenv
  l' <- io $ reloadIfChanged [] [] l
  hsp msgs $ const <div><% addform env %><pre><% f l' %></pre></div>

-- | A loli directive to serve a string in pre tags within the hledger web
-- layout.
string :: [String] -> String -> AppUnit
string msgs s = hsp msgs $ const <pre><% s %></pre>

-- | A loli directive to serve a hsp template wrapped in the hledger web
-- layout. The hack environment is passed in to every hsp template as an
-- argument, since I don't see how to get it within the hsp monad.
-- A list of messages is also passed, eg for form errors.
hsp :: [String] -> (Hack.Env -> HSP XML) -> AppUnit
hsp msgs f = do
  env <- getenv
  let contenthsp = f env
      pagehsp = hledgerpage env msgs title contenthsp
  html =<< (io $ do
              hspenv <- hackEnvToHspEnv env
              (_,xml) <- runHSP html4Strict pagehsp hspenv
              return $ addDoctype $ renderAsHTML xml)
  response $ set_content_type _TextHtmlUTF8
    where
      title = ""
      addDoctype = ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n" ++)
      hackEnvToHspEnv :: Hack.Env -> IO HSPEnv
      hackEnvToHspEnv env = do
          x <- newIORef 0
          let req = HSP.Request (reqparam env) (Hack.http env)
              num = NumberGen (atomicModifyIORef x (\a -> (a+1,a)))
          return $ HSPEnv req num

-- htmlToHsp :: Html -> HSP XML
-- htmlToHsp h = return $ cdata $ showHtml h

-- views

hledgerpage :: Hack.Env -> [String] -> String -> HSP XML -> HSP XML
hledgerpage env msgs title content =
    <html>
      <head>
        <meta http-equiv = "Content-Type" content = "text/html; charset=utf-8" />
        <link rel="stylesheet" type="text/css" href="/style.css" media="all" />
        <title><% title %></title>
      </head>
      <body>
        <% navbar env %>
        <div id="messages"><% intercalate ", " msgs %></div>
        <div id="content"><% content %></div>
      </body>
    </html>

navbar :: Hack.Env -> HSP XML
navbar env =
    <div id="navbar">
      <a href="http://hledger.org" id="hledgerorglink">hledger.org</a>
      <% navlinks env %>
      <% searchform env %>
      <a href="http://hledger.org/MANUAL.html" id="helplink">help</a>
    </div>

#if __GLASGOW_HASKELL__ <= 610
getParamOrNull p = (decodeString . fromMaybe "") `fmap` getParam p
#else
getParamOrNull p = fromMaybe "" `fmap` getParam p
#endif

navlinks :: Hack.Env -> HSP XML
navlinks _ = do
   a <- getParamOrNull "a"
   p <- getParamOrNull "p"
   let addparams=(++(printf "?a=%s&p=%s" (urlEncode a) (urlEncode p)))
       link s = <a href=(addparams s) class="navlink"><% s %></a>
   <div id="navlinks">
     <% link "transactions" %> |
     <% link "register" %> |
     <% link "balance" %>
    </div>

searchform :: Hack.Env -> HSP XML
searchform env = do
   a <- getParamOrNull "a"
   p <- getParamOrNull "p"
   let resetlink | null a && null p = <span></span>
                 | otherwise = <span id="resetlink"><% nbsp %><a href=u>reset</a></span>
                 where u = dropWhile (=='/') $ Hack.Contrib.Request.path env
   <form action="" id="searchform">
      <% nbsp %>search for:<% nbsp %><input name="a" size="20" value=a
      /><% help "filter-patterns"
      %><% nbsp %><% nbsp %>in reporting period:<% nbsp %><input name="p" size="20" value=p
      /><% help "period-expressions"
      %><input type="submit" name="submit" value="filter" style="display:none" />
      <% resetlink %>
    </form>

addform :: Hack.Env -> HSP XML
addform env = do
  today <- io $ liftM showDate $ getCurrentDay
  let inputs = Hack.Contrib.Request.inputs env
#if __GLASGOW_HASKELL__ <= 610
      date  = decodeString $ fromMaybe today $ lookup "date"  inputs
      desc  = decodeString $ fromMaybe "" $ lookup "desc"  inputs
#else
      date  = fromMaybe today $ lookup "date"  inputs
      desc  = fromMaybe "" $ lookup "desc"  inputs
#endif
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

help :: String -> HSP XML
help topic = <a href=u>?</a>
    where u = printf "http://hledger.org/MANUAL.html%s" l :: String
          l | null topic = ""
            | otherwise = '#':topic

transactionfields :: Int -> Hack.Env -> HSP XML
transactionfields n env = do
  let inputs = Hack.Contrib.Request.inputs env
#if __GLASGOW_HASKELL__ <= 610
      acct = decodeString $ fromMaybe "" $ lookup acctvar inputs
      amt  = decodeString $ fromMaybe "" $ lookup amtvar  inputs
#else
      acct = fromMaybe "" $ lookup acctvar inputs
      amt  = fromMaybe "" $ lookup amtvar  inputs
#endif
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

handleAddform :: Ledger -> AppUnit
handleAddform l = do
  env <- getenv
  d <- io getCurrentDay
  t <- io getCurrentLocalTime
  handle t $ validate env d
  where
    validate :: Hack.Env -> Day -> Failing Transaction
    validate env today =
        let inputs = Hack.Contrib.Request.inputs env
#if __GLASGOW_HASKELL__ <= 610
            date  = decodeString $ fromMaybe "today" $ lookup "date"  inputs
            desc  = decodeString $ fromMaybe "" $ lookup "desc"  inputs
            acct1 = decodeString $ fromMaybe "" $ lookup "acct1" inputs
            amt1  = decodeString $ fromMaybe "" $ lookup "amt1"  inputs
            acct2 = decodeString $ fromMaybe "" $ lookup "acct2" inputs
            amt2  = decodeString $ fromMaybe "" $ lookup "amt2"  inputs
#else
            date  = fromMaybe "today" $ lookup "date"  inputs
            desc  = fromMaybe "" $ lookup "desc"  inputs
            acct1 = fromMaybe "" $ lookup "acct1" inputs
            amt1  = fromMaybe "" $ lookup "amt1"  inputs
            acct2 = fromMaybe "" $ lookup "acct2" inputs
            amt2  = fromMaybe "" $ lookup "amt2"  inputs
#endif
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
                    io $ ledgerAddTransaction l t >> reload l
                    ledgerpage [msg] l (showTransactions (optsToFilterSpec [] [] ti))
       where msg = printf "Added transaction:\n%s" (show t)

nbsp :: XML
nbsp = cdata "&nbsp;"
