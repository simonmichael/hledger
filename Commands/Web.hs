{-| 
A happs-based web UI for hledger.
-}

module Commands.Web
where
import Control.Monad.Trans (liftIO)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Map as M
-- import Data.Map ((!))
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as B
import Happstack.Data (defaultValue)
import Happstack.Server
import Happstack.Server.HTTP.FileServe (fileServe)
import Happstack.State.Control (waitForTermination)
import System.Cmd (system)
import System.Info (os)
import System.Exit
import Network.HTTP (urlEncode, urlDecode, urlEncodeVars)
import Text.XHtml hiding (dir)

import Ledger
import Options hiding (value)
import Commands.Balance
import Commands.Register
import Commands.Print
import Commands.Histogram
import Utils (filterAndCacheLedgerWithOpts)


tcpport = 5000

web :: [Opt] -> [String] -> Ledger -> IO ()
web opts args l = do
  t <- getCurrentLocalTime -- how to get this per request ?
  if Debug `elem` opts
   then do
    -- just run the server in the foreground
    putStrLn $ printf "starting web server on port %d in debug mode" tcpport
    simpleHTTP nullConf{port=tcpport} $ webHandlers opts args l t
   else do
    -- start the server (in background, so we can..) then start the web browser
    putStrLn $ printf "starting web server on port %d" tcpport
    tid <- forkIO $ simpleHTTP nullConf{port=tcpport} $ webHandlers opts args l t
    putStrLn "starting web browser"
    openBrowserOn $ printf "http://localhost:%d/" tcpport
    waitForTermination
    putStrLn "shutting down web server..."
    killThread tid
    putStrLn "shutdown complete"

webHandlers :: [Opt] -> [String] -> Ledger -> LocalTime -> ServerPartT IO Response
webHandlers opts args l t = msum
 [
  methodSP GET    $ view showBalanceReport
 ,dir "balance"   $ view showBalanceReport
 ,dir "register"  $ view showRegisterReport
 ,dir "print"     $ view showLedgerTransactions
 ,dir "histogram" $ view showHistogram
 ]
 where 
   view f = withDataFn rqdata $ render f
   render f (a,p) = renderPage (a,p) $ f opts' args' l'
       where
         opts' = opts ++ [Period p]
         args' = args ++ (map urlDecode $ words a)
         -- re-filter the full ledger with the new opts
         l' = filterAndCacheLedgerWithOpts opts' args' t (rawledgertext l) (rawledger l)
   rqdata = do
     a <- look "a" `mplus` return "" -- filter patterns
     p <- look "p" `mplus` return "" -- reporting period
     return (a,p)
   renderPage :: (String, String) -> String -> ServerPartT IO Response
   renderPage (a,p) s = do
     r <- askRq
     return $ setHeader "Content-Type" "text/html" $ toResponse $ renderHtml $ hledgerview r a p s

{-
 <div style=\"float:right;text-align:right;\">
 <form action=%s>
 &nbsp; filter by:&nbsp;<input name=a size=30 value=\"%s\">
 &nbsp; reporting period:&nbsp;<input name=p size=30 value=\"%s\">
 %s
 </form>
 </div>
 <div style=\"width:100%%; font-weight:bold;\">
  <a href=balance%s>balance</a>
 | <a href=register%s>register</a>
 | <a href=print%s>print</a>
 | <a href=histogram%s>histogram</a>
 </div>
 <pre>%s</pre>
-}
hledgerview :: Request -> String -> String -> String -> Html
hledgerview r a p' s = body << topbar r a p' +++ pre << s

topbar :: Request -> String -> String -> Html
topbar r a p' = concatHtml
    [thediv ! [thestyle "float:right; text-align:right;"] << searchform r a p'
    ,thediv ! [thestyle "width:100%; font-weight:bold;"] << navlinks r a p']

searchform :: Request -> String -> String -> Html
searchform r a p' =
    form ! [action u] << concatHtml
      [spaceHtml +++ stringToHtml "filter by:" +++ spaceHtml 
      ,textfield "a" ! [size s, value a]
      ,spaceHtml
      ,spaceHtml +++ stringToHtml "reporting period:" +++ spaceHtml 
      ,textfield "p" ! [size s, value p']
      ,resetlink]
    where
      -- another way to get them
      -- a = fromMaybe "" $ queryValue "a" r
      -- p = fromMaybe "" $ queryValue "p" r
      u = dropWhile (=='/') $ rqUri r
      s = "20"
      resetlink | null a && null p' = noHtml
                | otherwise = spaceHtml +++ anchor ! [href u] << stringToHtml "reset"

navlinks :: Request -> String -> String -> Html
navlinks r a p' = 
    concatHtml $ intersperse sep $ map linkto ["balance", "register", "print", "histogram"]
    where
      sep = stringToHtml " | "
      linkto s = anchor ! [href (s++q)] << s
      q' = intercalate "&" $
           (if null a then [] else [(("a="++).urlEncode) a]) ++ 
           (if null p' then [] else [(("p="++).urlEncode) p'])
      q = if null q' then "" else '?':q'

-- queryValues :: String -> Request -> [String]
-- queryValues q r = map (B.unpack . inputValue . snd) $ filter ((==q).fst) $ rqInputs r

-- queryValue :: String -> Request -> Maybe String
-- queryValue q r = case filter ((==q).fst) $ rqInputs r of
--                    [] -> Nothing
--                    is -> Just $ B.unpack $ inputValue $ snd $ head is

-- | Attempt to open a web browser on the given url, all platforms.
openBrowserOn :: String -> IO ExitCode
openBrowserOn u = trybrowsers browsers u
    where
      trybrowsers (b:bs) u = do
        e <- system $ printf "%s %s" b u
        case e of
          ExitSuccess -> return ExitSuccess
          ExitFailure _ -> trybrowsers bs u
      trybrowsers [] u = do
        putStrLn $ printf "Sorry, I could not start a browser (tried: %s)" $ intercalate ", " browsers
        putStrLn $ printf "Please open your browser and visit %s" u
        return $ ExitFailure 127
      browsers | os=="darwin"  = ["open"]
               | os=="mingw32" = ["firefox","safari","opera","iexplore"]
               | otherwise     = ["sensible-browser","firefox"]
    -- jeffz: write a ffi binding for it using the Win32 package as a basis
    -- start by adding System/Win32/Shell.hsc and follow the style of any
    -- other module in that directory for types, headers, error handling and
    -- what not.
    -- ::ShellExecute(NULL, "open", "www.somepage.com", NULL, NULL, SW_SHOWNORMAL);
    -- ::ShellExecute(NULL, "open", "firefox.exe", "www.somepage.com" NULL, SW_SHOWNORMAL);

