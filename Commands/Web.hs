{-| 
A server-side-html web UI using happstack.
-}

module Commands.Web
where
import Control.Concurrent
import Happstack.Server
import Happstack.State.Control (waitForTermination)
import Network.HTTP (urlEncode, urlDecode)
import Text.XHtml hiding (dir)

import Ledger
import Options hiding (value)
import Commands.Balance
import Commands.Register
import Commands.Print
import Commands.Histogram
import Utils (filterAndCacheLedgerWithOpts, openBrowserOn)


tcpport = 5000

web :: [Opt] -> [String] -> Ledger -> IO ()
web opts args l = do
  t <- getCurrentLocalTime -- how to get this per request ?
  if Debug `elem` opts
   then do
    -- just run the server in the foreground
    putStrLn $ printf "starting web server on port %d in debug mode" tcpport
    simpleHTTP nullConf{port=tcpport} $ handlers opts args l t
   else do
    -- start the server (in background, so we can..) then start the web browser
    printf "starting web interface at %s\n" homeurl
    tid <- forkIO $ simpleHTTP nullConf{port=tcpport} $ handlers opts args l t
    putStrLn "starting web browser"
    openBrowserOn homeurl
    waitForTermination
    putStrLn "shutting down web server..."
    killThread tid
    putStrLn "shutdown complete"

homeurl = printf "http://localhost:%d/" tcpport

handlers :: [Opt] -> [String] -> Ledger -> LocalTime -> ServerPartT IO Response
handlers opts args l t = msum
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
      ,submit "submit" "filter" ! [thestyle "display:none;"]
      ,resetlink]
    where
      -- another way to get them
      -- a = fromMaybe "" $ queryValue "a" r
      -- p = fromMaybe "" $ queryValue "p" r
      u = rqUri r
      s = "20"
      resetlink | null a && null p' = noHtml
                | otherwise = spaceHtml +++ anchor ! [href u] << stringToHtml "reset"

navlinks :: Request -> String -> String -> Html
navlinks _ a p' = 
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

