{-| 
A happs-based web UI for hledger.
-}

module WebCommand
where
import qualified Data.Map as M
import Data.Map ((!))
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as B
-- import qualified Text.StringTemplate as T
-- import Codec.Compression.GZip (compress)
-- import Data.ByteString.UTF8 (fromString)
import HAppS.Server
import System.Cmd (system)
import System.Info (os)
import System.Exit

import Ledger
import Options
import BalanceCommand
import RegisterCommand
import PrintCommand


-- | The application state when running the ui command.
data AppState = AppState {
     aw :: Int                   -- ^ window width
    ,ah :: Int                   -- ^ window height
    ,amsg :: String              -- ^ status message
    ,aopts :: [Opt]              -- ^ command-line opts
    ,aargs :: [String]           -- ^ command-line args
    ,aledger :: Ledger           -- ^ parsed ledger
    ,abuf :: [String]            -- ^ lines of the current buffered view
                                -- ^ never null, head is current location
    } deriving (Show)

tcpport = 5000

web :: [Opt] -> [String] -> Ledger -> IO ()
web opts args l = do
  putStrLn $ printf "starting web server on port %d" tcpport
  tid <- forkIO $ simpleHTTP nullConf{port=tcpport} $ handlers opts args l
  putStrLn "starting web browser"
  browseUrl $ printf "http://localhost:%s/print" (show tcpport)
  waitForTermination
  putStrLn "shutting down..."
  killThread tid
  putStrLn "shutdown complete"

template r = printf (
             "<div style=float:right>" ++
             "<form action=%s>search:&nbsp;<input name=a value=%s></form>" ++
             "</div>" ++
             "<div align=center style=width:100%%%%>" ++
             " <a href=print>ledger</a>" ++
             " | <a href=register>register</a>" ++
             " | <a href=balance>balance</a>" ++
             "</div>" ++
             "<pre>%%s</pre>")
             (dropWhile (=='/') $ rqUri r)
             (fromMaybe "" $ queryValue "a" r)

type Handler = ServerPart Response

handlers :: [Opt] -> [String] -> Ledger -> [Handler]
handlers opts args l = 
        [
          dir "print" [withRequest $ \r -> respond r $ printreport r]
        , dir "register" [withRequest $ \r -> respond r $ registerreport r]
        , dir "balance" [withRequest $ \r -> respond r $ balancereport r]
        ]
    where
      respond r = ok . setContentType "text/html" . toResponse . (printf (template r) :: String -> String)
      printreport r = showEntries opts (pats r ++ args) l
      registerreport r = showRegisterReport opts (pats r ++ args) l
      balancereport r = showBalanceReport (opts++[SubTotal]) (pats r ++ args) l
      pats r = as -- ++ if null ds then [] else ("--":ds)
               where (as,ds) = (queryValues "a" r, queryValues "d" r)

queryValues :: String -> Request -> [String]
queryValues q r = map (B.unpack . inputValue . snd) $ filter ((==q).fst) $ rqInputs r

queryValue :: String -> Request -> Maybe String
queryValue q r = case filter ((==q).fst) $ rqInputs r of
                   [] -> Nothing
                   is -> Just $ B.unpack $ inputValue $ snd $ head is

-- | Attempt to open a web browser on the given url, all platforms.
browseUrl :: String -> IO ExitCode
browseUrl u = trybrowsers browsers u
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
      browsers | os=="darwin"  = ["open -a firefox", "open"]
               | os=="mingw32" = ["firefox","safari","opera","iexplore"]
               | otherwise     = ["firefox","sensible-browser"]
    -- jeffz: write a ffi binding for it using the Win32 package as a basis
    -- start by adding System/Win32/Shell.hsc and follow the style of any
    -- other module in that directory for types, headers, error handling and
    -- what not.
    -- ::ShellExecute(NULL, "open", "www.somepage.com", NULL, NULL, SW_SHOWNORMAL);
    -- ::ShellExecute(NULL, "open", "firefox.exe", "www.somepage.com" NULL, SW_SHOWNORMAL);

withExpiresHeaders :: ServerPart Response -> ServerPart Response
withExpiresHeaders sp = require getCacheTime $ \t -> [liftM (setHeader "Expires" $ formatDateTime "%a, %d %b %Y %T GMT" t) sp]

getCacheTime :: IO (Maybe UTCTime)
getCacheTime = getCurrentTime >>= (return . Just . addMinutes 360)

addMinutes :: Int -> UTCTime -> UTCTime
addMinutes n = addUTCTime (fromIntegral n) 

formatDateTime :: String -> UTCTime -> String
formatDateTime = formatTime defaultTimeLocale

setContentType :: String -> Response -> Response
setContentType = setHeader "Content-Type"

setFilename :: String -> Response -> Response
setFilename = setHeader "Content-Disposition" . \fname -> "attachment: filename=\"" ++ fname ++ "\""

-- gzipBinary :: Response -> Response
-- gzipBinary r@(Response {rsBody = b}) =  setHeader "Content-Encoding" "gzip" $ r {rsBody = compress b}

-- acceptsZip :: Request -> Bool
-- acceptsZip req = isJust $ M.lookup (fromString "accept-encoding") (rqHeaders req)

