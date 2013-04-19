-- | Web handler utilities.

module Handler.Utils where

import Prelude
import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import Data.Maybe
import Data.Text(pack,unpack)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Locale (defaultTimeLocale)
#if BLAZE_HTML_0_4
import Text.Blaze (toHtml)
#else
import Text.Blaze.Html (toHtml)
#endif
import Text.Hamlet
import Yesod.Core

import Foundation

import Hledger hiding (is)
import Hledger.Cli hiding (version)
import Hledger.Web.Options


-- | A bundle of data useful for hledger-web request handlers and templates.
data ViewData = VD {
     opts         :: WebOpts    -- ^ the command-line options at startup
    ,here         :: AppRoute   -- ^ the current route
    ,msg          :: Maybe Html -- ^ the current UI message if any, possibly from the current request
    ,today        :: Day        -- ^ today's date (for queries containing relative dates)
    ,j            :: Journal    -- ^ the up-to-date parsed unfiltered journal
    ,q            :: String     -- ^ the current q parameter, the main query expression
    ,m            :: Query    -- ^ a query parsed from the q parameter
    ,qopts        :: [QueryOpt] -- ^ query options parsed from the q parameter
    ,am           :: Query    -- ^ a query parsed from the accounts sidebar query expr ("a" parameter)
    ,aopts        :: [QueryOpt] -- ^ query options parsed from the accounts sidebar query expr
    ,showpostings :: Bool       -- ^ current p parameter, 1 or 0 shows/hides all postings where applicable
    }

-- | Make a default ViewData, using day 0 as today's date.
nullviewdata :: ViewData
nullviewdata = viewdataWithDateAndParams nulldate "" "" ""

-- | Make a ViewData using the given date and request parameters, and defaults elsewhere.
viewdataWithDateAndParams :: Day -> String -> String -> String -> ViewData
viewdataWithDateAndParams d q a p =
    let (querymatcher,queryopts) = parseQuery d q
        (acctsmatcher,acctsopts) = parseQuery d a
    in VD {
           opts         = defwebopts
          ,j            = nulljournal
          ,here         = RootR
          ,msg          = Nothing
          ,today        = d
          ,q            = q
          ,m            = querymatcher
          ,qopts        = queryopts
          ,am           = acctsmatcher
          ,aopts        = acctsopts
          ,showpostings = p == "1"
          }

-- | Gather data used by handlers and templates in the current request.
getViewData :: Handler ViewData
getViewData = do
  app        <- getYesod
  let opts@WebOpts{cliopts_=copts@CliOpts{reportopts_=ropts}} = appOpts app
  (j, err)   <- getCurrentJournal app copts{reportopts_=ropts{no_elide_=True}}
  msg        <- getMessageOr err
  Just here  <- getCurrentRoute
  today      <- liftIO getCurrentDay
  q          <- getParameterOrNull "q"
  a          <- getParameterOrNull "a"
  p          <- getParameterOrNull "p"
  return (viewdataWithDateAndParams today q a p){
               opts=opts
              ,msg=msg
              ,here=here
              ,today=today
              ,j=j
              }
    where
      -- | Update our copy of the journal if the file changed. If there is an
      -- error while reloading, keep the old one and return the error, and set a
      -- ui message.
      getCurrentJournal :: App -> CliOpts -> Handler (Journal, Maybe String)
      getCurrentJournal app opts = do
        -- XXX put this inside atomicModifyIORef' for thread safety
        j <- liftIO $ readIORef $ appJournal app
        (jE, changed) <- liftIO $ journalReloadIfChanged opts j
        if not changed
         then return (j,Nothing)
         else case jE of
                Right j' -> do liftIO $ writeIORef (appJournal app) j'
                               return (j',Nothing)
                Left e   -> do setMessage $ "error while reading" {- ++ ": " ++ e-}
                               return (j, Just e)

      -- | Get the named request parameter, or the empty string if not present.
      getParameterOrNull :: String -> Handler String
      getParameterOrNull p = unpack `fmap` fromMaybe "" <$> lookupGetParam (pack p)

-- | Get the message set by the last request, or the newer message provided, if any.
getMessageOr :: Maybe String -> Handler (Maybe Html)
getMessageOr mnewmsg = do
  oldmsg <- getMessage
  return $ maybe oldmsg (Just . toHtml) mnewmsg

numbered :: [a] -> [(Int,a)]
numbered = zip [1..]

dayToJsTimestamp :: Day -> Integer
dayToJsTimestamp d = read (formatTime defaultTimeLocale "%s" t) * 1000 -- XXX read
                     where t = UTCTime d (secondsToDiffTime 0)

chomp :: String -> String
chomp = reverse . dropWhile (`elem` "\r\n") . reverse

