{-|

hledger-web - a hledger add-on providing a web interface.
Copyright (c) 2007-2012 Simon Michael <simon@joyful.com>
Released under GPL version 3 or later.

-}

module Hledger.Web.Main
where

-- yesod scaffold imports
import Yesod.Default.Config --(fromArgs)
-- import Yesod.Default.Main   (defaultMain)
import Settings            --  (parseExtra)
import Application          (makeApplication)
import Data.String
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort)
import Network.Wai.Handler.Launch (runUrlPort)
--
import Prelude hiding (putStrLn)
import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Text (pack)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Printf

import Hledger
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.Web.Options


main :: IO ()
main = do
  opts <- getHledgerWebOpts
  when (debug_ (cliopts_ opts) > 0) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
  runWith opts

runWith :: WebOpts -> IO ()
runWith opts
  | "help" `inRawOpts` (rawopts_ $ cliopts_ opts)            = putStr (showModeHelp webmode) >> exitSuccess
  | "version" `inRawOpts` (rawopts_ $ cliopts_ opts)         = putStrLn prognameandversion >> exitSuccess
  | "binary-filename" `inRawOpts` (rawopts_ $ cliopts_ opts) = putStrLn (binaryfilename progname)
  | otherwise = do
    requireJournalFileExists =<< journalFilePathFromOpts (cliopts_ opts)
    withJournalDo' opts web

withJournalDo' :: WebOpts -> (WebOpts -> Journal -> IO ()) -> IO ()
withJournalDo' opts cmd = do
  journalFilePathFromOpts (cliopts_ opts) >>= readJournalFile Nothing Nothing True >>=
    either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))

-- | The web command.
web :: WebOpts -> Journal -> IO ()
web opts j = do
  d <- getCurrentDay
  let j' = filterJournalTransactions (queryFromOpts d $ reportopts_ $ cliopts_ opts) j
      p = port_ opts
      u = base_url_ opts
      staticRoot' = pack <$> static_root_ opts
  _ <- printf "Starting web app on port %d with base url %s\n" p u
  app <- makeApplication opts j' AppConfig{appEnv = Development
                                          ,appPort = p
                                          ,appRoot = pack u
                                          ,appHost = fromString "*4"
                                          ,appExtra = Extra "" Nothing staticRoot'
                                          }
  if server_ opts
   then do
    putStrLn "Press ctrl-c to quit"
    hFlush stdout
    runSettings (setPort p defaultSettings) app
   else do
    putStrLn "Starting web browser if possible"
    putStrLn "Web app will auto-exit after a few minutes with no browsers (or press ctrl-c)"
    hFlush stdout
    runUrlPort p "" app
