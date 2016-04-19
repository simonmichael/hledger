{-# LANGUAGE CPP, OverloadedStrings #-}
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
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Monad (when)
import Data.Text (pack)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Printf
import Prelude hiding (putStrLn)

import Hledger
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Cli hiding (progname,prognameandversion)
import Hledger.Web.WebOptions


hledgerWebMain :: IO ()
hledgerWebMain = do
  opts <- getHledgerWebOpts
  when (debug_ (cliopts_ opts) > 0) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
  runWith opts

runWith :: WebOpts -> IO ()
runWith opts
  | "h"               `inRawOpts` (rawopts_ $ cliopts_ opts) = putStr (showModeUsage webmode) >> exitSuccess
  | "help"            `inRawOpts` (rawopts_ $ cliopts_ opts) = printHelpForTopic (topicForMode webmode) >> exitSuccess
  | "man"             `inRawOpts` (rawopts_ $ cliopts_ opts) = runManForTopic (topicForMode webmode) >> exitSuccess
  | "info"            `inRawOpts` (rawopts_ $ cliopts_ opts) = runInfoForTopic (topicForMode webmode) >> exitSuccess
  | "version"         `inRawOpts` (rawopts_ $ cliopts_ opts) = putStrLn prognameandversion >> exitSuccess
  | "binary-filename" `inRawOpts` (rawopts_ $ cliopts_ opts) = putStrLn (binaryfilename progname)
  | otherwise = do
    requireJournalFileExists =<< (head `fmap` journalFilePathFromOpts (cliopts_ opts)) -- XXX head should be safe for now
    withJournalDo' opts web

withJournalDo' :: WebOpts -> (WebOpts -> Journal -> IO ()) -> IO ()
withJournalDo' opts cmd = do
  f <- head `fmap` journalFilePathFromOpts (cliopts_ opts) -- XXX head should be safe for now

  -- https://github.com/simonmichael/hledger/issues/202
  -- -f- gives [Error#yesod-core] <stdin>: hGetContents: illegal operation (handle is closed) for some reason
  -- Also we may be writing to this file. Just disallow it.
  when (f == "-") $ error' "hledger-web doesn't support -f -, please specify a file path"

  readJournalFile Nothing Nothing True f >>=
   either error' (cmd opts . journalApplyAliases (aliasesFromOpts $ cliopts_ opts))

-- | The web command.
web :: WebOpts -> Journal -> IO ()
web opts j = do
  d <- getCurrentDay
  let j' = filterJournalTransactions (queryFromOpts d $ reportopts_ $ cliopts_ opts) j
      p = port_ opts
      u = base_url_ opts
      staticRoot = pack <$> file_url_ opts
  _ <- printf "Starting web app on port %d with base url %s\n" p u
  app <- makeApplication opts j' AppConfig{appEnv = Development
                                          ,appPort = p
                                          ,appRoot = pack u
                                          ,appHost = fromString "*4"
                                          ,appExtra = Extra "" Nothing staticRoot
                                          }
  if server_ opts
   then do
    putStrLn "Press ctrl-c to quit"
    hFlush stdout
    Network.Wai.Handler.Warp.runSettings (setPort p defaultSettings) app
   else do
    putStrLn "Starting web browser if possible"
    putStrLn "Web app will auto-exit after a few minutes with no browsers (or press ctrl-c)"
    hFlush stdout
    Network.Wai.Handler.Launch.runUrlPort p "" app
