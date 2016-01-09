{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Monad
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import           Servant
import           System.IO
import           Text.Printf

-- import Hledger hiding (Reader)
import Hledger.Cli hiding (Reader)

-- boilerplate:
hledgerApiApp :: Journal -> Wai.Application
hledgerApiApp j = Servant.serve hledgerApi (hledgerApiServer j)

hledgerApiServer :: Journal -> Servant.Server HledgerApi
hledgerApiServer j = enter readerToEither hledgerServerT
  where
    readerToEither :: Reader Journal :~> EitherT ServantErr IO
    readerToEither = Nat $ \r -> return (runReader r j)

hledgerApi :: Proxy HledgerApi
hledgerApi = Proxy
--

type HledgerApi =
  "accounts" :> Get '[JSON] [AccountName]

hledgerServerT :: ServerT HledgerApi (Reader Journal)
hledgerServerT =
  accountsH
  where
    accountsH = do
      j <- ask
      return $ journalAccountNames j

main :: IO ()
main = do

--   opts <- getHledgerWebOpts
--   when (debug_ (cliopts_ opts) > 0) $ printf "%s\n" prognameandversion >> printf "opts: %s\n" (show opts)
--   runWith opts

  f <- defaultJournalPath

-- runWith :: WebOpts -> IO ()
-- runWith opts
--   | "help" `inRawOpts` (rawopts_ $ cliopts_ opts)            = putStr (showModeHelp webmode) >> exitSuccess
--   | "version" `inRawOpts` (rawopts_ $ cliopts_ opts)         = putStrLn prognameandversion >> exitSuccess
--   | otherwise = do
--     requireJournalFileExists =<< (head `fmap` journalFilePathFromOpts (cliopts_ opts)) -- XXX head should be safe for now
  requireJournalFileExists f
--     withJournalDo' opts web

-- withJournalDo' :: WebOpts -> (WebOpts -> Journal -> IO ()) -> IO ()
-- withJournalDo' opts cmd = do
--   f <- head `fmap` journalFilePathFromOpts (cliopts_ opts) -- XXX head should be safe for now

  -- https://github.com/simonmichael/hledger/issues/202
  -- -f- gives [Error#yesod-core] <stdin>: hGetContents: illegal operation (handle is closed) for some reason
  -- Also we may be writing to this file. Just disallow it.
  when (f == "-") $ error' "-f -, please specify a file path"

  let opts = defcliopts
  readJournalFile Nothing Nothing True f >>=
    either error' (go opts . journalApplyAliases (aliasesFromOpts opts))

go :: CliOpts -> Journal -> IO ()
go opts j = do
  d <- getCurrentDay
  let j' = filterJournalTransactions (queryFromOpts d $ reportopts_ opts) j
      p = 8001 :: Int
  _ <- printf "Starting web api on port %d\n" p
  putStrLn "Press ctrl-c to quit"
  hFlush stdout

  Warp.run 8001 $ hledgerApiApp j'
