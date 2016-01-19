{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Decimal
import qualified Data.Map as M
import           Data.Monoid
import           Data.Proxy
import           Data.Text hiding (map,reverse)
import           GHC.Generics
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import           Safe
import           Servant
import           System.Console.Docopt
import           System.Environment (getArgs)
import           System.Exit
import           System.IO
import           Text.Printf

import Hledger.Query
import Hledger.Cli hiding (Reader, version)

version="0.27.98"

doc :: Docopt
doc = [docopt|
hledger-api 0.27.98

Usage:
  hledger-api [options]
  hledger-api --version
  hledger-api --help

Options:
  -f --file FILE  use a different input file
                  (default: $LEDGER_FILE or ~/.hledger.journal)
  -p --port PORT  use a different TCP port (default: 8001)
     --version    show version
  -h --help       show this help
|]

main :: IO ()
main = do
  args <- getArgs >>= parseArgsOrExit doc
  when (isPresent args (longOption "help")) $ exitWithUsage doc
  when (isPresent args (longOption "version")) $ putStrLn version >> exitSuccess
  let defp = "8001"
  p <- case readMay $ getArgWithDefault args defp (longOption "port") of
        Nothing -> exitWithUsage doc
        Just n  -> return n
  deff <- defaultJournalPath
  let f = getArgWithDefault args deff (longOption "file")
  requireJournalFileExists f
  readJournalFile Nothing Nothing True f >>= either error' (serveApi f p)

serveApi :: FilePath -> Int -> Journal -> IO ()
serveApi f p j = do
  printf "Starting web api for %s on port %d\nPress ctrl-c to quit\n" f p
  Warp.run p $ hledgerApiApp j

type HledgerApi =
       "accountnames" :> Get '[JSON] [AccountName]
  :<|> "transactions" :> Get '[JSON] [Transaction]
  :<|> "prices"       :> Get '[JSON] [MarketPrice]
  :<|> "commodities"  :> Get '[JSON] [Commodity]
  :<|> "accounts"     :> Get '[JSON] [Account]
  :<|> "accounts"     :> Capture "acct" AccountName :> Get '[JSON] AccountTransactionsReport

type CombinedServer =
       "api" :>
         "v1" :> HledgerApi
  :<|> Raw

hledgerApiApp :: Journal -> Wai.Application
hledgerApiApp j = Servant.serve api combinedServer
  where
    api :: Proxy CombinedServer
    api = Proxy

    apiServer :: Server HledgerApi
    apiServer =
           accountnamesH
      :<|> transactionsH
      :<|> pricesH
      :<|> commoditiesH
      :<|> accountsH
      :<|> accounttransactionsH
      where
        accountnamesH = return $ journalAccountNames j
        transactionsH = return $ jtxns j
        pricesH       = return $ jmarketprices j
        commoditiesH  = return $ (M.keys . jcommoditystyles) j
        accountsH     = return $ ledgerTopAccounts $ ledgerFromJournal Hledger.Query.Any j
        accounttransactionsH (a::AccountName) = do
          -- d <- liftIO getCurrentDay
          let
            ropts = defreportopts
            -- ropts' = ropts {depth_=Nothing
            --                ,balancetype_=HistoricalBalance
            --                }
            q = Hledger.Query.Any --filterQuery (not . queryIsDepth) $ queryFromOpts d ropts'
            thisacctq = Acct $ accountNameToAccountRegex a -- includes subs
          return $ accountTransactionsReport ropts j q thisacctq

    combinedServer :: Server CombinedServer
    combinedServer = apiServer
        :<|> serveDirectory "static"

-- brief toJSON definitions included to avoid https://github.com/bos/aeson/issues/290
-- use toEncoding = genericToEncoding defaultOptions instead ?
instance ToJSON ClearedStatus where toJSON = genericToJSON defaultOptions
instance ToJSON GenericSourcePos where toJSON = genericToJSON defaultOptions
instance ToJSON Decimal where
  toJSON = toJSON . show
instance ToJSON Amount where toJSON = genericToJSON defaultOptions
instance ToJSON AmountStyle where toJSON = genericToJSON defaultOptions
instance ToJSON Side where toJSON = genericToJSON defaultOptions
instance ToJSON DigitGroupStyle where toJSON = genericToJSON defaultOptions
instance ToJSON MixedAmount where toJSON = genericToJSON defaultOptions
instance ToJSON Price where toJSON = genericToJSON defaultOptions
instance ToJSON MarketPrice where toJSON = genericToJSON defaultOptions
instance ToJSON PostingType where toJSON = genericToJSON defaultOptions
instance ToJSON Posting where
  toJSON Posting{..} =
    object
    ["pdate"             .= toJSON pdate
    ,"pdate2"            .= toJSON pdate2
    ,"pstatus"           .= toJSON pstatus
    ,"paccount"          .= toJSON paccount
    ,"pamount"           .= toJSON pamount
    ,"pcomment"          .= toJSON pcomment
    ,"ptype"             .= toJSON ptype
    ,"ptags"             .= toJSON ptags
    ,"pbalanceassertion" .= toJSON pbalanceassertion
    ,"ptransactionidx"   .= toJSON (maybe "" (show.tindex) ptransaction)
    ]
instance ToJSON Transaction where toJSON = genericToJSON defaultOptions
instance ToJSON Account where
  toJSON a =
    object
    ["aname"        .= toJSON (aname a)
    ,"aebalance"    .= toJSON (aebalance a)
    ,"aibalance"    .= toJSON (aibalance a)
    ,"anumpostings" .= toJSON (anumpostings a)
    ,"aboring"      .= toJSON (aboring a)
    ,"aparentname"  .= toJSON (maybe "" aname $ aparent a)
    ,"asubs"        .= toJSON (map toJSON $ asubs a)
    ]
instance ToJSON AccountTransactionsReport where toJSON = genericToJSON defaultOptions
