{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Lens ((&), (.~), (?~))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Decimal
import qualified Data.Map as M
import           Data.Monoid
import           Data.Proxy
import           Data.Swagger
import           Data.Text hiding (map,reverse)
import           GHC.Generics
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import           Safe
import           Servant
import           Servant.Swagger
import           System.Console.Docopt
import           System.Environment (getArgs)
import           System.Exit
import           System.IO
import           Text.Printf

import Hledger.Query
import Hledger.Cli hiding (Reader, version)

version="0.27.98"

-- https://github.com/docopt/docopt.hs#readme
doc :: Docopt
doc = [docopt|
hledger-api 0.27.98

Serves hledger data and reports as a JSON web API.

Usage:
  hledger-api [options]
    start API server
  hledger-api --swagger
    print API docs in Swagger 2.0 format
  hledger-api --version
  hledger-api --help

Options:
  -f --file FILE  use a different input file
                  (default: $LEDGER_FILE or ~/.hledger.journal)
  -d --static-dir DIR  serve files from a different directory
                  (default: ./static/)
  -p --port PORT  use a different TCP port (default: 8001)
     --version    show version
  -h --help       show this help
|]

main :: IO ()
main = do
  args <- getArgs >>= parseArgsOrExit doc
  when (isPresent args (longOption "help")) $ exitWithUsage doc
  when (isPresent args (longOption "version")) $ putStrLn version >> exitSuccess
  when (isPresent args (longOption "swagger")) $ BL8.putStrLn (encode swaggerSpec) >> exitSuccess
  let defp = "8001"
  p <- case readMay $ getArgWithDefault args defp (longOption "port") of
        Nothing -> exitWithUsage doc
        Just n  -> return n
  deff <- defaultJournalPath
  let f = getArgWithDefault args deff (longOption "file")
  requireJournalFileExists f
  let
    defd = "static"
    d = getArgWithDefault args defd (longOption "static-dir")
  readJournalFile Nothing Nothing True f >>= either error' (serveApi p d f)

serveApi :: Int -> FilePath -> FilePath -> Journal -> IO ()
serveApi p d f j = do
  printf "Starting web api on port %d using files from %s for %s\nPress ctrl-c to quit\n" p d f
  Warp.run p $ hledgerApiApp d j

type HledgerApi =
       "accountnames" :> Get '[JSON] [AccountName]
  :<|> "transactions" :> Get '[JSON] [Transaction]
  :<|> "prices"       :> Get '[JSON] [MarketPrice]
  :<|> "commodities"  :> Get '[JSON] [Commodity]
  :<|> "accounts"     :> Get '[JSON] [Account]
  :<|> "accounttransactions" :> Capture "acct" AccountName :> Get '[JSON] AccountTransactionsReport
  :<|> Raw

type HledgerApi' = ("swagger.json" :> Get '[JSON] Swagger) :<|> HledgerApi

hledgerApiApp :: FilePath -> Journal -> Wai.Application
hledgerApiApp staticdir j =
  Servant.serve (Proxy :: Proxy HledgerApi') (return swaggerSpec :<|> server)
  where
    api :: Proxy HledgerApi
    api = Proxy

    server :: Server HledgerApi
    server =
           accountnamesH
      :<|> transactionsH
      :<|> pricesH
      :<|> commoditiesH
      :<|> accountsH
      :<|> accounttransactionsH
      :<|> serveDirectory staticdir
      where
        accountnamesH = return $ journalAccountNames j
        transactionsH = return $ jtxns j
        pricesH       = return $ jmarketprices j
        commoditiesH  = return $ (M.keys . jcommoditystyles) j
        accountsH     = return $ laccounts $ ledgerFromJournal Hledger.Query.Any j
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

instance ToJSON ClearedStatus where toJSON = genericToJSON defaultOptions -- avoiding https://github.com/bos/aeson/issues/290
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

-- swagger api doc

swaggerSpec :: Swagger
swaggerSpec = toSwagger (Proxy :: Proxy HledgerApi)
  & info.infoTitle   .~ "hledger API"
  & info.infoVersion .~ "0.0.0.1"
  & info.infoDescription ?~ "This is the API provided by hledger-api for reading hledger data"
  & info.infoLicense ?~ License "GPLv3+" (Nothing)

instance ToSchema ClearedStatus
instance ToSchema GenericSourcePos
instance ToSchema Decimal
 where
  declareNamedSchema _proxy = pure (Just "Decimal", schema)
   where
     schema = mempty
       & schemaType .~ SwaggerNumber
       & schemaExample ?~ toJSON (100 :: Decimal)
instance ToSchema Amount
instance ToSchema AmountStyle
instance ToSchema Side
instance ToSchema DigitGroupStyle
instance ToSchema MixedAmount
instance ToSchema Price
instance ToSchema MarketPrice
instance ToSchema PostingType
instance ToSchema Posting
instance ToSchema Transaction
instance ToSchema Account
-- instance ToSchema AccountTransactionsReport
