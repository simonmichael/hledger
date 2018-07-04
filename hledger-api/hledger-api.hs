{-# LANGUAGE CPP                 #-}
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

import           Lens.Micro ((&), (.~))
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Decimal
import           Data.Default
import qualified Data.Map as M
import           Data.Proxy
import           Data.String (fromString)
import           Data.Swagger
import           Data.Text hiding (map,reverse)
import           Network.Wai as Wai
import           Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger
import           Safe
import           Servant
import           Servant.Swagger
import           System.Console.Docopt
import           System.Environment (getArgs)
import           System.Exit
import           Text.Printf

import Hledger.Query
import Hledger.Cli hiding (Reader, version)

hledgerApiVersion="1.10.99"

-- https://github.com/docopt/docopt.hs#readme
doc :: Docopt
doc = [docopt|
hledger-api 1.10.99

Serves hledger data and reports as a JSON web API.

Usage:
  hledger-api [options]
    start API server
  hledger-api --swagger
    print API docs in Swagger 2.0 format
  hledger-api --version
  hledger-api -h|--help

Options:
  -f --file FILE   use a different input file
                   (default: $LEDGER_FILE or ~/.hledger.journal)
  -d --static-dir  DIR  serve files from a different directory
                   (default: .)
     --host IPADDR listen on this IP address (default: 127.0.0.1)
  -p --port PORT   listen on this TCP port (default: 8001)
     --version     show version
  -h --help        show usage
|]

swaggerSpec :: Swagger
swaggerSpec = toSwagger (Proxy :: Proxy HledgerApi)
  & info.title       .~ "hledger API"
  & info.version     .~ pack hledgerApiVersion
  & info.description .~ Just "This is the API provided by hledger-api for reading hledger data"
  & info.license     .~ Just (License "GPLv3+" (Nothing))

main :: IO ()
main = do
  args <- getArgs >>= parseArgsOrExit doc
  when (isPresent args (shortOption 'h') || isPresent args (longOption "help")) $ exitWithUsage doc
  when (isPresent args (longOption "version")) $ putStrLn ("hledger-api " ++ hledgerApiVersion) >> exitSuccess
  when (isPresent args (longOption "swagger")) $ BL8.putStrLn (encode swaggerSpec) >> exitSuccess
  let
    defh = "127.0.0.1"
    h = getArgWithDefault args defh (longOption "host")
    defp = "8001"
  p <- case readMay $ getArgWithDefault args defp (longOption "port") of
        Nothing -> exitWithUsage doc
        Just n  -> return n
  deff <- defaultJournalPath
  let f = getArgWithDefault args deff (longOption "file")
  requireJournalFileExists f
  let
    defd = "."
    d = getArgWithDefault args defd (longOption "static-dir")
  readJournalFile def f >>= either error' (serveApi h p d f)

serveApi :: String -> Int -> FilePath -> FilePath -> Journal -> IO ()
serveApi h p d f j = do
  printf "Starting web api http://%s:%d/api/v1 for %s\n" h p f
  printf "and file server  http://%s:%d        for %s/\n" h p d
  printf "Press ctrl-c to quit\n"
  let warpsettings = defaultSettings
        & setHost (fromString h)
        & setPort p
  Warp.runSettings warpsettings $
    logStdout $
    hledgerApiApp d j

type HledgerApi =
  "api" :> "v1" :>
    (
         "accountnames" :> Get '[JSON] [AccountName]
    :<|> "transactions" :> Get '[JSON] [Transaction]
    :<|> "prices"       :> Get '[JSON] [MarketPrice]
    :<|> "commodities"  :> Get '[JSON] [CommoditySymbol]
    :<|> "accounts"     :> Get '[JSON] [Account]
    :<|> "accounts"     :> Capture "acct" AccountName :> Get '[JSON] AccountTransactionsReport
    )

type HledgerSwaggerApi =
       "swagger.json" :> Get '[JSON] Swagger
  :<|> HledgerApi

type HledgerSwaggerFilesApi =
       HledgerSwaggerApi
  :<|> Raw

hledgerApiApp :: FilePath -> Journal -> Wai.Application
hledgerApiApp staticdir j = Servant.serve api server
  where
    api :: Proxy HledgerSwaggerFilesApi
    api = Proxy

    server :: Server HledgerSwaggerFilesApi
    server =
      (
           return swaggerSpec
      --
      :<|> accountnamesH
      :<|> transactionsH
      :<|> pricesH
      :<|> commoditiesH
      :<|> accountsH
      :<|> accounttransactionsH
      )
      --
      :<|> serveDirectoryFileServer staticdir
      where
        accountnamesH = return $ journalAccountNames j
        transactionsH = return $ jtxns j
        pricesH       = return $ jmarketprices j
        commoditiesH  = return $ (M.keys . jinferredcommodities) j
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

instance ToJSON Status where toJSON = genericToJSON defaultOptions -- avoiding https://github.com/bos/aeson/issues/290
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
instance ToSchema Status
instance ToSchema GenericSourcePos
instance ToSchema Decimal
 where
  declareNamedSchema _proxy = pure $ NamedSchema (Just "Decimal") schema
   where
     schema = mempty
       & type_   .~ SwaggerNumber
       & example .~ Just (toJSON (100 :: Decimal))
instance ToSchema Amount
instance ToSchema AmountStyle
instance ToSchema Side
instance ToSchema DigitGroupStyle
instance ToSchema MixedAmount
instance ToSchema Price
#if MIN_VERSION_swagger2(2,1,5)
  where declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
#endif
instance ToSchema MarketPrice
instance ToSchema PostingType
instance ToSchema Posting
instance ToSchema Transaction
instance ToSchema Account
-- instance ToSchema AccountTransactionsReport
