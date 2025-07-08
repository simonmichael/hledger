{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Web.Handler.ApiR where

import Data.Aeson
import Network.HTTP.Types.Status
import qualified Data.Yaml as Yaml
import qualified Data.ByteString as BS
import Data.FileEmbed (embedFile)

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Web.Import
import Hledger.Web.WebOptions

openApiYaml :: BS.ByteString
openApiYaml = $(embedFile "config/openapi-v1.yaml")

getOpenApiV1R :: Handler Value
getOpenApiV1R =
  case Yaml.decodeEither' openApiYaml of
    Left _ -> notFound
    Right openapi -> do
      addHeader "Content-Type" "application/json"
      return openapi

-- | GET /api/v1/accounts
getApiAccountsR :: Handler Value
getApiAccountsR = do
  VD{j} <- getViewData
  require ViewPermission
  let accts = journalAccountNames j
  returnJson accts

-- | GET /api/v1/accounts/{account_name}
getApiAccountR :: Text -> Handler Value
getApiAccountR acctName = do
  VD{j} <- getViewData
  require ViewPermission
  let accts = journalAccountNames j
  if acctName `elem` accts
    then returnJson $ object ["account" .= acctName, "exists" .= True]
    else sendResponseStatus status404 ("account not found" :: Text)

getSimpleAccountBalance :: Journal -> AccountName -> Value
getSimpleAccountBalance j acctName =
  let acctq = Acct $ accountNameToAccountRegex acctName
      rspec = defreportspec{_rsQuery = acctq}
      (balanceItems, _total) = balanceReport rspec j
  in case balanceItems of
       [] -> object
         [ "name" .= acctName
         , "balance" .= ("0" :: Text)
         , "commodity" .= ("" :: Text)
         , "depth" .= (0 :: Int)
         ]
       ((fullName, _, depth_, mixedAmt):_) ->
         let (commodity, amount) = case amounts mixedAmt of
               [] -> ("", "0")
               (amt:_) -> (acommodity amt, showAmount amt)
         in object
           [ "name" .= fullName
           , "balance" .= amount
           , "commodity" .= commodity
           , "depth" .= depth_
           ]

-- | GET /api/v1/accounts/{account_name}/balance
getApiAccountBalanceR :: Text -> Handler Value
getApiAccountBalanceR acctName = do
  VD{j} <- getViewData
  require ViewPermission
  returnJson $ getSimpleAccountBalance j acctName


-- | GET /api/v1/balance
getApiBalanceReportR :: Handler Value
getApiBalanceReportR = do
  VD{j, q, opts} <- getViewData
  require ViewPermission
  let rspec = (reportspec_ $ cliopts_ opts){_rsQuery = q}
      (balanceItems, total) = balanceReport rspec j
      styledItems = styleAmounts (journalCommodityStylesWith HardRounding j) balanceItems
      styledTotal = styleAmounts (journalCommodityStylesWith HardRounding j) total

      accountsJson = map itemToJson styledItems
      totalAmount = case amounts styledTotal of
        [] -> "0"
        (amt:_) -> showAmount amt

  returnJson $ object
    [ "accounts" .= accountsJson
    , "total" .= totalAmount
    ]
  where
    itemToJson (fullName, _, depth_, mixedAmt) =
      let (commodity, amount) = case amounts mixedAmt of
            [] -> ("", "0")
            (amt:_) -> (acommodity amt, showAmount amt)
      in object
        [ "name" .= fullName
        , "balance" .= amount
        , "commodity" .= commodity
        , "depth" .= depth_
        ]

-- | GET /api/v1/transactions
getApiTransactionsR :: Handler Value
getApiTransactionsR = do
  VD{j, q, opts} <- getViewData
  require ViewPermission
  let rspec = (reportspec_ $ cliopts_ opts){_rsQuery = q}
      items = entriesReport rspec j
      styledItems = styleAmounts (journalCommodityStylesWith HardRounding j) items
  returnJson $ map transactionToJson styledItems
  where
    transactionToJson txn =
      let postingsJson = map postingToJson (tpostings txn)
          status = case tstatus txn of
            -- don't use Show because it renders as !/*
            Unmarked -> "unmarked"
            Pending -> "pending"
            Cleared -> "cleared"
      in object
        [ "date" .= tdate txn
        , "description" .= tdescription txn
        , "postings" .= postingsJson
        , "status" .= (status :: String)
        ]

    postingToJson posting_ =
      let amount = case pamount posting_ of
            mixedAmt -> case amounts mixedAmt of
              [] -> "0"
              (amt:_) -> showAmount amt
      in object
        [ "account" .= paccount posting_
        , "amount" .= amount
        ]

-- | GET /api/v1/register
getApiRegisterReportR :: Handler Value
getApiRegisterReportR = do
  VD{j, q} <- getViewData
  require ViewPermission
  let transactions = jtxns j
      filteredTxns = filter (matchesTransaction q) transactions
      styledTxns = styleAmounts (journalCommodityStylesWith HardRounding j) filteredTxns

      (txnItems, finalBalance) = buildRegisterItems styledTxns

  returnJson $ object
    [ "final_balance" .= mixedAmountToJson finalBalance
    , "transactions" .= txnItems
    ]
  where
    buildRegisterItems txns =
      let (items, balance) = foldl' collectTransaction ([], nullmixedamt) txns
      in (reverse items, balance)

    collectTransaction (acc, runningBal) txn =
      let newBal = runningBal + sumPostings (tpostings txn)
          item = object
            [ "date" .= tdate txn
            , "description" .= tdescription txn
            , "postings" .= map postingToJson (tpostings txn)
            , "status" .= statusToText (tstatus txn)
            , "running_balance" .= mixedAmountToJson newBal
            ]
      in (item:acc, newBal)

    postingToJson p = object
      [ "account" .= paccount p
      , "amount" .= mixedAmountToJson (pamount p)
      ]

    mixedAmountToJson ma = toJSON $ map amountToJson $ amounts ma

    amountToJson amt = object $
      [ "commodity" .= acommodity amt
      , "quantity" .=
        showAmountWith defaultFmt{displayCost=False, displayCommodity=False} amt
      ] ++ costField
      where
        costField = case acost amt of
          Nothing -> []
          Just cost -> ["cost_basis" .= show cost]

    statusToText :: Hledger.Status -> String
    statusToText Unmarked = "unmarked"
    statusToText Pending = "pending"
    statusToText Cleared = "cleared"

-- let's not worry about adding/updating transactions yet. i'm not even sure how
-- that would work tbh, i'd need to look at how its implemented in the webapp
-- side
--
-- -- | POST /api/v1/transactions
-- postApiTransactionsR :: Handler Value
-- postApiTransactionsR = do
--   VD{j, opts} <- getViewData
--   require AddPermission
--   (r :: Result Transaction) <- parseCheckJsonBody
--   case r of
--     Error err -> sendResponseStatus status400 ("could not parse transaction: " <> T.pack err)
--     Success t -> do
--       result <- liftIO $ journalAddTransaction j (cliopts_ opts) t
--       case result of
--         Left err -> sendResponseStatus status400 (T.pack $ show err)
--         Right _ -> sendResponseStatus status201 ("transaction added" :: Text)
--
-- -- | GET /api/v1/transactions/{txn_id}
-- getApiTransactionR :: Text -> Handler Value
-- getApiTransactionR txnId = do
--   VD{j} <- getViewData
--   require ViewPermission
--   -- naive implementation - find by description match
--   let txns = jtxns j
--       matchingTxns = filter (\t -> T.isInfixOf txnId (tdescription t)) txns
--   case matchingTxns of
--     [t] -> returnJson t
--     [] -> sendResponseStatus status404 ("transaction not found" :: Text)
--     _ -> sendResponseStatus status400 ("ambiguous transaction id" :: Text)
--
-- -- | PUT /api/v1/transactions/{txn_id}
-- putApiTransactionR :: Text -> Handler Value
-- putApiTransactionR _ = do
--   require EditPermission
--   -- simplified - would need proper txn replacement logic
--   sendResponseStatus status501 ("transaction updates not implemented" :: Text)
--
-- -- | DELETE /api/v1/transactions/{txn_id}
-- deleteApiTransactionR :: Text -> Handler Value
-- deleteApiTransactionR _ = do
--   require EditPermission
--   sendResponseStatus status501 ("transaction deletion not implemented" :: Text)
--
-- -- | GET /api/v1/register
-- getApiRegisterReportR :: Handler Value
-- getApiRegisterReportR = do
--   VD{j, q, opts} <- getViewData
--   require ViewPermission
--   let rspec = (reportspec_ $ cliopts_ opts){_rsQuery = q}
--       transactions = jtxns j  -- get all transactions
--       filteredTxns = filter (matchesTransaction q) transactions
--       styledTxns = styleAmounts (journalCommodityStylesWith HardRounding j) filteredTxns
--
--       (txnItems, finalBalance) = buildRegisterItems styledTxns
--
--   returnJson $ object
--     [ "final_balance" .= showMixedAmount finalBalance
--     , "transactions" .= txnItems
--     ]
--   where
--     buildRegisterItems txns =
--       let (items, balance) = foldl' addTransaction ([], nullmixedamt) txns
--       in (reverse items, balance)
--
--     addTransaction (acc, runningBal) txn =
--       let newBal = runningBal + sumPostings (tpostings txn)
--           newBalStr = showMixedAmount newBal
--           item = object
--             [ "date" .= tdate txn
--             , "description" .= tdescription txn
--             , "postings" .= map postingToJson (tpostings txn)
--             , "status" .= statusToText (tstatus txn)
--             , "running_balance" .= newBalStr
--             ]
--       in (item:acc, newBal)
--
--     postingToJson p = object
--       [ "account" .= paccount p
--       , "amount" .= showMixedAmount (pamount p)
--       ]
--
--     statusToText :: Hledger.Status -> String
--     statusToText Unmarked = "unmarked"
--     statusToText Pending = "pending"
--     statusToText Cleared = "cleared"
--
-- do i want these? i never use them at the cli, and they are implemented in
-- Hledger.Cli anyway, not in the hledger-lib package
--
-- -- | GET /api/v1/income-statement
-- getApiIncomeStatementR :: Handler Value
-- getApiIncomeStatementR = do
--   VD{j, q, opts} <- getViewData
--   require ViewPermission
--   let rspec = (reportspec_ $ cliopts_ opts){_rsQuery = q}
--       is = incomeStatement rspec j
--   returnJson $
--     styleAmounts (journalCommodityStylesWith HardRounding j) is
--
-- -- | GET /api/v1/balance-sheet
-- getApiBalanceSheetR :: Handler Value
-- getApiBalanceSheetR = do
--   VD{j, q, opts} <- getViewData
--   require ViewPermission
--   let rspec = (reportspec_ $ cliopts_ opts){_rsQuery = q}
--       bs = balanceSheet rspec j
--   returnJson $
--     styleAmounts (journalCommodityStylesWith HardRounding j) bs
