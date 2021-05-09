{-
JSON instances. Should they be in Types.hs ?
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

--{-# LANGUAGE DataKinds           #-}
--{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
--{-# LANGUAGE NamedFieldPuns #-}
--{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE PolyKinds           #-}
--{-# LANGUAGE QuasiQuotes         #-}
--{-# LANGUAGE QuasiQuotes #-}
--{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
--{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
--{-# LANGUAGE TemplateHaskell       #-}
--{-# LANGUAGE TypeFamilies        #-}
--{-# LANGUAGE TypeOperators       #-}

module Hledger.Data.Json (
  -- * Instances
  -- * Utilities
   toJsonText
  ,writeJsonFile
  ,readJsonFile
) where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
--import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import           Data.Decimal (DecimalRaw(..), roundTo)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy    as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TB
import           GHC.Generics (Generic)
import           System.Time (ClockTime)

import           Hledger.Data.Types
import           Hledger.Data.Amount (amountsRaw, mixed)

-- To JSON

instance ToJSON Status
instance ToJSON GenericSourcePos

-- https://github.com/simonmichael/hledger/issues/1195

-- The default JSON output for Decimal can contain 255-digit integers
-- (for repeating decimals caused by implicit transaction prices).
-- JSON output is intended to be consumed by diverse apps and
-- programming languages, which can't handle numbers like that.
-- From #1195:
--
-- > - JavaScript uses 64-bit IEEE754 numbers which can only accurately
-- >   represent integers up to 9007199254740991 (i.e. a maximum of 15 digits).
-- > - Java’s largest integers are limited to 18 digits.
-- > - Python 3 integers are unbounded.
-- > - Python 2 integers are limited to 18 digits like Java.
-- > - C and C++ number limits depend on platform — most platforms should
-- >   be able to represent unsigned integers up to 64 bits, i.e. 19 digits.
--
-- What is the best compromise for both accuracy and practicality ?
-- For now, we provide both the maximum precision representation
-- (decimalPlaces & decimalMantissa), and a floating point representation
-- with up to 10 decimal places (and an unbounded number of integer digits).
-- We hope the mere presence of the large number in JSON won't break things,
-- and that the overall number of significant digits in the floating point
-- remains manageable in practice. (I'm not sure how to limit the number
-- of significant digits in a Decimal right now.)
instance (Integral a, ToJSON a) => ToJSON (DecimalRaw a) where
  toJSON = object . decimalKV
  toEncoding = pairs . mconcat . decimalKV

decimalKV :: (KeyValue kv, Integral a, ToJSON a) => DecimalRaw a -> [kv]
decimalKV d = let d' = if decimalPlaces d <= 10 then d else roundTo 10 d in
    [ "decimalPlaces"   .= decimalPlaces d'
    , "decimalMantissa" .= decimalMantissa d'
    , "floatingPoint"   .= (realToFrac d' :: Double)
    ]

instance ToJSON Amount
instance ToJSON AmountStyle

-- Use the same JSON serialisation as Maybe Word8
instance ToJSON AmountPrecision where
  toJSON = toJSON . \case
    Precision n      -> Just n
    NaturalPrecision -> Nothing
  toEncoding = toEncoding . \case
    Precision n      -> Just n
    NaturalPrecision -> Nothing

instance ToJSON Side
instance ToJSON DigitGroupStyle

instance ToJSON MixedAmount where
  toJSON = toJSON . amountsRaw
  toEncoding = toEncoding . amountsRaw

instance ToJSON BalanceAssertion
instance ToJSON AmountPrice
instance ToJSON MarketPrice
instance ToJSON PostingType

instance ToJSON Posting where
  toJSON = object . postingKV
  toEncoding = pairs . mconcat . postingKV

postingKV :: KeyValue kv => Posting -> [kv]
postingKV Posting{..} =
    [ "pdate"             .= pdate
    , "pdate2"            .= pdate2
    , "pstatus"           .= pstatus
    , "paccount"          .= paccount
    , "pamount"           .= pamount
    , "pcomment"          .= pcomment
    , "ptype"             .= ptype
    , "ptags"             .= ptags
    , "pbalanceassertion" .= pbalanceassertion
    -- To avoid a cycle, show just the parent transaction's index number
    -- in a dummy field. When re-parsed, there will be no parent.
    , "ptransaction_"     .= maybe "" (show.tindex) ptransaction
    -- This is probably not wanted in json, we discard it.
    , "poriginal"         .= (Nothing :: Maybe Posting)
    ]

instance ToJSON Transaction
instance ToJSON TransactionModifier
instance ToJSON PeriodicTransaction
instance ToJSON PriceDirective
instance ToJSON DateSpan
instance ToJSON Interval
instance ToJSON AccountAlias
instance ToJSON AccountType
instance ToJSONKey AccountType
instance ToJSON AccountDeclarationInfo
instance ToJSON PayeeDeclarationInfo
instance ToJSON Commodity
instance ToJSON TimeclockCode
instance ToJSON TimeclockEntry
instance ToJSON ClockTime
instance ToJSON Journal

instance ToJSON Account where
  toJSON = object . accountKV
  toEncoding = pairs . mconcat . accountKV

accountKV :: KeyValue kv => Account -> [kv]
accountKV a =
    [ "aname"        .= aname a
    , "aebalance"    .= aebalance a
    , "aibalance"    .= aibalance a
    , "anumpostings" .= anumpostings a
    , "aboring"      .= aboring a
    -- To avoid a cycle, show just the parent account's name
    -- in a dummy field. When re-parsed, there will be no parent.
    , "aparent_"     .= maybe "" aname (aparent a)
    -- Just the names of subaccounts, as a dummy field, ignored when parsed.
    , "asubs_"       .= map aname (asubs a)
    -- The actual subaccounts (and their subs..), making a (probably highly redundant) tree
    -- ,"asubs"        .= asubs a
    -- Omit the actual subaccounts
    , "asubs"        .= ([]::[Account])
    ]

deriving instance Generic (Ledger)
instance ToJSON Ledger

-- From JSON

instance FromJSON Status
instance FromJSON GenericSourcePos
instance FromJSON Amount
instance FromJSON AmountStyle

-- Use the same JSON serialisation as Maybe Word8
instance FromJSON AmountPrecision where
  parseJSON = fmap (maybe NaturalPrecision Precision) . parseJSON

instance FromJSON Side
instance FromJSON DigitGroupStyle

instance FromJSON MixedAmount where
  parseJSON = fmap (mixed :: [Amount] -> MixedAmount) . parseJSON

instance FromJSON BalanceAssertion
instance FromJSON AmountPrice
instance FromJSON MarketPrice
instance FromJSON PostingType
instance FromJSON Posting
instance FromJSON Transaction
instance FromJSON AccountDeclarationInfo
-- XXX The ToJSON instance replaces subaccounts with just names.
-- Here we should try to make use of those to reconstruct the
-- parent-child relationships.
instance FromJSON Account

-- Decimal, various attempts
--
-- https://stackoverflow.com/questions/40331851/haskell-data-decimal-as-aeson-type
----instance FromJSON Decimal where parseJSON =
----  A.withScientific "Decimal" (return . right . eitherFromRational . toRational)
--
-- https://github.com/bos/aeson/issues/474
-- http://hackage.haskell.org/package/aeson-1.4.2.0/docs/Data-Aeson-TH.html
-- $(deriveFromJSON defaultOptions ''Decimal) -- doesn't work
-- $(deriveFromJSON defaultOptions ''DecimalRaw)  -- works; requires TH, but gives better parse error messages
--
-- https://github.com/PaulJohnson/Haskell-Decimal/issues/6
--deriving instance Generic Decimal
--instance FromJSON Decimal
deriving instance Generic (DecimalRaw a)
instance FromJSON (DecimalRaw Integer)
--
-- @simonmichael, I think the code in your first comment should work if it compiles—though “work” doesn’t mean you can parse a JSON number directly into a `Decimal` using the generic instance, as you’ve discovered.
--
--Error messages with these extensions are always rather cryptic, but I’d prefer them to Template Haskell. Typically you’ll want to start by getting a generic `ToJSON` instance working, then use that to figure out what the `FromJSON` instance expects to parse: for a correct instance, `encode` and `decode` should give you an isomorphism between your type and a subset of `Bytestring` (up to the `Maybe` wrapper that `decode` returns).
--
--I don’t have time to test it right now, but I think it will also work without `DeriveAnyClass`, just using `DeriveGeneric` and `StandAloneDeriving`. It should also work to use the [`genericParseJSON`](http://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#v:genericParseJSON) function to implement the class explicitly, something like this:
--
--{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE StandAloneDeriving #-}
--import GHC.Generics
--import Data.Aeson
--deriving instance Generic Decimal
--instance FromJSON Decimal where
--  parseJSON = genericParseJSON defaultOptions
--
--And of course you can avoid `StandAloneDeriving` entirely if you’re willing to wrap `Decimal` in your own `newtype`.

-- XXX these will allow reading a Journal, but currently the
-- jdeclaredaccounttypes Map gets serialised as a JSON list, which
-- can't be read back.
--
-- instance FromJSON AccountAlias
-- instance FromJSONKey AccountType where fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
-- instance FromJSON AccountType
-- instance FromJSON ClockTime
-- instance FromJSON Commodity
-- instance FromJSON DateSpan
-- instance FromJSON Interval
-- instance FromJSON PeriodicTransaction
-- instance FromJSON PriceDirective
-- instance FromJSON TimeclockCode
-- instance FromJSON TimeclockEntry
-- instance FromJSON TransactionModifier
-- instance FromJSON Journal


-- Utilities

-- | Show a JSON-convertible haskell value as pretty-printed JSON text.
toJsonText :: ToJSON a => a -> TL.Text
toJsonText = TB.toLazyText . (<> TB.fromText "\n") . encodePrettyToTextBuilder

-- | Write a JSON-convertible haskell value to a pretty-printed JSON file.
-- Eg: writeJsonFile "a.json" nulltransaction
writeJsonFile :: ToJSON a => FilePath -> a -> IO ()
writeJsonFile f = TL.writeFile f . toJsonText
-- we write with Text and read with ByteString, is that fine ?

-- | Read a JSON file and decode it to the target type, or raise an error if we can't.
-- Eg: readJsonFile "a.json" :: IO Transaction
readJsonFile :: FromJSON a => FilePath -> IO a
readJsonFile f = do
  bl <- BL.readFile f
  -- PARTIAL:
  let v = fromMaybe (error $ "could not decode JSON in "++show f++" to target value")
          (decode bl :: Maybe Value)
  case fromJSON v :: FromJSON a => Result a of
    Error e   -> error e
    Success t -> return t

