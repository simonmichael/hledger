{-# LANGUAGE CPP, RecordWildCards, NamedFieldPuns, NoMonoLocalBinds, ScopedTypeVariables, FlexibleContexts, TupleSections, OverloadedStrings #-}

module Hledger.Read.LedgerReader (
--- * exports

  -- * Reader
  reader

  -- * Tests
  ,tests_Hledger_Read_LedgerReader

)
where
--- * imports
import Prelude ()
import Prelude.Compat hiding (readFile)
-- import qualified Control.Exception as C
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT(..), throwError)
-- import Control.Monad.State.Strict
-- import qualified Data.Map.Strict as M
import Data.Maybe
-- import Data.List
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
-- import qualified Data.Text as T
-- import Data.Time.Calendar
-- import Data.Time.LocalTime
-- import Safe
import Test.HUnit
-- #ifdef TESTS
-- import Test.Framework
-- import Text.Megaparsec.Error
-- #endif
import Text.Megaparsec (eof)
-- import Text.Printf
import System.FilePath
import System.Time
import qualified Filesystem.Path.CurrentOS as F

import Hledger.Data
import Hledger.Read.Common
import Hledger.Utils
import Ledger.Parser.Text
import Text.Trifecta.Result (Result(..))

-- $setup
-- >>> :set -XOverloadedStrings

--- * reader

reader :: Reader
reader = Reader format detect parse

format :: String
format = "ledger"

-- | Does the given file path and data look like something this reader can handle ?
detect :: FilePath -> Text -> Bool
detect f _
  -- file name known: try this reader if it has any of these extensions
  | f /= "-"  = takeExtension f `elem` ['.':format, ".l"]
  -- file name unknown: don't try this reader
  | otherwise = False

-- | Parse and post-process a "Journal" from ledger's journal format, or give an error.
parse :: Maybe FilePath -> Bool -> FilePath -> Text -> ExceptT String IO Journal
parse _mrulespath assrt path txt = do
  let
    path' = F.decodeString path -- XXX can fail, according to docs
    bs = encodeUtf8 txt
    r = parseJournalFile path' bs
  case r of
    Failure ei -> throwError $ show ei
    Success res -> do
      -- dbg7IO "raw entities" res
      ejus <- liftIO $ sequence <$> mapM rawEntityInSituToJournalUpdate res
      case ejus of
        Left err  -> throwError err
        Right jus -> do
          let pj = foldr (flip (.)) id jus nulljournal
          t <- liftIO getClockTime
          either throwError return $ journalFinalise t path txt assrt pj

-- (I'm not too fond of journal update functions, but ok..)
-- | Convert a ledger4 RawEntityInSitu - representing a parsed transaction,
-- directive, comment etc. - into either a journal update function or an error.
-- Currently converts only transactions, and ignores some transaction fields.
-- Runs in IO because it uses some hledger parsers that have some need for that.
rawEntityInSituToJournalUpdate :: RawEntityInSitu -> IO (Either String (ParsedJournal -> ParsedJournal))
rawEntityInSituToJournalUpdate RawEntityInSitu{rawEntity=RawTransactionEntity (rt@RawTransaction{
    rawTxnDate    = date     -- :: !String
  , rawTxnDateAux = mdate2   -- :: Maybe String
  , rawTxnState   = _mstatus -- :: Maybe Char
  , rawTxnCode    = _mcode   -- :: Maybe String
  , rawTxnDesc    = desc     -- :: !String
  , rawTxnNote    = _mnote   -- :: Maybe String
  , rawTxnPosts   = rps      -- :: ![RawPosting]
  })}
  = do
    let md = parsedateM date
        md2 = mdate2 >>= parsedateM
        dateerr = return . Left . ("could not parse date "++)
    case (md, mdate2, md2) of
      (Nothing, _, _)          -> dateerr date
      (_, Just date2, Nothing) -> dateerr date2
      (Just d, _, _)           -> do
        eps <- sequence . catMaybes <$> mapM rawPostingToPosting rps
        case eps of
          Left err -> return $ Left err
          Right ps -> do
            let t = nulltransaction{
              -- XXX TODO more complete transaction parsing
              -- tindex                   -- :: Integer,   -- ^ this transaction's 1-based position in the input stream, or 0 when not available
              -- tsourcepos               -- :: GenericSourcePos,
                tdate = d                 -- :: Day
              , tdate2 = md2              -- :: Maybe Day
              -- tstatus                  -- :: ClearedStatus,
              -- tcode                    -- :: Text,
              , tdescription = pack desc  -- :: Text,
              -- tcomment                 -- :: Text,      -- ^ this transaction's comment lines, as a single non-indented multi-line string
              -- ttags                    -- :: [Tag],     -- ^ tag names and values, extracted from the comment
              , tpostings = ps            -- :: [Posting], -- ^ this transaction's postings
              -- tpreceding_comment_lines -- :: Text       -- ^ any comment lines immediately preceding this transaction
              }
            dbg7IO "raw transaction" rt
            dbg7IO "cooked transaction" t
            return $ Right $ addTransaction t
-- TODO convert other entities
rawEntityInSituToJournalUpdate _ = return $ Right id

-- | Convert a ledger4 RawPosting to a hledger Posting or an error message.
-- Currently ignores some posting fields, and the RawPostingNote variant
-- (which represents a comment line, not a posting; returns Nothing for these).
rawPostingToPosting :: RawPosting -> IO (Maybe (Either String Posting))
rawPostingToPosting RawPosting{
    -- TODO
    rawPostState   = _mstatus -- :: Maybe Char
  , rawPostAccount = acct     -- :: !String
  , rawPostAmount  = mamtstr  -- :: Maybe String
  , rawPostNote    = _mnote   -- :: Maybe String
  } = do
    eamt <- runErroringJournalParser (spaceandamountormissingp <* eof) $ pack $ maybe "" (' ':) mamtstr
    case eamt of
      Left err -> return $ Just $ Left err
      Right (amt :: MixedAmount) -> do
        return $ Just $ Right nullposting{
        --   pdate             -- :: Maybe Day,         -- ^ this posting's date, if different from the transaction's
        -- , pdate2            -- :: Maybe Day,         -- ^ this posting's secondary date, if different from the transaction's
        -- , pstatus           -- :: ClearedStatus,
          paccount = pack acct -- :: AccountName,
        , pamount = amt        -- :: MixedAmount,
        -- , pcomment          -- :: Text,              -- ^ this posting's comment lines, as a single non-indented multi-line string
        -- , ptype             -- :: PostingType,
        -- , ptags             -- :: [Tag],             -- ^ tag names and values, extracted from the comment
        -- , pbalanceassertion -- :: Maybe MixedAmount, -- ^ optional: the expected balance in the account after this posting
        -- , ptransaction      -- :: Maybe Transaction  -- ^ this posting's parent transaction (co-recursive types).
        }
rawPostingToPosting (RawPostingNote _) = return Nothing


-- A raw parse example:
--
-- 2010/01/01 * T1
--     Accounts:Hub    30.00 USD
--     Accounts:A1
-- & comments...
--
-- [
-- RawTransactionEntity (RawTransaction {rawTxnDate = "2010/01/01",
--  rawTxnDateAux = Nothing, rawTxnState = Just '*', rawTxnCode = Nothing,
--  rawTxnDesc = "T1", rawTxnNote = Nothing, rawTxnPosts = [
--   RawPosting {rawPostState = Nothing, rawPostAccount = "Accounts:Hub",
--    rawPostAmount = Just "30.00 USD", rawPostNote = Nothing},
--   RawPosting {rawPostState = Nothing, rawPostAccount = "Accounts:A1",
--    rawPostAmount = Nothing, rawPostNote = Nothing}
--   ]})
-- ,Whitespace "\n"
-- ,FileComment "2010/01/01 * T2\n    Accounts:Hub    40.00 USD\n    Accounts:A2\n\n2010/01/01 * T3\n    Accounts:Hub    10.00 USD\n    Accounts:A1\n"
-- ,Whitespace "\n"
-- ,FileComment " Now, I wish to list all transactions that pay into Accounts:Hub ONLY from \n Accounts:A1. How can I write a query like that? My cursory filtering \n attempts didn't work.\n"
-- ,Whitespace "\n"
-- ,FileComment " 2. The register displays all transactions that put a commodity into or take \n a commodity out of an account. Can I display where the money comes \n from/goes to as well? In other words, can I get the complete transaction \n detail in the register?\n"
-- ]


--- * hunit tests

tests_Hledger_Read_LedgerReader = TestList $ concat [
 ]
