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
-- import Text.Megaparsec hiding (parse)
-- import Text.Printf
import System.FilePath
import System.Time
import qualified Filesystem.Path.CurrentOS as F

import Hledger.Data
import Hledger.Read.Common
-- import Hledger.Utils
import Ledger.Parser.Text
import Text.Trifecta.Result (Result(..))

-- $setup
-- >>> :set -XOverloadedStrings

--- * reader

reader :: Reader
reader = Reader format detect parse

format :: String
format = "ledger"

-- | Does the given file path and data look like it might be ledger's journal format ?
detect :: FilePath -> Text -> Bool
detect f _t
  | f /= "-"  = takeExtension f `elem` ['.':format, ".l"] -- from a known file name: yes if the extension is .ledger or .l
  | otherwise = False                                      -- from stdin: yes, always attempt to parse stdin as a ledger journal
  -- otherwise = regexMatches "(^|\n)[0-9]+.*\n[ \t]+" $ T.unpack t   -- from stdin: yes if we can see something that looks like a journal entry (digits in column 0 with the next line indented)

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
      -- liftIO $ putStrLn $ show res
      pj <- liftIO $ foldM journalAddRawEntityInSitu nulljournal res
      -- journalFinalise :: ClockTime -> FilePath -> Text -> Bool -> ParsedJournal -> Either String Journal
      t <- liftIO getClockTime
      either throwError return $
        journalFinalise t path txt assrt pj

journalAddRawEntityInSitu :: ParsedJournal -> RawEntityInSitu -> IO ParsedJournal
journalAddRawEntityInSitu
  j
  RawEntityInSitu{rawEntity=RawTransactionEntity (RawTransaction{
      rawTxnDate    = date -- :: !String
    , rawTxnDateAux = mdate2 -- :: Maybe String
    , rawTxnState   = _mstatus -- :: Maybe Char
    , rawTxnCode    = _mcode -- :: Maybe String
    , rawTxnDesc    = desc -- :: !String
    , rawTxnNote    = _mnote -- :: Maybe String
    , rawTxnPosts   = rps -- :: ![RawPosting]
    })}
  = do
    ps <- catMaybes <$> mapM rawPostingToPosting rps
    let t = nulltransaction{
      -- tindex                   -- :: Integer,   -- ^ this transaction's 1-based position in the input stream, or 0 when not available
      -- tsourcepos               -- :: GenericSourcePos,
        tdate = parsedate date -- XXX                   -- :: Day,
      , tdate2 = parsedate <$> mdate2 -- XXX                  -- :: Maybe Day,
      -- tstatus                  -- :: ClearedStatus,
      -- tcode                    -- :: Text,
      , tdescription = pack desc            -- :: Text,
      -- tcomment                 -- :: Text,      -- ^ this transaction's comment lines, as a single non-indented multi-line string
      -- ttags                    -- :: [Tag],     -- ^ tag names and values, extracted from the comment
      , tpostings = ps               -- :: [Posting], -- ^ this transaction's postings
      -- tpreceding_comment_lines -- :: Text       -- ^ any comment lines immediately preceding this transaction
      }
    return $ addTransaction t j

journalAddRawEntityInSitu j _ = return j

rawPostingToPosting :: RawPosting -> IO (Maybe Posting)
rawPostingToPosting RawPosting{
    rawPostState   = _mstatus -- :: Maybe Char
  , rawPostAccount = acct -- :: !String
  , rawPostAmount  = mamtstr -- :: Maybe String
  , rawPostNote    = _mnote -- :: Maybe String
  } = do
    eamt <- runErroringJournalParser spaceandamountormissingp $ pack $ maybe "" (' ':) mamtstr
    case eamt of
      Left _err -> return Nothing -- XXX should throw error
      Right (amt :: MixedAmount) -> do
        return $ Just nullposting{
        --   pdate             -- :: Maybe Day,         -- ^ this posting's date, if different from the transaction's
        -- , pdate2            -- :: Maybe Day,         -- ^ this posting's secondary date, if different from the transaction's
        -- , pstatus           -- :: ClearedStatus,
          paccount = pack acct         -- :: AccountName,
        , pamount = amt          -- :: MixedAmount,
        -- , pcomment          -- :: Text,              -- ^ this posting's comment lines, as a single non-indented multi-line string
        -- , ptype             -- :: PostingType,
        -- , ptags             -- :: [Tag],             -- ^ tag names and values, extracted from the comment
        -- , pbalanceassertion -- :: Maybe MixedAmount, -- ^ optional: the expected balance in the account after this posting
        -- , ptransaction      -- :: Maybe Transaction  -- ^ this posting's parent transaction (co-recursive types).
        }
rawPostingToPosting (RawPostingNote _) = return Nothing

-- raw parse example:
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
    -- test_numberp
 ]
