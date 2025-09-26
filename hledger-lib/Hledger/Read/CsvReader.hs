--- * -*- outline-regexp:"--- \\*"; -*-
--- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

A reader for CSV (character-separated) data.
This also reads a rules file to help interpret the CSV data.

-}

--- ** language
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

--- ** exports
module Hledger.Read.CsvReader (
  -- * Reader
  reader,
  -- * Tests
  tests_CsvReader,
)
where

--- ** imports
import Prelude hiding (Applicative(..))
import Control.Monad.Except       (ExceptT(..), liftEither)
import Control.Monad.IO.Class     (MonadIO)
import System.IO                  (Handle)

import Hledger.Data
import Hledger.Utils
import Hledger.Read.Common (aliasesFromOpts, Reader(..), InputOpts(..), journalFinalise)
import Hledger.Read.RulesReader (readJournalFromCsv, getRulesFile, rulesEncoding, readRules)
import Control.Monad.Trans (lift)

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** reader

reader :: MonadIO m => SepFormat -> Reader m
reader sep = Reader
  {rFormat     = Sep sep
  ,rExtensions = [show sep]
  ,rReadFn     = parse sep
  ,rParser     = const $ fail "sorry, CSV files can't be included yet"
    -- This unnecessarily shows the CSV file's first line in the error message,
    -- but gives a more useful message than just calling error'.
    -- XXX Note every call to error' in Hledger.Read.* is potentially a similar problem -
    -- the error message is good enough when the file was specified directly by the user,
    -- but not good if it was loaded by a possibly long chain of include directives.
  }

-- | Parse and post-process a "Journal" from a CSV(/SSV/TSV/*SV) data file, or give an error.
-- This currently ignores the provided input file handle, and reads from the data file itself,
-- inferring a corresponding rules file to help convert it.
-- This does not check balance assertions.
parse :: SepFormat -> InputOpts -> FilePath -> Handle -> ExceptT String IO Journal
parse sep iopts f h = do
  rules <- readRules $ getRulesFile f (mrules_file_ iopts)
  mencoding <- rulesEncoding rules
  csvtext <- lift $ hGetContentsPortably mencoding h
  readJournalFromCsv rules f csvtext (Just sep)
  -- apply any command line account aliases. Can fail with a bad replacement pattern.
  >>= liftEither . journalApplyAliases (aliasesFromOpts iopts)
      -- journalFinalise assumes the journal's items are
      -- reversed, as produced by JournalReader's parser.
      -- But here they are already properly ordered. So we'd
      -- better preemptively reverse them once more. XXX inefficient
      . journalReverse
  >>= journalFinalise iopts{balancingopts_=(balancingopts_ iopts){ignore_assertions_=True}} f ""

--- ** tests

tests_CsvReader = testGroup "CsvReader" [
  ]
