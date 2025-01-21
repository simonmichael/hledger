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
import Hledger.Read.RulesReader (readJournalFromCsv)

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** reader

reader :: MonadIO m => SepFormat -> Reader m
reader sep = Reader
  {rFormat     = Sep sep
  ,rExtensions = [show sep]
  ,rReadFn     = parse sep
  ,rParser     = error' "sorry, CSV files can't be included yet"  -- PARTIAL:
  }

-- | Parse and post-process a "Journal" from CSV data, or give an error.
-- This currently ignores the provided data, and reads it from the file path instead.
-- This file path is normally the CSV(/SSV/TSV) data file, and a corresponding rules file is inferred.
-- But it can also be the rules file, in which case the corresponding data file is inferred.
-- This does not check balance assertions.
parse :: SepFormat -> InputOpts -> FilePath -> Handle -> ExceptT String IO Journal
parse sep iopts f h = do
  let mrulesfile = mrules_file_ iopts
  readJournalFromCsv (Right <$> mrulesfile) f h (Just sep)
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

