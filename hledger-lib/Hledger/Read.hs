{-# LANGUAGE ScopedTypeVariables #-}
{-|

This is the entry point to hledger's reading system, which can read
Journals from various data formats. Use this module if you want to parse
journal data or read journal files. Generally it should not be necessary
to import modules below this one.

-}

module Hledger.Read (
       -- * Journal reading API
       defaultJournalPath,
       defaultJournal,
       readJournal,
       readJournal',
       readJournalFile,
       readJournalFiles,
       requireJournalFileExists,
       ensureJournalFileExists,
       -- * Parsers used elsewhere
       postingp,
       accountnamep,
       amountp,
       amountp',
       mamountp',
       numberp,
       codep,
       accountaliasp,
       -- * Tests
       samplejournal,
       tests_Hledger_Read,
)
where
import Control.Monad.Except
import Data.List
import Data.Maybe
import Test.HUnit

import Hledger.Data.Types
import Hledger.Data.Journal (nullctx)
import Hledger.Read.Util
import Hledger.Read.JournalReader as JournalReader
import Hledger.Read.TimedotReader as TimedotReader
import Hledger.Read.TimelogReader as TimelogReader
import Hledger.Read.CsvReader as CsvReader
import Hledger.Utils
import Prelude hiding (getContents, writeFile)


tests_Hledger_Read = TestList $
  tests_readJournal'
  ++ [
   tests_Hledger_Read_JournalReader,
   tests_Hledger_Read_TimedotReader,
   tests_Hledger_Read_TimelogReader,
   tests_Hledger_Read_CsvReader,

   "journal" ~: do
    r <- runExceptT $ parseWithCtx nullctx JournalReader.journalp ""
    assertBool "journalp should parse an empty file" (isRight $ r)
    jE <- readJournal Nothing Nothing True Nothing "" -- don't know how to get it from journal
    either error' (assertBool "journalp parsing an empty file should give an empty journal" . null . jtxns) jE

  ]
