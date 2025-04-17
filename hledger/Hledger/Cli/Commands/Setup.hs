{-|

Check and help set up various installation things.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.Cli.Commands.Setup (
  setupmode
 ,setup
)
where

-- import Data.Default (def)
-- import System.FilePath (takeFileName)
-- import Data.List (intercalate, nub, sortOn)
-- import Data.List.Extra (nubSort)
-- import qualified Data.Map as Map
-- import Data.Maybe (fromMaybe)
-- import Data.HashSet (size, fromList)
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
-- import qualified Data.Text.Lazy.Builder as TB
-- import Data.Time.Calendar (Day, addDays, diffDays)
-- import Data.Time.Clock.POSIX (getPOSIXTime)
-- import GHC.Stats
-- -- import System.Console.CmdArgs.Explicit hiding (Group)
-- import System.Mem (performMajorGC)
-- import Text.Printf (printf)
-- import Text.Tabular.AsciiWide

import Hledger
import Hledger.Cli.CliOptions
-- import Hledger.Cli.Utils (writeOutputLazyText)


setupmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Setup.txt")
  []
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- like Register.summarisePostings
-- | Print various statistics for the journal.
setup :: CliOpts -> Journal -> IO ()
setup _opts@CliOpts{rawopts_=_rawopts, reportspec_=_rspec} _j = do
  print "setup"


  -- let today = _rsDay rspec
  --     verbose = boolopt "verbose" rawopts
  --     q = _rsQuery rspec
  --     l = ledgerFromJournal q j
  --     intervalspans = snd $ reportSpanBothDates j rspec
  --     ismultiperiod = length intervalspans > 1
  --     (ls, txncounts) = unzip $ map (showLedgerStats verbose l today) intervalspans
  --     numtxns = sum txncounts
  --     txt = (if ismultiperiod then id else TL.init) $ TB.toLazyText $ unlinesB ls
  -- writeOutputLazyText opts txt
