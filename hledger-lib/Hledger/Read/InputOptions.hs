{-|

Various options to use when reading journal files.
Similar to CliOptions.inputflags, simplifies the journal-reading functions.

-}

module Hledger.Read.InputOptions (
    -- * Types and helpers for input options
    InputOpts(..)
  , definputopts
)
where

import Hledger.Data.Types
import Hledger.Data.Transaction
import Hledger.Data.Dates()

data InputOpts = InputOpts {
     -- files_             :: [FilePath]
     mformat_           :: Maybe StorageFormat                       -- ^ a file/storage format to try, unless overridden
                                                                     --   by a filename prefix. Nothing means try all.
    ,mrules_file_       :: Maybe FilePath                            -- ^ a conversion rules file to use (when reading CSV)
    ,aliases_           :: [String]                                  -- ^ account name aliases to apply
    ,anon_              :: Bool                                      -- ^ do light anonymisation/obfuscation of the data
    ,new_               :: Bool                                      -- ^ read only new transactions since this file was last read
    ,new_save_          :: Bool                                      -- ^ save latest new transactions state for next time
    ,pivot_             :: String                                    -- ^ use the given field's value as the account name
    ,forecast_          :: Maybe DateSpan                            -- ^ span in which to generate forecast transactions
    ,auto_              :: Bool                                      -- ^ generate automatic postings when journal is parsed
    ,balancingopts_     :: BalancingOpts                             -- ^ options for balancing transactions
    ,strict_            :: Bool                                      -- ^ do extra error checking (eg, all posted accounts are declared, no prices are inferred)
 } deriving (Show)

definputopts :: InputOpts
definputopts = InputOpts
    { mformat_           = Nothing
    , mrules_file_       = Nothing
    , aliases_           = []
    , anon_              = False
    , new_               = False
    , new_save_          = True
    , pivot_             = ""
    , forecast_          = Nothing
    , auto_              = False
    , balancingopts_     = balancingOpts
    , strict_            = False
    }
