{-|
A convenient module to import in hledger scripts, 
aiming to provide the most useful imports and reduce boilerplate.
|-}

{-# LANGUAGE PackageImports #-}

module Hledger.Cli.Script
( module Script
) 
where

import Control.Applicative as Script
import Control.Concurrent as Script
import Control.Monad as Script
import Data.Char as Script
import Data.Either as Script
import Data.Functor as Script
import Data.List as Script
import Data.Maybe as Script
import Data.Ord as Script
-- import Data.String.QQ (s)  -- https://github.com/audreyt/string-qq/pull/3
import Data.Time as Script
import Text.Printf as Script hiding (formatString)
import "text" Data.Text as Script (Text, pack, unpack)
  -- can't re-export much of Data.Text & Data.Text.IO, they need to be qualified
import Safe as Script hiding (at)
-- import qualified System.Console.CmdArgs.Explicit as Script
import System.Directory as Script
import System.Environment as Script
import System.Exit as Script
import System.FilePath as Script
import System.IO as Script
import System.IO.Error as Script
import System.Process as Script

import Hledger as Script
import Hledger.Cli as Script
import Hledger.Cli.Main as Script (argsToCliOpts)
