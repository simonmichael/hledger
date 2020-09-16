{-|

The @codes@ command lists the codes seen in transactions, in the order parsed.

-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Codes (
  codesmode
 ,codes
) where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Hledger
import Hledger.Cli.CliOptions


-- | Command line options for this command.
codesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Codes.txt")
  []
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The codes command.
codes :: CliOpts -> Journal -> IO ()
codes CliOpts{reportspec_=rspec} j = do
  let ts = entriesReport rspec j
      codes = (if empty_ (rsOpts rspec) then id else filter (not . T.null)) $
              map tcode ts
  mapM_ T.putStrLn codes
