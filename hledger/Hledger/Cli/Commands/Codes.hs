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

import Data.Text qualified as T

import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Utils (writeOutputWithTitle)


-- | Command line options for this command.
codesmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Codes.txt")
  []
  cligeneralflagsgroups1
  hiddenflags
  ([], Just $ argsFlag "[QUERY]")

-- | The codes command.
codes :: CliOpts -> Journal -> IO ()
codes opts@CliOpts{reportspec_=rspec} j = do
  let ts = entriesReport rspec j
      codes' = (if empty_ (_rsReportOpts rspec) then id else filter (not . T.null)) $
              map tcode ts
  writeOutputWithTitle opts $ T.unlines codes'
