{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Printunique (
  printuniquemode
 ,printunique
)
where

import Data.List.Extra (nubSortOn)
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Print

printuniquemode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Printunique.txt")
  []
  [generalflagsgroup1]
  hiddenflags
  ([], Nothing)

printunique opts j@Journal{jtxns=ts} = do
  print' opts j{jtxns=uniquify ts}
  where
    uniquify = nubSortOn thingToCompare
    thingToCompare = tdescription
    -- thingToCompare = tdate
