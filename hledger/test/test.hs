{-
Run hledger's (and hledger-lib's) unit tests as a cabal test suite,
by running the test command with no options.
-}

import Hledger.Cli

main = testcmd defcliopts (error "journal-less command tried to use the journal")
