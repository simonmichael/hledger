-- the hledger command-line executable; see Hledger/Cli.hs

module Main (main)
where
import qualified Hledger.Cli (main)

-- Have to write this explicitly for GHC 9.0.1a for some reason:
main :: IO ()
main = Hledger.Cli.main
