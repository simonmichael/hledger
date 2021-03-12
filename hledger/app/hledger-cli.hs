-- the hledger command-line executable; see Hledger/Cli/Main.hs

module Main (main)
where
import qualified Hledger.Cli.Main (main)

-- Have to write this explicitly for GHC 9.0.1a for some reason:
main :: IO ()
main = Hledger.Cli.Main.main
