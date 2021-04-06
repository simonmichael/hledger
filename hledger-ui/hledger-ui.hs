module Main (main)
where
-- import Hledger.UI (main)
-- workaround for GHC 9.0.1 https://gitlab.haskell.org/ghc/ghc/-/issues/19397, #1503
import qualified Hledger.UI.Main (main)
main :: IO ()
main = Hledger.UI.Main.main
