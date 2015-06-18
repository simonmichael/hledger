import Control.Concurrent (threadDelay)
import SimpleBench        (defaultMain)
import System.Environment (withArgs)

main = do
  -- expects to be run from the parent directory, as by cabal
  withArgs [
    "-fbench/default.bench"
   ,"dist/build/hledger/hledger"
   -- ,"-v"
   ] defaultMain

  -- a little delay to avoid truncation of final output by stack
  -- in a slow-rendering terminal, such as an emacs shell
  threadDelay 500000
