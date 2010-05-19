{-# LANGUAGE CPP #-}
{-|
The main function is in this separate module so it can be imported by
benchmark scripts. As a side benefit, this avoids a weakness of sp, which
doesn't allow both #! and \{\-\# lines.
-}

module Hledger.Cli.Main where
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding (putStr, putStrLn)
import System.IO.UTF8
#endif

import Hledger.Cli.Commands.All
import Hledger.Data
import Hledger.Cli.Options
import Hledger.Tests
import Hledger.Utils (withLedgerDo)
import Hledger.Version (versionmsg, binaryfilename)

main :: IO ()
main = do
  (opts, cmd, args) <- parseArguments
  run cmd opts args
    where
      run cmd opts args
       | Help `elem` opts             = putStr usage
       | Version `elem` opts          = putStrLn versionmsg
       | BinaryFilename `elem` opts   = putStrLn binaryfilename
       | cmd `isPrefixOf` "balance"   = withLedgerDo opts args cmd balance
       | cmd `isPrefixOf` "convert"   = withLedgerDo opts args cmd convert
       | cmd `isPrefixOf` "print"     = withLedgerDo opts args cmd print'
       | cmd `isPrefixOf` "register"  = withLedgerDo opts args cmd register
       | cmd `isPrefixOf` "histogram" = withLedgerDo opts args cmd histogram
       | cmd `isPrefixOf` "add"       = withLedgerDo opts args cmd add
       | cmd `isPrefixOf` "stats"     = withLedgerDo opts args cmd stats
#ifdef VTY
       | cmd `isPrefixOf` "vty"       = withLedgerDo opts args cmd vty
#endif
#if defined(WEB) || defined(WEBHAPPSTACK)
       | cmd `isPrefixOf` "web"       = withLedgerDo opts args cmd web
#endif
#ifdef CHART
       | cmd `isPrefixOf` "chart"       = withLedgerDo opts args cmd chart
#endif
       | cmd `isPrefixOf` "test"      = runtests opts args >> return ()
       | otherwise                    = putStr usage
