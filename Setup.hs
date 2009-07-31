#!/usr/bin/env runhaskell
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import System.FilePath
import System.Process

main = defaultMainWithHooks $ simpleUserHooks{runTests=runTests'}

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = system testprog >> return ()
    where testprog = (buildDir lbi) </> "hledger" </> "hledger test"
