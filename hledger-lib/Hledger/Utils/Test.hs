{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Utils.Test where

import Control.Exception
import Control.Monad
import Data.Functor.Identity
import Data.List
import qualified Data.Text as T
import EasyTest
import Safe 
import System.Exit
import System.IO
import Test.HUnit as HUnit
import Text.Megaparsec

import Hledger.Utils.Debug (pshow)
import Hledger.Utils.Parse (parseWithState)
import Hledger.Utils.UTF8IOCompat (error')

-- | Get a Test's label, or the empty string.
testName :: HUnit.Test -> String
testName (TestLabel n _) = n
testName _ = ""

-- | Flatten a Test containing TestLists into a list of single tests.
flattenTests :: HUnit.Test -> [HUnit.Test]
flattenTests (TestLabel _ t@(TestList _)) = flattenTests t
flattenTests (TestList ts) = concatMap flattenTests ts
flattenTests t = [t]

-- | Filter TestLists in a Test, recursively, preserving the structure.
filterTests :: (HUnit.Test -> Bool) -> HUnit.Test -> HUnit.Test
filterTests p (TestLabel l ts) = TestLabel l (filterTests p ts)
filterTests p (TestList ts) = TestList $ filter (any p . flattenTests) $ map (filterTests p) ts
filterTests _ t = t

-- | Simple way to assert something is some expected value, with no label.
is :: (Eq a, Show a) => a -> a -> Assertion
a `is` e = assertEqual "" e a  -- XXX should it have a message ?

-- | Assert a parse result is successful, printing the parse error on failure.
assertParse :: (Show t, Show e) => (Either (ParseError t e) a) -> Assertion
assertParse parse = either (assertFailure.show) (const (return ())) parse


-- | Assert a parse result is successful, printing the parse error on failure.
assertParseFailure :: (Either (ParseError t e) a) -> Assertion
assertParseFailure parse = either (const $ return ()) (const $ assertFailure "parse should not have succeeded") parse

-- | Assert a parse result is some expected value, printing the parse error on failure.
assertParseEqual :: (Show a, Eq a, Show t, Show e) => (Either (ParseError t e) a) -> a -> Assertion
assertParseEqual parse expected = either (assertFailure.show) (`is` expected) parse

-- | Assert that the parse result returned from an identity monad is some expected value,
-- on failure printing the parse error or differing values.
assertParseEqual' :: (Show a, Eq a, Show t, Show e) => Identity (Either (ParseError t e) a) -> a -> Assertion
assertParseEqual' parse expected = 
  either 
    (assertFailure . ("parse error: "++) . pshow) 
    (\actual -> assertEqual (unlines ["expected: " ++ show expected, " but got: " ++ show actual]) expected actual) 
    $ runIdentity parse

assertParseEqual'' :: (Show a, Eq a, Show t, Show e) => String -> Identity (Either (ParseError t e) a) -> a -> Assertion
assertParseEqual'' label parse expected = 
  either 
    (assertFailure . ("parse error: "++) . pshow) 
    (\actual -> assertEqual (unlines [label, "expected: " ++ show expected, " but got: " ++ show actual]) expected actual) 
    $ runIdentity parse

printParseError :: (Show a) => a -> IO ()
printParseError e = do putStr "parse error at "; print e

-- | Run some easytests, returning True if there was a problem. Catches ExitCode.
-- With arguments, runs only tests in the scope named by the first argument
-- (case sensitive). 
-- If there is a second argument, it should be an integer and will be used
-- as the seed for randomness. 
runEasyTests :: [String] -> EasyTest.Test () -> IO Bool
runEasyTests args easytests = (do
  case args of
    []    -> EasyTest.run easytests
    [a]   -> EasyTest.runOnly (T.pack a) easytests
    a:b:_ -> do
      case readMay b :: Maybe Int of
        Nothing   -> error' "the second argument should be an integer (a seed for easytest)"
        Just seed -> EasyTest.rerunOnly seed (T.pack a) easytests
  return False
  )
  `catch` (\(_::ExitCode) -> return True)

expectParseEq parser input expected = do
  let ep = runIdentity $ parseWithState mempty parser input
  scope "parse succeeded" $ expectRight ep
  let Right p = ep
  scope "parse result" $ expectEq p expected

-- | Run some hunit tests, returning True if there was a problem.
-- With arguments, runs only tests whose names contain the first argument
-- (case sensitive). 
runHunitTests :: [String] -> HUnit.Test -> IO Bool
runHunitTests args hunittests = do
  let ts = 
        (case args of
          a:_ -> filterTests ((a `isInfixOf`) . testName)
          _   -> id
        ) hunittests
  results <- liftM (fst . flip (,) 0) $ runTestTTStdout ts
  return $ errors results > 0 || failures results > 0
  where
    -- | Like runTestTT but prints to stdout.
    runTestTTStdout t = do
      (counts, 0) <- HUnit.runTestText (putTextToHandle stdout True) t
      return counts

--    matchedTests opts ts 
--      | tree_ $ reportopts_ opts = 
--        -- Tests, filtered by any arguments, in a flat list with simple names.
--        TestList $
--          filter (matchesAccount (queryFromOpts nulldate $ reportopts_ opts) . T.pack . testName) $ 
--          flattenTests ts
--      | otherwise = 
--        -- Tests, filtered by any arguments, in the original suites with hierarchical names.
--        filterTests (matchesAccount (queryFromOpts nulldate $ reportopts_ opts) . T.pack . testName) 
--        ts

-- -- | Like runTestTT but can optionally not erase progress output.
-- runTestTT' verbose t = do
--   (counts, 0) <- runTestText' (f stderr True) t
--   return counts
--   where f | verbose   = putTextToHandle'
--           | otherwise = putTextToHandle

-- -- | Like runTestText but also prints test names if any.
-- runTestText' :: PutText st -> Test -> IO (Counts, st)
-- runTestText' _pt _t@(TestLabel _label _) = error "HERE"  -- hPutStrLn stderr label >> runTestText pt t
-- runTestText' pt t = runTestText pt t

-- -- runTestText' (PutText put us0) t = do
-- --   (counts', us1) <- trace "XXX" $ performTest reportStart reportError reportFailure us0 t
-- --   us2 <- put (showCounts counts' ++ " :::: " ++ testName t) True us1
-- --   return (counts', us2)
-- --  where
-- --   reportStart ss us = put (showCounts (counts ss)) False us
-- --   reportError   = reportProblem "Error:"   "Error in:   "
-- --   reportFailure = reportProblem "Failure:" "Failure in: "
-- --   reportProblem p0 p1 loc msg ss us = put line True us
-- --    where line  = "### " ++ kind ++ path' ++ "\n" ++ formatLocation loc ++ msg
-- --          kind  = if null path' then p0 else p1
-- --          path' = showPath (path ss)

-- -- formatLocation :: Maybe SrcLoc -> String
-- -- formatLocation Nothing = ""
-- -- formatLocation (Just loc) = srcLocFile loc ++ ":" ++ show (srcLocStartLine loc) ++ "\n"

-- -- | Like putTextToHandle but does not erase progress lines.
-- putTextToHandle'
--     :: Handle
--     -> Bool -- ^ Write progress lines to handle?
--     -> PutText Int
-- putTextToHandle' handle showProgress = PutText put initCnt
--  where
--   initCnt = if showProgress then 0 else -1
--   put line pers (-1) = do when pers (hPutStrLn handle line); return (-1)
--   put line True  cnt = do hPutStrLn handle (erase cnt ++ line); return 0
--   put line False _   = do hPutStr handle ('\n' : line); return (length line)
--     -- The "erasing" strategy with a single '\r' relies on the fact that the
--     -- lengths of successive summary lines are monotonically nondecreasing.
--   erase cnt = if cnt == 0 then "" else "\r" ++ replicate cnt ' ' ++ "\r"

