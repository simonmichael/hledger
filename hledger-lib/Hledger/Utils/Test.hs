module Hledger.Utils.Test where

import Data.Functor.Identity
import Test.HUnit
import Text.Megaparsec
import Hledger.Utils.Debug (pshow)

-- | Get a Test's label, or the empty string.
testName :: Test -> String
testName (TestLabel n _) = n
testName _ = ""

-- | Flatten a Test containing TestLists into a list of single tests.
flattenTests :: Test -> [Test]
flattenTests (TestLabel _ t@(TestList _)) = flattenTests t
flattenTests (TestList ts) = concatMap flattenTests ts
flattenTests t = [t]

-- | Filter TestLists in a Test, recursively, preserving the structure.
filterTests :: (Test -> Bool) -> Test -> Test
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

