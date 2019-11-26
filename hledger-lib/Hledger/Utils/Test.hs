{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Utils.Test (
   module Test.Tasty
  ,module Test.Tasty.HUnit
  -- ,module QC
  -- ,module SC
  ,tests
  ,test
  ,is
  ,expect
  ,assertEq
  ,expectEq
  ,assertLeft
  ,expectLeft
  ,assertRight
  ,expectRight
  ,expectParse
  ,expectParseEq
  ,expectParseEqOn
  ,expectParseError
  ,expectParseE
  ,expectParseEqE
  ,expectParseErrorE
  ,expectParseStateOn
)
where

import Test.Tasty
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Strict (StateT, evalStateT, execStateT)
-- #if !(MIN_VERSION_base(4,11,0))
-- import Data.Monoid ((<>))
-- #endif
-- import Data.CallStack
import Data.List (isInfixOf)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Custom

import Hledger.Utils.Debug (pshow)
-- import Hledger.Utils.UTF8IOCompat (error')

-- * tasty helpers

-- | Name and group a list of tests.
tests :: String -> [TestTree] -> TestTree
tests = testGroup

-- | Name the given test(s).
-- test :: T.Text -> E.Test a -> E.Test a
-- test :: String -> Assertion -> TestTree
test :: String -> TestTree -> TestTree
test _name = id

-- | Skip the given test(s), with the same type signature as "test".
-- If called in a monadic sequence of tests, also skips following tests. (?)
-- _test :: T.Text -> E.Test a -> E.Test a
-- _test _name = (E.skip >>)

-- | Short equality test constructor. Actual value on the left, expected on the right.
is :: (Eq a, Show a, HasCallStack) => a -> a -> TestTree
is actual expected = testCase "sometest" $ actual @?= expected

-- | Expect True.
expect :: HasCallStack => Bool -> TestTree
expect val = testCase "sometest" $ assertBool "was false" val

-- | Assert equality. Expected first, actual second.
assertEq :: (HasCallStack, Eq a, Show a) => a -> a -> Assertion
assertEq expected actual = assertEqual "was not equal" expected actual

-- | Test for equality. Expected first, actual second.
expectEq :: (HasCallStack, Eq a, Show a) => a -> a -> TestTree
expectEq a b = testCase "sometest" $ assertEq a b

-- | Assert any Left value.
assertLeft :: (HasCallStack, Eq b, Show b) => Either a b -> Assertion
assertLeft (Left _)  = return ()
assertLeft (Right b) = assertFailure $ "expected Left, got (Right " ++ show b ++ ")"

-- | Test for any Left value.
expectLeft :: (HasCallStack, Eq a, Show a) => Either e a -> TestTree
expectLeft = testCase "sometest" . assertLeft

-- | Assert any Right value.
assertRight :: (HasCallStack, Eq a, Show a) => Either a b -> Assertion
assertRight (Right _) = return ()
assertRight (Left a)  = assertFailure $ "expected Right, got (Left " ++ show a ++ ")"

-- | Test for any Right value.
expectRight :: (HasCallStack, Eq a, Show a) => Either a b -> TestTree
expectRight = testCase "sometest" . assertRight

-- | Test that this stateful parser runnable in IO successfully parses
-- all of the given input text, showing the parse error if it fails.
-- Suitable for hledger's JournalParser parsers.
-- expectParse :: (Monoid st, Eq a, Show a, HasCallStack) =>
--   StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> E.Test ()
expectParse :: (HasCallStack, Eq a, Show a, Monoid st) =>
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> TestTree
expectParse parser input = testCaseSteps "sometest" $ \_step -> do
  ep <- runParserT (evalStateT (parser <* eof) mempty) "" input
  either (assertFailure.(++"\n").("\nparse error at "++).customErrorBundlePretty)
         (const $ return ())
         ep

-- -- pretty-printing both if it fails.
-- | Like expectParse, but also test the parse result is an expected value.
expectParseEq :: (HasCallStack, Eq a, Show a, Monoid st) =>
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> a -> TestTree
expectParseEq parser input expected = expectParseEqOn parser input id expected

-- | Like expectParseEq, but transform the parse result with the given function
-- before comparing it.
expectParseEqOn :: (HasCallStack, Eq b, Show b, Monoid st) =>
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> (a -> b) -> b -> TestTree
expectParseEqOn parser input f expected = testCaseSteps "sometest" $ \_step -> do
  ep <- runParserT (evalStateT (parser <* eof) mempty) "" input
  either (assertFailure . (++"\n") . ("\nparse error at "++) . customErrorBundlePretty)
         (assertEq expected . f)
         ep

-- | Test that this stateful parser runnable in IO fails to parse
-- the given input text, with a parse error containing the given string.
expectParseError :: (HasCallStack, Eq a, Show a, Monoid st) =>
  StateT st (ParsecT CustomErr T.Text IO) a -> String -> String -> TestTree
expectParseError parser input errstr = testCaseSteps "sometest" $ \_step -> do
  ep <- runParserT (evalStateT parser mempty) "" (T.pack input)
  case ep of
    Right v -> assertFailure $ "\nparse succeeded unexpectedly, producing:\n" ++ pshow v ++ "\n"
    Left e  -> do
      let e' = customErrorBundlePretty e
      if errstr `isInfixOf` e'
      then return ()
      else assertFailure $ "\nparse error is not as expected:\n" ++ e' ++ "\n"

-- Suitable for hledger's ErroringJournalParser parsers.
expectParseE
  :: (HasCallStack, Eq a, Show a, Monoid st)
  => StateT st (ParsecT CustomErr T.Text (ExceptT FinalParseError IO)) a
  -> T.Text
  -> TestTree
expectParseE parser input = testCaseSteps "sometest" $ \_step -> do
  let filepath = ""
  eep <- runExceptT $
           runParserT (evalStateT (parser <* eof) mempty) filepath input
  case eep of
    Left finalErr ->
      let prettyErr = finalErrorBundlePretty $ attachSource filepath input finalErr
      in  assertFailure $ "parse error at " <> prettyErr
    Right ep ->
      either (assertFailure.(++"\n").("\nparse error at "++).customErrorBundlePretty)
             (const $ return ())
             ep

expectParseEqE
  :: (Monoid st, Eq a, Show a, HasCallStack)
  => StateT st (ParsecT CustomErr T.Text (ExceptT FinalParseError IO)) a
  -> T.Text
  -> a
  -> TestTree
expectParseEqE parser input expected = expectParseEqOnE parser input id expected

expectParseEqOnE
  :: (HasCallStack, Eq b, Show b, Monoid st)
  => StateT st (ParsecT CustomErr T.Text (ExceptT FinalParseError IO)) a
  -> T.Text
  -> (a -> b)
  -> b
  -> TestTree
expectParseEqOnE parser input f expected = testCaseSteps "sometest" $ \_step -> do
  let filepath = ""
  eep <- runExceptT $ runParserT (evalStateT (parser <* eof) mempty) filepath input
  case eep of
    Left finalErr ->
      let prettyErr = finalErrorBundlePretty $ attachSource filepath input finalErr
      in  assertFailure $ "parse error at " <> prettyErr
    Right ep ->
      either (assertFailure . (++"\n") . ("\nparse error at "++) . customErrorBundlePretty)
             (assertEq expected . f)
             ep

expectParseErrorE
  :: (Monoid st, Eq a, Show a, HasCallStack)
  => StateT st (ParsecT CustomErr T.Text (ExceptT FinalParseError IO)) a
  -> T.Text
  -> String
  -> TestTree
expectParseErrorE parser input errstr = testCaseSteps "sometest" $ \_step -> do
  let filepath = ""
  eep <- runExceptT $ runParserT (evalStateT parser mempty) filepath input
  case eep of
    Left finalErr -> do
      let prettyErr = finalErrorBundlePretty $ attachSource filepath input finalErr
      if errstr `isInfixOf` prettyErr
      then return ()
      else assertFailure $ "\nparse error is not as expected:\n" ++ prettyErr ++ "\n"
    Right ep -> case ep of
      Right v -> assertFailure $ "\nparse succeeded unexpectedly, producing:\n" ++ pshow v ++ "\n"
      Left e  -> do
        let e' = customErrorBundlePretty e
        if errstr `isInfixOf` e'
        then return ()
        else assertFailure $ "\nparse error is not as expected:\n" ++ e' ++ "\n"

-- | Run a stateful parser in IO like expectParse, then compare the
-- final state (the wrapped state, not megaparsec's internal state),
-- transformed by the given function, with the given expected value.
expectParseStateOn :: (HasCallStack, Eq b, Show b, Monoid st) =>
     StateT st (ParsecT CustomErr T.Text IO) a
  -> T.Text
  -> (st -> b)
  -> b
  -> TestTree
expectParseStateOn parser input f expected = testCaseSteps "sometest" $ \_step -> do
  es <- runParserT (execStateT (parser <* eof) mempty) "" input
  case es of
    Left err -> assertFailure $ (++"\n") $ ("\nparse error at "++) $ customErrorBundlePretty err
    Right s  -> assertEq expected $ f s

