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
  ,assertLeft
  ,assertRight
  ,assertParse
  ,assertParseEq
  ,assertParseEqOn
  ,assertParseError
  ,assertParseE
  ,assertParseEqE
  ,assertParseErrorE
  ,assertParseStateOn
)
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State.Strict (StateT, evalStateT, execStateT)
import Data.Default (Default(..))
import Data.List (isInfixOf)
import qualified Data.Text as T
import Test.Tasty hiding (defaultMain)
import Test.Tasty.HUnit
-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC
import Text.Megaparsec
import Text.Megaparsec.Custom
  ( CustomErr,
    FinalParseError,
    attachSource,
    customErrorBundlePretty,
    finalErrorBundlePretty,
  )

import Hledger.Utils.Debug (pshow)
-- import Hledger.Utils.UTF8IOCompat (error')

-- * tasty helpers

-- TODO: pretty-print values in failure messages


-- | Name and group a list of tests. Shorter alias for Test.Tasty.HUnit.testGroup.
tests :: String -> [TestTree] -> TestTree
tests = testGroup

-- | Name an assertion or sequence of assertions. Shorter alias for Test.Tasty.HUnit.testCase.
test :: String -> Assertion -> TestTree
test = testCase

-- | Assert any Left value.
assertLeft :: (HasCallStack, Eq b, Show b) => Either a b -> Assertion
assertLeft (Left _)  = return ()
assertLeft (Right b) = assertFailure $ "expected Left, got (Right " ++ show b ++ ")"

-- | Assert any Right value.
assertRight :: (HasCallStack, Eq a, Show a) => Either a b -> Assertion
assertRight (Right _) = return ()
assertRight (Left a)  = assertFailure $ "expected Right, got (Left " ++ show a ++ ")"

-- | Assert that this stateful parser runnable in IO successfully parses
-- all of the given input text, showing the parse error if it fails.
-- Suitable for hledger's JournalParser parsers.
assertParse :: (HasCallStack, Eq a, Show a, Default st) =>
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> Assertion
assertParse parser input = do
  ep <- runParserT (evalStateT (parser <* eof) def) "" input
  either (assertFailure.(++"\n").("\nparse error at "++).customErrorBundlePretty)
         (const $ return ())
         ep

-- | Assert a parser produces an expected value.
assertParseEq :: (HasCallStack, Eq a, Show a, Default st) =>
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> a -> Assertion
assertParseEq parser input expected = assertParseEqOn parser input id expected

-- | Like assertParseEq, but transform the parse result with the given function
-- before comparing it.
assertParseEqOn :: (HasCallStack, Eq b, Show b, Default st) =>
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> (a -> b) -> b -> Assertion
assertParseEqOn parser input f expected = do
  ep <- runParserT (evalStateT (parser <* eof) def) "" input
  either (assertFailure . (++"\n") . ("\nparse error at "++) . customErrorBundlePretty)
         (assertEqual "" expected . f)
         ep

-- | Assert that this stateful parser runnable in IO fails to parse
-- the given input text, with a parse error containing the given string.
assertParseError :: (HasCallStack, Eq a, Show a, Default st) =>
  StateT st (ParsecT CustomErr T.Text IO) a -> String -> String -> Assertion
assertParseError parser input errstr = do
  ep <- runParserT (evalStateT parser def) "" (T.pack input)
  case ep of
    Right v -> assertFailure $ "\nparse succeeded unexpectedly, producing:\n" ++ pshow v ++ "\n"
    Left e  -> do
      let e' = customErrorBundlePretty e
      if errstr `isInfixOf` e'
      then return ()
      else assertFailure $ "\nparse error is not as expected:\n" ++ e' ++ "\n"

-- | Run a stateful parser in IO like assertParse, then assert that the
-- final state (the wrapped state, not megaparsec's internal state),
-- transformed by the given function, matches the given expected value.
assertParseStateOn :: (HasCallStack, Eq b, Show b, Default st) =>
     StateT st (ParsecT CustomErr T.Text IO) a
  -> T.Text
  -> (st -> b)
  -> b
  -> Assertion
assertParseStateOn parser input f expected = do
  es <- runParserT (execStateT (parser <* eof) def) "" input
  case es of
    Left err -> assertFailure $ (++"\n") $ ("\nparse error at "++) $ customErrorBundlePretty err
    Right s  -> assertEqual "" expected $ f s

-- | These "E" variants of the above are suitable for hledger's ErroringJournalParser parsers.
assertParseE
  :: (HasCallStack, Eq a, Show a, Default st)
  => StateT st (ParsecT CustomErr T.Text (ExceptT FinalParseError IO)) a
  -> T.Text
  -> Assertion
assertParseE parser input = do
  let filepath = ""
  eep <- runExceptT $
           runParserT (evalStateT (parser <* eof) def) filepath input
  case eep of
    Left finalErr ->
      let prettyErr = finalErrorBundlePretty $ attachSource filepath input finalErr
      in  assertFailure $ "parse error at " <> prettyErr
    Right ep ->
      either (assertFailure.(++"\n").("\nparse error at "++).customErrorBundlePretty)
             (const $ return ())
             ep

assertParseEqE
  :: (Default st, Eq a, Show a, HasCallStack)
  => StateT st (ParsecT CustomErr T.Text (ExceptT FinalParseError IO)) a
  -> T.Text
  -> a
  -> Assertion
assertParseEqE parser input expected = assertParseEqOnE parser input id expected

assertParseEqOnE
  :: (HasCallStack, Eq b, Show b, Default st)
  => StateT st (ParsecT CustomErr T.Text (ExceptT FinalParseError IO)) a
  -> T.Text
  -> (a -> b)
  -> b
  -> Assertion
assertParseEqOnE parser input f expected = do
  let filepath = ""
  eep <- runExceptT $ runParserT (evalStateT (parser <* eof) def) filepath input
  case eep of
    Left finalErr ->
      let prettyErr = finalErrorBundlePretty $ attachSource filepath input finalErr
      in  assertFailure $ "parse error at " <> prettyErr
    Right ep ->
      either (assertFailure . (++"\n") . ("\nparse error at "++) . customErrorBundlePretty)
             (assertEqual "" expected . f)
             ep

assertParseErrorE
  :: (Default st, Eq a, Show a, HasCallStack)
  => StateT st (ParsecT CustomErr T.Text (ExceptT FinalParseError IO)) a
  -> T.Text
  -> String
  -> Assertion
assertParseErrorE parser input errstr = do
  let filepath = ""
  eep <- runExceptT $ runParserT (evalStateT parser def) filepath input
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
