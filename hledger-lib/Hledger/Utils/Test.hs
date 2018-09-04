{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hledger.Utils.Test (
   HasCallStack
  ,module EasyTest
  ,runEasyTests
  ,tests
  ,_tests
  ,test
  ,_test
  ,it
  ,_it
  ,is
  ,expectEqPP
  ,expectParse
  ,expectParseError
  ,expectParseEq
  ,expectParseEqOn
) 
where

import Control.Exception
import Control.Monad.State.Strict (StateT, evalStateT)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Data.CallStack
import Data.List
import qualified Data.Text as T
import Safe 
import System.Exit
import Text.Megaparsec
import Text.Megaparsec.Custom

import EasyTest hiding (char, char', tests)  -- reexported
import qualified EasyTest as E               -- used here

import Hledger.Utils.Debug (pshow)
import Hledger.Utils.UTF8IOCompat (error')

-- * easytest helpers

-- | Name the given test(s). A readability synonym for easytest's "scope".
test :: T.Text -> E.Test a -> E.Test a 
test = E.scope

-- | Skip the given test(s), with the same type signature as "test".
-- If called in a monadic sequence of tests, also skips following tests.
_test :: T.Text -> E.Test a -> E.Test a 
_test _name = (E.skip >>) 

-- | Name the given test(s). A synonym for "test".
it :: T.Text -> E.Test a -> E.Test a 
it = test

-- | Skip the given test(s), and any following tests in a monadic sequence. 
-- A synonym for "_test".
_it :: T.Text -> E.Test a -> E.Test a 
_it = _test

-- | Name and group a list of tests. Combines easytest's "scope" and "tests".
tests :: T.Text -> [E.Test ()] -> E.Test () 
tests name = E.scope name . E.tests

-- | Skip the given list of tests, and any following tests in a monadic sequence,
-- with the same type signature as "group".
_tests :: T.Text -> [E.Test ()] -> E.Test () 
_tests _name = (E.skip >>) . E.tests

-- | Run some easytests, returning True if there was a problem. Catches ExitCode.
-- With arguments, runs only tests in the scope named by the first argument
-- (case sensitive). 
-- If there is a second argument, it should be an integer and will be used
-- as the seed for randomness. 
runEasyTests :: [String] -> E.Test () -> IO Bool
runEasyTests args easytests = (do
  case args of
    []    -> E.run easytests
    [a]   -> E.runOnly (T.pack a) easytests
    a:b:_ -> do
      case readMay b :: Maybe Int of
        Nothing   -> error' "the second argument should be an integer (a seed for easytest)"
        Just seed -> E.rerunOnly seed (T.pack a) easytests
  return False
  )
  `catch` (\(_::ExitCode) -> return True)

-- | Like easytest's expectEq (asserts the second (actual) value equals the first (expected) value)
-- but pretty-prints the values in the failure output. 
expectEqPP :: (Eq a, Show a, HasCallStack) => a -> a -> E.Test ()
expectEqPP expected actual = if expected == actual then E.ok else E.crash $
  "\nexpected:\n" <> T.pack (pshow expected) <> "\nbut got:\n" <> T.pack (pshow actual) <> "\n"

-- | Shorter and flipped version of expectEqPP. The expected value goes last.
is :: (Eq a, Show a, HasCallStack) => a -> a -> Test ()
is = flip expectEqPP

-- | Test that this stateful parser runnable in IO successfully parses 
-- all of the given input text, showing the parse error if it fails. 
-- Suitable for hledger's JournalParser parsers.
expectParse :: (Monoid st, Eq a, Show a, HasCallStack) => 
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> E.Test ()
expectParse parser input = do
  ep <- E.io (runParserT (evalStateT (parser <* eof) mempty) "" input)
  either (fail.(++"\n").("\nparse error at "++).parseErrorPretty) (const ok) ep

-- | Test that this stateful parser runnable in IO fails to parse 
-- the given input text, with a parse error containing the given string. 
expectParseError :: (Monoid st, Eq a, Show a, HasCallStack) => 
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> String -> E.Test ()
expectParseError parser input errstr = do
  ep <- E.io (runParserT (evalStateT parser mempty) "" input)
  case ep of
    Right v -> fail $ "\nparse succeeded unexpectedly, producing:\n" ++ pshow v ++ "\n"
    Left e  -> do
      let e' = parseErrorPretty e
      if errstr `isInfixOf` e'
      then ok
      else fail $ "\nparse error is not as expected:\n" ++ e' ++ "\n"

-- | Like expectParse, but also test the parse result is an expected value,
-- pretty-printing both if it fails. 
expectParseEq :: (Monoid st, Eq a, Show a, HasCallStack) => 
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> a -> E.Test ()
expectParseEq parser input expected = expectParseEqOn parser input id expected

-- | Like expectParseEq, but transform the parse result with the given function 
-- before comparing it.
expectParseEqOn :: (Monoid st, Eq b, Show b, HasCallStack) => 
  StateT st (ParsecT CustomErr T.Text IO) a -> T.Text -> (a -> b) -> b -> E.Test ()
expectParseEqOn parser input f expected = do
  ep <- E.io $ runParserT (evalStateT (parser <* eof) mempty) "" input
  either (fail . (++"\n") . ("\nparse error at "++) . parseErrorPretty) (expectEqPP expected . f) ep

