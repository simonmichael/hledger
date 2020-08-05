{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Hledger.Utils.Parse (
  SimpleStringParser,
  SimpleTextParser,
  TextParser,
  JournalParser,
  ErroringJournalParser,

  choice',
  choiceInState,
  surroundedBy,
  parsewith,
  parsewithString,
  parseWithState,
  parseWithState',
  fromparse,
  parseerror,
  showDateParseError,
  nonspace,
  isNonNewlineSpace,
  restofline,
  eolof,

  spacenonewline,
  skipNonNewlineSpaces,
  skipNonNewlineSpaces1,
  skipNonNewlineSpaces',

  -- * re-exports
  CustomErr
)
where

import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Char
import Data.Functor.Identity (Identity(..))
import Data.List
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Custom
import Text.Printf

import Hledger.Data.Types
import Hledger.Utils.UTF8IOCompat (error')

-- | A parser of string to some type.
type SimpleStringParser a = Parsec CustomErr String a

-- | A parser of strict text to some type.
type SimpleTextParser = Parsec CustomErr Text  -- XXX an "a" argument breaks the CsvRulesParser declaration somehow

-- | A parser of text that runs in some monad.
type TextParser m a = ParsecT CustomErr Text m a

-- | A parser of text that runs in some monad, keeping a Journal as state.
type JournalParser m a = StateT Journal (ParsecT CustomErr Text m) a

-- | A parser of text that runs in some monad, keeping a Journal as
-- state, that can throw an exception to end parsing, preventing
-- further parser backtracking.
type ErroringJournalParser m a =
  StateT Journal (ParsecT CustomErr Text (ExceptT FinalParseError m)) a

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choice' :: [TextParser m a] -> TextParser m a
choice' = choice . map try

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choiceInState :: [StateT s (ParsecT CustomErr Text m) a] -> StateT s (ParsecT CustomErr Text m) a
choiceInState = choice . map try

surroundedBy :: Applicative m => m openclose -> m a -> m a
surroundedBy p = between p p

parsewith :: Parsec e Text a -> Text -> Either (ParseErrorBundle Text e) a
parsewith p = runParser p ""

parsewithString
  :: Parsec e String a -> String -> Either (ParseErrorBundle String e) a
parsewithString p = runParser p ""

-- | Run a stateful parser with some initial state on a text.
-- See also: runTextParser, runJournalParser.
parseWithState
  :: Monad m
  => st
  -> StateT st (ParsecT CustomErr Text m) a
  -> Text
  -> m (Either (ParseErrorBundle Text CustomErr) a)
parseWithState ctx p s = runParserT (evalStateT p ctx) "" s

parseWithState'
  :: (Stream s)
  => st
  -> StateT st (ParsecT e s Identity) a
  -> s
  -> (Either (ParseErrorBundle s e) a)
parseWithState' ctx p s = runParser (evalStateT p ctx) "" s

fromparse
  :: (Show t, Show (Token t), Show e) => Either (ParseErrorBundle t e) a -> a
fromparse = either parseerror id

parseerror :: (Show t, Show (Token t), Show e) => ParseErrorBundle t e -> a
parseerror e = error' $ showParseError e  -- PARTIAL:

showParseError
  :: (Show t, Show (Token t), Show e)
  => ParseErrorBundle t e -> String
showParseError e = "parse error at " ++ show e

showDateParseError
  :: (Show t, Show (Token t), Show e) => ParseErrorBundle t e -> String
showDateParseError e = printf "date parse error (%s)" (intercalate ", " $ tail $ lines $ show e)

nonspace :: TextParser m Char
nonspace = satisfy (not . isSpace)

isNonNewlineSpace :: Char -> Bool
isNonNewlineSpace c = c /= '\n' && isSpace c
-- XXX support \r\n ?
-- isNonNewlineSpace c = c /= '\n' && c /= '\r' && isSpace c

spacenonewline :: (Stream s, Char ~ Token s) => ParsecT CustomErr s m Char
spacenonewline = satisfy isNonNewlineSpace
{-# INLINABLE spacenonewline #-}

restofline :: TextParser m String
restofline = anySingle `manyTill` eolof

-- Skip many non-newline spaces.
skipNonNewlineSpaces :: (Stream s, Token s ~ Char) => ParsecT CustomErr s m ()
skipNonNewlineSpaces = () <$ takeWhileP Nothing isNonNewlineSpace
{-# INLINABLE skipNonNewlineSpaces #-}

-- Skip many non-newline spaces, failing if there are none.
skipNonNewlineSpaces1 :: (Stream s, Token s ~ Char) => ParsecT CustomErr s m ()
skipNonNewlineSpaces1 = () <$ takeWhile1P Nothing isNonNewlineSpace
{-# INLINABLE skipNonNewlineSpaces1 #-}

-- Skip many non-newline spaces, returning True if any have been skipped.
skipNonNewlineSpaces' :: (Stream s, Token s ~ Char) => ParsecT CustomErr s m Bool
skipNonNewlineSpaces' = True <$ skipNonNewlineSpaces1 <|> pure False
{-# INLINABLE skipNonNewlineSpaces' #-}


eolof :: TextParser m ()
eolof = (newline >> return ()) <|> eof
