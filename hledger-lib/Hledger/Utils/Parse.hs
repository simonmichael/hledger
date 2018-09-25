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
  spacenonewline,
  restofline,
  eolof,

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

-- | A parser of text in some monad.
type TextParser m a = ParsecT CustomErr Text m a

-- | A parser of text in some monad, with a journal as state.
type JournalParser m a = StateT Journal (ParsecT CustomErr Text m) a

-- | A parser of text in some monad, with a journal as state, that can throw a
-- "final" parse error that does not backtrack.
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

parsewith :: Parsec e Text a -> Text -> Either (ParseError Char e) a
parsewith p = runParser p ""

parsewithString :: Parsec e String a -> String -> Either (ParseError Char e) a
parsewithString p = runParser p ""

-- | Run a stateful parser with some initial state on a text.
-- See also: runTextParser, runJournalParser.
parseWithState :: Monad m => st -> StateT st (ParsecT CustomErr Text m) a -> Text -> m (Either (ParseError Char CustomErr) a)
parseWithState ctx p s = runParserT (evalStateT p ctx) "" s

parseWithState'
  :: (Stream s)
  => st
  -> StateT st (ParsecT e s Identity) a
  -> s
  -> (Either (ParseError (Token s) e) a)
parseWithState' ctx p s = runParser (evalStateT p ctx) "" s

fromparse :: (Show t, Show e) => Either (ParseError t e) a -> a
fromparse = either parseerror id

parseerror :: (Show t, Show e) => ParseError t e -> a
parseerror e = error' $ showParseError e

showParseError :: (Show t, Show e) => ParseError t e -> String
showParseError e = "parse error at " ++ show e

showDateParseError :: (Show t, Show e) => ParseError t e -> String
showDateParseError e = printf "date parse error (%s)" (intercalate ", " $ tail $ lines $ show e)

nonspace :: TextParser m Char
nonspace = satisfy (not . isSpace)

isNonNewlineSpace :: Char -> Bool
isNonNewlineSpace c = c /= '\n' && isSpace c

spacenonewline :: (Stream s, Char ~ Token s) => ParsecT CustomErr s m Char
spacenonewline = satisfy isNonNewlineSpace

restofline :: TextParser m String
restofline = anyChar `manyTill` newline

eolof :: TextParser m ()
eolof = (newline >> return ()) <|> eof
