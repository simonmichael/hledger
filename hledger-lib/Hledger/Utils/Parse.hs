{-# LANGUAGE CPP, TypeFamilies #-}
module Hledger.Utils.Parse where

import Control.Monad.Except
import Control.Monad.State.Strict (StateT, evalStateT)
import Data.Char
import Data.Functor.Identity (Identity(..))
import Data.List
import Data.Text (Text)
import Text.Megaparsec.Compat
import Text.Printf

import Hledger.Data.Types
import Hledger.Utils.UTF8IOCompat (error')

-- | A parser of string to some type.
type SimpleStringParser a = Parsec MPErr String a

-- | A parser of strict text to some type.
type SimpleTextParser = Parsec MPErr Text  -- XXX an "a" argument breaks the CsvRulesParser declaration somehow

-- | A parser of text in some monad.
type TextParser m a = ParsecT MPErr Text m a

-- | A parser of text in some monad, with a journal as state.
type JournalParser m a = StateT Journal (ParsecT MPErr Text m) a

-- | A parser of text in some monad, with a journal as state, that can throw an error string mid-parse.
type ErroringJournalParser m a = StateT Journal (ParsecT MPErr Text (ExceptT String m)) a

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choice' :: [TextParser m a] -> TextParser m a
choice' = choice . map try

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choiceInState :: [StateT s (ParsecT MPErr Text m) a] -> StateT s (ParsecT MPErr Text m) a
choiceInState = choice . map try

parsewith :: Parsec e Text a -> Text -> Either (ParseError Char e) a
parsewith p = runParser p ""

parsewithString :: Parsec e String a -> String -> Either (ParseError Char e) a
parsewithString p = runParser p ""

parseWithState :: Monad m => st -> StateT st (ParsecT MPErr Text m) a -> Text -> m (Either (ParseError Char MPErr) a)
parseWithState ctx p s = runParserT (evalStateT p ctx) "" s

parseWithState' :: (
  Stream s 
#if !MIN_VERSION_megaparsec(6,0,0)
  ,ErrorComponent e
#endif
  ) => st -> StateT st (ParsecT e s Identity) a -> s -> (Either (ParseError (Token s) e) a)
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

spacenonewline :: (Stream s, Char ~ Token s) => ParsecT MPErr s m Char
spacenonewline = satisfy (`elem` " \v\f\t")

restofline :: TextParser m String
restofline = anyChar `manyTill` newline

eolof :: TextParser m ()
eolof = (newline >> return ()) <|> eof
