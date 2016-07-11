{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Hledger.Utils.Parse where

import Control.Monad.Except
import Data.Char
import Data.List
import Data.Text (Text)
import Text.Megaparsec hiding (State)
import Text.Printf

import Control.Monad.State.Strict (StateT, State, evalStateT, evalState)

import Hledger.Data.Types
import Hledger.Utils.UTF8IOCompat (error')

-- | A parser of strict text with generic user state, monad and return type.
type TextParser m a = ParsecT Dec Text m a

type JournalParser a = TextParser (State Journal) a

-- | A journal parser that runs in IO and can throw an error mid-parse.
type ErroringJournalParser a = TextParser (StateT Journal (ExceptT String IO)) a

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choice' :: [TextParser m a] -> TextParser m a
choice' = choice . map Text.Megaparsec.try

parsewith :: Parsec e Text a -> Text -> Either (ParseError Char e) a
parsewith p = runParser p ""

parsewithString :: Parsec e String a -> String -> Either (ParseError Char e) a
parsewithString p = runParser p ""

parseWithState :: Monad m => st -> TextParser (StateT st m) a -> Text -> m (Either (ParseError Char Dec) a)
parseWithState ctx p s = evalStateT (runParserT p "" s) ctx

parseWithState' :: st -> ParsecT e s (State st) a -> s -> (Either (ParseError (Token s) e) a)
parseWithState' ctx p s = evalState (runParserT p "" s) ctx

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

spacenonewline :: (Stream s, Char ~ Token s) => ParsecT Dec s m Char
spacenonewline = satisfy (`elem` " \v\f\t")

restofline :: TextParser m String
restofline = anyChar `manyTill` newline

eolof :: TextParser m ()
eolof = (newline >> return ()) <|> eof
