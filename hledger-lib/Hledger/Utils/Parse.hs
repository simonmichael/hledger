{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Hledger.Utils.Parse where

import Data.Char
import Data.List
import Text.Megaparsec hiding (State)
import Text.Printf

import Control.Monad.State.Strict (StateT, State, evalStateT, evalState)

import Hledger.Utils.UTF8IOCompat (error')

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choice' :: Stream s => [ParsecT Dec s m a] -> ParsecT Dec s m a
choice' = choice . map Text.Megaparsec.try

parsewith :: Parsec e s a -> s -> Either (ParseError (Token s) e) a
parsewith p = runParser p ""

parseWithState :: Monad m => st -> ParsecT e s (StateT st m) a -> s -> m (Either (ParseError (Token s) e) a)
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

nonspace :: (Stream s, Char ~ Token s) => ParsecT Dec s m Char
nonspace = satisfy (not . isSpace)

spacenonewline :: (Stream s, Char ~ Token s) => ParsecT Dec s m Char
spacenonewline = satisfy (`elem` " \v\f\t")

restofline :: (Stream s, Char ~ Token s) => ParsecT Dec s m String
restofline = anyChar `manyTill` newline

eolof :: (Stream s, Char ~ Token s) => ParsecT Dec s m ()
eolof = (newline >> return ()) <|> eof
