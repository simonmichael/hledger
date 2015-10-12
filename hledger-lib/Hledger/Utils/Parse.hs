{-# LANGUAGE FlexibleContexts #-}
module Hledger.Utils.Parse where

import Data.Char
import Data.List
import Text.Megaparsec
import Text.Printf

import Control.Monad.State (StateT, evalStateT)

import Hledger.Utils.UTF8IOCompat (error')

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choice' :: Stream s t => [ParsecT s m a] -> ParsecT s m a
choice' = choice . map Text.Megaparsec.try

parsewith :: Parsec [Char] a -> String -> Either ParseError a
parsewith p = runParser p ""

parseWithCtx :: (Monad m, Stream s t) => st -> ParsecT s (StateT st m) a -> s -> m (Either ParseError a)
parseWithCtx ctx p s = evalStateT (runParserT p "" s) ctx

fromparse :: Either ParseError a -> a
fromparse = either parseerror id

parseerror :: ParseError -> a
parseerror e = error' $ showParseError e

showParseError :: ParseError -> String
showParseError e = "parse error at " ++ show e

showDateParseError :: ParseError -> String
showDateParseError e = printf "date parse error (%s)" (intercalate ", " $ tail $ lines $ show e)

nonspace :: (Stream [Char] Char) => ParsecT [Char] m Char
nonspace = satisfy (not . isSpace)

spacenonewline :: (Stream [Char] Char) => ParsecT [Char] m Char
spacenonewline = satisfy (`elem` " \v\f\t")

restofline :: (Stream [Char] Char) => ParsecT [Char] m String
restofline = anyChar `manyTill` newline

eolof :: (Stream [Char] Char) => ParsecT [Char] m ()
eolof = (newline >> return ()) <|> eof

