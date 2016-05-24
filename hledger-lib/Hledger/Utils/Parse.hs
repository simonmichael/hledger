{-# LANGUAGE FlexibleContexts #-}
module Hledger.Utils.Parse where

import Data.Char
import Data.List
-- import Data.Text (Text)
-- import qualified Data.Text as T
import Text.Parsec
import Text.Printf

import Hledger.Utils.UTF8IOCompat (error')

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choice' :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
choice' = choice . map Text.Parsec.try

parsewith :: Parsec [Char] () a -> String -> Either ParseError a
parsewith p = runParser p () ""

parseWithState :: Stream s m t => u -> ParsecT s u m a -> s -> m (Either ParseError a)
parseWithState jps p = runParserT p jps ""

fromparse :: Either ParseError a -> a
fromparse = either parseerror id

parseerror :: ParseError -> a
parseerror e = error' $ showParseError e

showParseError :: ParseError -> String
showParseError e = "parse error at " ++ show e

showDateParseError :: ParseError -> String
showDateParseError e = printf "date parse error (%s)" (intercalate ", " $ tail $ lines $ show e)

nonspace :: (Stream s m Char) => ParsecT s st m Char
nonspace = satisfy (not . isSpace)

spacenonewline :: (Stream s m Char) => ParsecT s st m Char
spacenonewline = satisfy (`elem` " \v\f\t")

restofline :: (Stream s m Char) => ParsecT s st m String
restofline = anyChar `manyTill` newline

eolof :: (Stream s m Char) => ParsecT s st m ()
eolof = (newline >> return ()) <|> eof

