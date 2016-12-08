{-# LANGUAGE FlexibleContexts, TypeFamilies #-}
module Hledger.Utils.Parse where

import Control.Monad.Except
import Data.Char
import Data.List
import Data.Text (Text)
import Text.Megaparsec hiding (State)
import Data.Functor.Identity (Identity(..))
import Text.Printf

import Control.Monad.State.Strict (StateT, evalStateT)

import Hledger.Data.Types
import Hledger.Utils.UTF8IOCompat (error')

-- | A parser of strict text with generic user state, monad and return type.
type TextParser m a = ParsecT Dec Text m a

type JournalStateParser m a = StateT Journal (ParsecT Dec Text m) a

type JournalParser a = StateT Journal (ParsecT Dec Text Identity) a

-- | A journal parser that runs in IO and can throw an error mid-parse.
type ErroringJournalParser a = StateT Journal (ParsecT Dec Text (ExceptT String IO)) a

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choice' :: [TextParser m a] -> TextParser m a
choice' = choice . map Text.Megaparsec.try

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
{-# INLINABLE choiceInState #-}
choiceInState :: [StateT s (ParsecT Dec Text m) a] -> StateT s (ParsecT Dec Text m) a
choiceInState = choice . map Text.Megaparsec.try

parsewith :: Parsec e Text a -> Text -> Either (ParseError Char e) a
parsewith p = runParser p ""

parsewithString :: Parsec e String a -> String -> Either (ParseError Char e) a
parsewithString p = runParser p ""

parseWithState :: Monad m => st -> StateT st (ParsecT Dec Text m) a -> Text -> m (Either (ParseError Char Dec) a)
parseWithState ctx p s = runParserT (evalStateT p ctx) "" s

parseWithState' :: (Stream s, ErrorComponent e) => st -> StateT st (ParsecT e s Identity) a -> s -> (Either (ParseError (Token s) e) a)
parseWithState' ctx p s = runParser (evalStateT p ctx) "" s

fromparse :: (Show t, Show e) => Either (ParseError t e) a -> a
fromparse = either parseerror id

parseerror :: (Show t, Show e) => ParseError t e -> a
parseerror e = error' $ showParseError e

showParseError :: (Show t, Show e) => ParseError t e -> String
showParseError e = "parse error at " ++ show e

showDateParseError :: (Show t, Show e) => ParseError t e -> String
showDateParseError e = printf "date parse error (%s)" (intercalate ", " $ tail $ lines $ show e)

{-# INLINABLE nonspace #-}
nonspace :: TextParser m Char
nonspace = satisfy (not . isSpace)

{-# INLINABLE spacenonewline #-}
spacenonewline :: (Stream s, Char ~ Token s) => ParsecT Dec s m Char
spacenonewline = oneOf " \v\f\t"

{-# INLINABLE restofline #-}
restofline :: TextParser m String
restofline = anyChar `manyTill` newline

{-# INLINABLE eolof #-}
eolof :: TextParser m ()
eolof = (newline >> return ()) <|> eof
