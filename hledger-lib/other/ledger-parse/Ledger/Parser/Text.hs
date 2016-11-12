{-# LANGUAGE OverloadedStrings #-}

module Ledger.Parser.Text
       ( parseJournalFile
       , RawJournal(..)
       , RawEntity(..)
       , RawEntityInSitu(..)
       , RawPosting(..)
       , RawTransaction(..)
       , RawAutoTxn(..)
       , RawPeriodTxn(..)
       -- , main
       ) where

import           Control.Applicative
import           Data.ByteString as B hiding (pack, unpack, singleton,
                                              zipWith, concat)
import           Data.Maybe
import qualified Data.Text.Encoding as E
import           Filesystem.Path.CurrentOS hiding (concat)
import           Prelude hiding (FilePath, readFile, until)
import           Text.Parser.Combinators
import           Text.Parser.LookAhead
import           Text.Parser.Token
import           Text.Trifecta
import           Text.Trifecta.Delta
-- import Control.DeepSeq
-- import Criterion
-- import Criterion.Main

infixl 4 <$!>

(<$!>) :: TokenParsing m => (a -> b) -> m a -> m b
f <$!> ma = ($!) <$> pure f <*> ma

data RawJournal = RawJournal [RawEntity]
                deriving (Show, Eq)

data RawEntity = Whitespace String
               | FileComment String
               | Directive { directiveChar :: Maybe Char
                           , directiveName :: !String
                           , directiveArg  :: Maybe String }
               | RawTransactionEntity RawTransaction
               | RawAutoTxnEntity RawAutoTxn
               | RawPeriodTxnEntity RawPeriodTxn
               | EndOfFile
               deriving (Show, Eq)

data RawEntityInSitu = RawEntityInSitu { rawEntityIndex    :: !Int
                                       , rawEntityStartPos :: !Rendering
                                       , rawEntity         :: !RawEntity
                                       , rawEntityEndPos   :: !Rendering }

instance Show RawEntityInSitu where
  show x = show (rawEntity x) ++ "\n"

data RawPosting = RawPosting { rawPostState   :: Maybe Char
                             , rawPostAccount :: !String
                             , rawPostAmount  :: Maybe String
                             , rawPostNote    :: Maybe String }
                | RawPostingNote !String
                deriving (Show, Eq)

data RawTransaction = RawTransaction { rawTxnDate    :: !String
                                     , rawTxnDateAux :: Maybe String
                                     , rawTxnState   :: Maybe Char
                                     , rawTxnCode    :: Maybe String
                                     , rawTxnDesc    :: !String
                                     , rawTxnNote    :: Maybe String
                                     , rawTxnPosts   :: ![RawPosting] }
                    deriving (Show, Eq)

data RawAutoTxn = RawAutoTxn { rawATxnQuery :: !String
                             , rawATxnPosts :: ![RawPosting] }
                deriving (Show, Eq)

data RawPeriodTxn = RawPeriodTxn { rawPTxnPeriod :: !String
                                 , rawPTxnPosts  :: ![RawPosting] }
                  deriving (Show, Eq)

txnDateParser :: TokenParsing m => m String
txnDateParser = some (digit <|> oneOf "/-." <|> letter)
                <?> "transaction date"

longSep :: CharParsing m => m ()
longSep = () <$ (try (char ' ' *> char ' ') <|> tab)

noteParser :: (LookAheadParsing m, CharParsing m) => m String
noteParser = char ';' *> manyTill anyChar (try (lookAhead endOfLine))
             <?> "note"

longSepOrEOL :: (LookAheadParsing m, CharParsing m) => m ()
longSepOrEOL = try (lookAhead (longSep <|> endOfLine))

longSepOrEOLIf :: (LookAheadParsing m, CharParsing m) => m p -> m ()
longSepOrEOLIf p = try (lookAhead ((() <$ longSep <* p) <|> endOfLine))

until :: CharParsing m => m () -> m String
until end = (:) <$> noneOf "\r\n" <*> manyTill anyChar end

tokenP :: TokenParsing m => m p -> m p
tokenP p = p <* skipMany spaceChars

postingParser :: (LookAheadParsing m, TokenParsing m) => m RawPosting
postingParser =
  (RawPosting <$!> (some spaceChars *>
                   optional (tokenP (char '*' <|> char '!')))
              <*> tokenP (until longSepOrEOL)
              <*> optional (tokenP (until (longSepOrEOLIf (char ';'))))
              <*> (optional noteParser <* endOfLine)
              <?> "posting")
  <|>
  (RawPostingNote <$!> (concat <$!>
                        some ((++) <$!> (some spaceChars *> noteParser)
                                   <*> ((:[]) <$> endOfLineChar)))
                  <?> "posting note")

spaceChars :: CharParsing m => m ()
spaceChars = () <$ oneOf " \t"

regularTxnParser :: (LookAheadParsing m, TokenParsing m) => m RawEntity
regularTxnParser = RawTransactionEntity <$!> go
  where go = RawTransaction
             <$!> txnDateParser
             <*> optional (char '=' *> txnDateParser)
             <*> (many spaceChars *>
                  optional (tokenP (char '*' <|> char '!')))
             <*> optional
                 (tokenP (parens (many (noneOf ")\r\n"))))
             <*> tokenP (until (longSepOrEOLIf (char ';')))
             <*> optional noteParser
             <*> (endOfLine *> some postingParser)
             <?> "regular transaction"

automatedTxnParser :: (LookAheadParsing m, TokenParsing m) => m RawEntity
automatedTxnParser = RawAutoTxnEntity <$!> go
  where go = RawAutoTxn
             <$!> (tokenP (char '=') *>
                   manyTill anyChar (try (lookAhead endOfLine)))
             <*> (endOfLine *> some postingParser)
             <?> "automated transaction"

periodicTxnParser :: (LookAheadParsing m, TokenParsing m) => m RawEntity
periodicTxnParser = RawPeriodTxnEntity <$!> go
  where go = RawPeriodTxn
             <$!> (tokenP (char '~') *>
                   manyTill anyChar (try (lookAhead endOfLine)))
             <*> (endOfLine *> some postingParser)
             <?> "periodic transaction"

transactionParser :: (LookAheadParsing m, TokenParsing m) => m RawEntity
transactionParser = regularTxnParser
                    <|> automatedTxnParser
                    <|> periodicTxnParser
                    <?> "transaction"

directiveParser :: (LookAheadParsing m, TokenParsing m) => m RawEntity
directiveParser =
  Directive <$!> optional (oneOf "@!")
            <*> ((:) <$!> letter <*> tokenP (many alphaNum))
            <*> (optional
                 ((:) <$!> noneOf "\r\n"
                      <*> manyTill anyChar (try (lookAhead endOfLine)))
                 <* endOfLine)
            <?> "directive"

endOfLine :: CharParsing m => m ()
endOfLine = () <$ endOfLineChar

endOfLineChar :: CharParsing m => m Char
endOfLineChar = skipOptional (char '\r') *> char '\n'

commentParser :: (LookAheadParsing m, TokenParsing m) => m RawEntity
commentParser = FileComment
                <$!> (concat <$!>
                      some ((++) <$!> noteParser
                                 <*> ((:[]) <$> endOfLineChar)))
                <?> "comment"

whitespaceParser :: TokenParsing m => m RawEntity
whitespaceParser = Whitespace <$!> some space <?> "whitespace"

entityParser :: (LookAheadParsing m, TokenParsing m) => m RawEntity
entityParser = directiveParser
               <|> commentParser
               <|> whitespaceParser
               <|> transactionParser
               <?> "journal"

rendCaret :: DeltaParsing m => m Rendering
rendCaret = addCaret <$!> position <*> rend

journalParser :: (LookAheadParsing m, DeltaParsing m) => m [RawEntityInSitu]
journalParser =
  many (RawEntityInSitu <$!> pure 0 <*> rendCaret <*> entityParser <*> rendCaret)

parseJournalFile :: FilePath -> ByteString -> Result [RawEntityInSitu]
parseJournalFile file contents =
  let filepath = either id id $ toText file
      start    = Directed (E.encodeUtf8 filepath) 0 0 0 0
  in zipWith (\e i -> e { rawEntityIndex = i})
       <$> parseByteString journalParser start contents
       <*> pure [1..]

-- testme :: IO (Result [RawEntityInSitu])
-- testme =
--   let file = "/Users/johnw/Documents/Finances/ledger.dat"
--   in parseJournalFile (fromText (T.pack file)) <$> B.readFile file

-- instance NFData RawEntityInSitu
-- instance NFData (Result a)

-- main = do let file = "/Users/johnw/Documents/Finances/ledger.dat"
--           bs <- B.readFile file
--           defaultMain [
--             bench "main" $ nf (parseJournalFile (fromText (T.pack file))) bs ]

-- Text.hs ends here
