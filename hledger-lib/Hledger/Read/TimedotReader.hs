{-|

A reader for the "timedot" file format.
Example:

@
#DATE
#ACCT DOTS  # Each dot represents 15m, spaces are ignored

# on 2/1, 1h was spent on FOSS haskell work, 0.25h on research, etc.
2/1
fos.haskell   .... ..
biz.research  .
inc.client1   .... .... .... .... .... ....

2/2
biz.research  .
inc.client1   .... .... ..

@

-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Read.TimedotReader (
  -- * Reader
  reader,
  -- * Misc other exports
  timedotfilep,
  -- * Tests
  tests_Hledger_Read_TimedotReader
)
where
import Prelude ()
import Prelude.Compat
import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Test.HUnit
import Text.Megaparsec hiding (parse)
import System.FilePath

import Hledger.Data
import Hledger.Read.Common
import Hledger.Utils hiding (ptrace)

-- easier to toggle this here sometimes
-- import qualified Hledger.Utils (ptrace)
-- ptrace = Hledger.Utils.ptrace
ptrace = return

reader :: Reader
reader = Reader format detect parse

format :: String
format = "timedot"

-- | Does the given file path and data look like it might contain this format ?
detect :: FilePath -> Text -> Bool
detect f t
  | f /= "-"  = takeExtension f == '.':format -- from a file: yes if the extension matches the format name
  | otherwise = regexMatches "(^|\n)[0-9]" $ T.unpack t  -- from stdin: yes if we can see a possible timedot day entry (digits in column 0)

-- | Parse and post-process a "Journal" from the timedot format, or give an error.
parse :: Maybe FilePath -> Bool -> FilePath -> Text -> ExceptT String IO Journal
parse _ = parseAndFinaliseJournal timedotfilep

timedotfilep :: ErroringJournalParser ParsedJournal
timedotfilep = do many timedotfileitemp
                  eof
                  get
    where
      timedotfileitemp :: ErroringJournalParser ()
      timedotfileitemp = do
        ptrace "timedotfileitemp"
        choice [
          void emptyorcommentlinep
         ,timedotdayp >>= \ts -> modify' (addTransactions ts)
         ] <?> "timedot day entry, or default year or comment line or blank line"

addTransactions :: [Transaction] -> Journal -> Journal
addTransactions ts j = foldl' (flip ($)) j (map addTransaction ts)

-- | Parse timedot day entries to zero or more time transactions for that day.
-- @
-- 2/1
-- fos.haskell  .... ..
-- biz.research .
-- inc.client1  .... .... .... .... .... ....
-- @
timedotdayp :: ErroringJournalParser [Transaction]
timedotdayp = do
  ptrace " timedotdayp"
  d <- datep <* eolof
  es <- catMaybes <$> many (const Nothing <$> try emptyorcommentlinep <|>
                            Just <$> (notFollowedBy datep >> timedotentryp))
  return $ map (\t -> t{tdate=d}) es -- <$> many timedotentryp

-- | Parse a single timedot entry to one (dateless) transaction.
-- @
-- fos.haskell  .... ..
-- @
timedotentryp :: ErroringJournalParser Transaction
timedotentryp = do
  ptrace "  timedotentryp"
  pos <- genericSourcePos <$> getPosition
  many spacenonewline
  a <- modifiedaccountnamep
  many spacenonewline
  hours <-
    try (followingcommentp >> return 0)
    <|> (timedotdurationp <*
         (try followingcommentp <|> (newline >> return "")))
  let t = nulltransaction{
        tsourcepos = pos,
        tstatus    = Cleared,
        tpostings  = [
          nullposting{paccount=a
                     ,pamount=Mixed [setAmountPrecision 2 $ num hours]  -- don't assume hours; do set precision to 2
                     ,ptype=VirtualPosting
                     ,ptransaction=Just t
                     }
          ]
        }
  return t

timedotdurationp :: ErroringJournalParser Quantity
timedotdurationp = try timedotnumberp <|> timedotdotsp

-- | Parse a duration written as a decimal number of hours (optionally followed by the letter h).
-- @
-- 1.5h
-- @
timedotnumberp :: ErroringJournalParser Quantity
timedotnumberp = do
   (q, _, _, _) <- numberp
   many spacenonewline
   optional $ char 'h'
   many spacenonewline
   return q

-- | Parse a quantity written as a line of dots, each representing 0.25.
-- @
-- .... ..
-- @
timedotdotsp :: ErroringJournalParser Quantity
timedotdotsp = do
  dots <- filter (not.isSpace) <$> many (oneOf (". " :: [Char]))
  return $ (/4) $ fromIntegral $ length dots

tests_Hledger_Read_TimedotReader = TestList [
 ]

