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

module Hledger.Read.TimedotReader (
  -- * Reader
  reader,
  -- * Tests
  tests_Hledger_Read_TimedotReader
)
where
import Prelude ()
import Prelude.Compat
import Control.Monad (liftM)
import Control.Monad.Except (ExceptT)
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Maybe
import Test.HUnit
import Text.Parsec hiding (parse)
import System.FilePath

import Hledger.Data
-- XXX too much reuse ?
import Hledger.Read.JournalReader (
  datep, numberp, defaultyeardirectivep, emptyorcommentlinep, followingcommentp,
  parseAndFinaliseJournal, modifiedaccountnamep, genericSourcePos
  )
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
detect :: FilePath -> String -> Bool
detect f _s
  | f /= "-"  = takeExtension f == '.':format  -- from a file: yes if the extension matches the format name
  | otherwise = False                          -- from stdin: yes if...

-- | Parse and post-process a "Journal" from the timedot format, or give an error.
parse :: Maybe FilePath -> Bool -> FilePath -> String -> ExceptT String IO Journal
parse _ = parseAndFinaliseJournal timedotfilep

timedotfilep :: ParsecT [Char] JournalContext (ExceptT String IO) (JournalUpdate, JournalContext)
timedotfilep = do items <- many timedotfileitemp
                  eof
                  ctx <- getState
                  return (liftM (foldl' (\acc new x -> new (acc x)) id) $ sequence items, ctx)
    where
      timedotfileitemp = do
        ptrace "timedotfileitemp"
        choice [
         defaultyeardirectivep,
         emptyorcommentlinep >> return (return id),
         liftM (return . addTransactions) timedotdayp
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
timedotdayp :: ParsecT [Char] JournalContext (ExceptT String IO) [Transaction]
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
timedotentryp :: ParsecT [Char] JournalContext (ExceptT String IO) Transaction
timedotentryp = do
  ptrace "  timedotentryp"
  pos <- genericSourcePos <$> getPosition
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

timedotdurationp :: ParsecT [Char] JournalContext (ExceptT String IO) Quantity
timedotdurationp = try timedotnumberp <|> timedotdotsp

-- | Parse a duration written as a decimal number of hours (optionally followed by the letter h).
-- @
-- 1.5h
-- @
timedotnumberp :: ParsecT [Char] JournalContext (ExceptT String IO) Quantity
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
timedotdotsp :: ParsecT [Char] JournalContext (ExceptT String IO) Quantity
timedotdotsp = do
  dots <- filter (not.isSpace) <$> many (oneOf ". ")
  return $ (/4) $ fromIntegral $ length dots

tests_Hledger_Read_TimedotReader = TestList [
 ]

