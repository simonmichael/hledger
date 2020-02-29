-- * -*- eval: (orgstruct-mode 1); orgstruct-heading-prefix-regexp:"-- "; -*-
-- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

A reader for the "timedot" file format.
Example:

@
#DATE
#ACCT  DOTS  # Each dot represents 15m, spaces are ignored
#ACCT  8    # numbers with or without a following h represent hours
#ACCT  5m   # numbers followed by m represent minutes

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

-- ** language
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

-- ** exports
module Hledger.Read.TimedotReader (
  -- * Reader
  reader,
  -- * Misc other exports
  timedotfilep,
)
where

-- ** imports
import Prelude ()
import "base-compat-batteries" Prelude.Compat
import Control.Monad
import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict
import Data.Char (isSpace)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char

import Hledger.Data
import Hledger.Read.Common hiding (emptyorcommentlinep)
import Hledger.Utils

-- ** reader

reader :: Reader
reader = Reader
  {rFormat     = "timedot"
  ,rExtensions = ["timedot"]
  ,rParser     = parse
  ,rExperimental = False
  }

-- | Parse and post-process a "Journal" from the timedot format, or give an error.
parse :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal
parse = parseAndFinaliseJournal' timedotp

-- ** utilities

traceparse :: String -> TextParser m ()
traceparse = const $ return ()
-- traceparse = traceParse  -- for debugging

-- ** parsers
{-
Rough grammar for timedot format:

timedot:           preamble day*
preamble:          (emptyline | commentline | orgheading)*
orgheading:        orgheadingprefix restofline
day:               dateline entry* (emptyline | commentline)*
dateline:          orgheadingprefix? date description?
orgheadingprefix:  star+ space+
description:       restofline  ; till semicolon?
entry:          orgheadingprefix? space* singlespaced (doublespace quantity?)?
doublespace:       space space+
quantity:          (dot (dot | space)* | number | number unit)

Date lines and item lines can begin with an org heading prefix, which is ignored.
Org headings before the first date line are ignored, regardless of content.
-}

timedotfilep = timedotp -- XXX rename export above

timedotp :: JournalParser m ParsedJournal
timedotp = preamblep >> many dayp >> eof >> get

preamblep :: JournalParser m ()
preamblep = do
  lift $ traceparse "preamblep"
  void $ many $ notFollowedBy datelinep >> (lift $ emptyorcommentlinep "#;*")

-- | XXX new comment line parser, move to Hledger.Read.Common.emptyorcommentlinep
-- Parse empty lines, all-blank lines, and lines beginning with any of the provided
-- comment-beginning characters.
emptyorcommentlinep :: [Char] -> TextParser m ()
emptyorcommentlinep cs =
  label ("empty line or comment line beginning with "++cs) $ do
    traceparse "emptyorcommentlinep" -- XXX possible to combine label and traceparse ?
    skipMany spacenonewline
    void newline <|> void commentp
    where
      commentp = do
        choice (map (some.char) cs)
        takeWhileP Nothing (/='\n') <* newline

-- | Parse timedot day entries to zero or more time transactions for that day.
-- @
-- 2020/2/1 optional day description
-- fos.haskell  .... ..
-- biz.research .
-- inc.client1  .... .... .... .... .... ....
-- @
dayp :: JournalParser m ()
dayp = label "timedot day entry" $ do
  lift $ traceparse "dayp"
  (d,desc) <- datelinep
  ts <- many entryp
  let ts' = map (\t -> t{tdate=d, tdescription=desc}) ts
  modify' $ addTransactions ts'
  void $ many $
    (lift $ emptyorcommentlinep "#;") <|> orgnondatelinep
  where
    addTransactions :: [Transaction] -> Journal -> Journal
    addTransactions ts j = foldl' (flip ($)) j (map addTransaction ts)

datelinep :: JournalParser m (Day,Text)
datelinep = do
  lift $ traceparse "datelinep"
  lift $ optional orgheadingprefixp
  d <- datep
  desc <- strip <$> lift restofline
  return (d, T.pack desc)

orgnondatelinep :: JournalParser m ()
orgnondatelinep = do
  lift $ traceparse "orgnondatelinep"
  notFollowedBy datelinep
  lift orgheadingprefixp
  void $ lift restofline

orgheadingprefixp = skipSome (char '*') >> skipSome spacenonewline

-- | Parse a single timedot entry to one (dateless) transaction.
-- @
-- fos.haskell  .... ..
-- @
entryp :: JournalParser m Transaction
entryp = do
  lift $ traceparse "  entryp"
  pos <- genericSourcePos <$> getSourcePos
  lift $ optional $ choice [orgheadingprefixp, skipSome spacenonewline]
  a <- modifiedaccountnamep
  lift (skipMany spacenonewline)
  hours <-
    try (lift followingcommentp >> return 0)
    <|> (durationp <*
         (try (lift followingcommentp) <|> (newline >> return "")))
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

durationp :: JournalParser m Quantity
durationp = try numericquantityp <|> dotquantityp

-- | Parse a duration of seconds, minutes, hours, days, weeks, months or years,
-- written as a decimal number followed by s, m, h, d, w, mo or y, assuming h
-- if there is no unit. Returns the duration as hours, assuming
-- 1m = 60s, 1h = 60m, 1d = 24h, 1w = 7d, 1mo = 30d, 1y=365d.
-- @
-- 1.5
-- 1.5h
-- 90m
-- @
numericquantityp :: JournalParser m Quantity
numericquantityp = do
  (q, _, _, _) <- lift $ numberp Nothing
  msymbol <- optional $ choice $ map (string . fst) timeUnits
  lift (skipMany spacenonewline)
  let q' =
        case msymbol of
          Nothing  -> q
          Just sym ->
            case lookup sym timeUnits of
              Just mult -> q * mult
              Nothing   -> q  -- shouldn't happen.. ignore
  return q'

-- (symbol, equivalent in hours).
timeUnits =
  [("s",2.777777777777778e-4)
  ,("mo",5040) -- before "m"
  ,("m",1.6666666666666666e-2)
  ,("h",1)
  ,("d",24)
  ,("w",168)
  ,("y",61320)
  ]

-- | Parse a quantity written as a line of dots, each representing 0.25.
-- @
-- .... ..
-- @
dotquantityp :: JournalParser m Quantity
dotquantityp = do
  dots <- filter (not.isSpace) <$> many (oneOf (". " :: [Char]))
  return $ (/4) $ fromIntegral $ length dots
