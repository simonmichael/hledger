--- * -*- outline-regexp:"--- \\*"; -*-
--- ** doc
-- In Emacs, use TAB on lines beginning with "-- *" to collapse/expand sections.
{-|

A reader for the "timedot" file format.
Example:

@
;DATE
;ACCT  DOTS  # Each dot represents 15m, spaces are ignored
;ACCT  8    # numbers with or without a following h represent hours
;ACCT  5m   # numbers followed by m represent minutes

; on 2/1, 1h was spent on FOSS haskell work, 0.25h on research, etc.
2/1
fos.haskell   .... ..
biz.research  .
inc.client1   .... .... .... .... .... ....

2/2
biz.research  .
inc.client1   .... .... ..

@

-}

--- ** language
{-# LANGUAGE OverloadedStrings #-}

--- ** exports
module Hledger.Read.TimedotReader (
  -- * Reader
  reader,
  -- * Misc other exports
  timedotfilep,
)
where

--- ** imports
import Control.Monad
import Control.Monad.Except (ExceptT, liftEither)
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

--- ** doctest setup
-- $setup
-- >>> :set -XOverloadedStrings

--- ** reader

reader :: MonadIO m => Reader m
reader = Reader
  {rFormat     = "timedot"
  ,rExtensions = ["timedot"]
  ,rReadFn     = parse
  ,rParser    = timedotp
  }

-- | Parse and post-process a "Journal" from the timedot format, or give an error.
parse :: InputOpts -> FilePath -> Text -> ExceptT String IO Journal
parse iopts fp t = initialiseAndParseJournal timedotp iopts fp t
                   >>= liftEither . journalApplyAliases (aliasesFromOpts iopts)
                   >>= journalFinalise iopts fp t

--- ** utilities

traceparse, traceparse' :: String -> TextParser m ()
traceparse  = const $ return ()
traceparse' = const $ return ()
-- for debugging:
-- traceparse  s = traceParse (s++"?")
-- traceparse' s = trace s $ return ()

--- ** parsers
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
  many $ notFollowedBy datelinep >> (lift $ emptyorcommentlinep "#;*")
  lift $ traceparse' "preamblep"

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
  (date,desc,comment,tags) <- datelinep
  commentlinesp
  ts <- many $ entryp <* commentlinesp
  modify' $ addTransactions $ map (\t -> t{tdate=date, tdescription=desc, tcomment=comment, ttags=tags}) ts
  lift $ traceparse' "dayp"
  where
    addTransactions :: [Transaction] -> Journal -> Journal
    addTransactions ts j = foldl' (flip ($)) j (map addTransaction ts)

datelinep :: JournalParser m (Day,Text,Text,[Tag])
datelinep = do
  lift $ traceparse "datelinep"
  lift $ optional orgheadingprefixp
  date <- datep
  desc <- T.strip <$> lift descriptionp
  (comment, tags) <- lift transactioncommentp
  lift $ traceparse' "datelinep"
  return (date, desc, comment, tags)

-- | Zero or more empty lines or hash/semicolon comment lines
-- or org headlines which do not start a new day.
commentlinesp :: JournalParser m ()
commentlinesp = do
  lift $ traceparse "commentlinesp"
  void $ many $ try $ lift $ emptyorcommentlinep "#;"

-- orgnondatelinep :: JournalParser m ()
-- orgnondatelinep = do
--   lift $ traceparse "orgnondatelinep"
--   lift orgheadingprefixp
--   notFollowedBy datelinep
--   void $ lift restofline
--   lift $ traceparse' "orgnondatelinep"

orgheadingprefixp = do
  -- traceparse "orgheadingprefixp"
  skipSome (char '*') >> skipNonNewlineSpaces1

-- | Parse a single timedot entry to one (dateless) transaction.
-- @
-- fos.haskell  .... ..
-- @
entryp :: JournalParser m Transaction
entryp = do
  lift $ traceparse "entryp"
  pos <- getSourcePos
  notFollowedBy datelinep
  lift $ optional $ choice [orgheadingprefixp, skipNonNewlineSpaces1]
  a <- modifiedaccountnamep
  lift skipNonNewlineSpaces
  (hours, comment, tags) <-
    try (do
      (c,ts) <- lift transactioncommentp  -- or postingp, but let's not bother supporting date:/date2:
      return (0, c, ts)
    )
    <|> (do
      h <- lift durationp
      (c,ts) <- try (lift transactioncommentp) <|> (newline >> return ("",[]))
      return (h,c,ts)
    )
  mcs <- getDefaultCommodityAndStyle
  let 
    (c,s) = case mcs of
      Just (defc,defs) -> (defc, defs{asprecision=max (asprecision defs) (Precision 2)})
      _ -> ("", amountstyle{asprecision=Precision 2})
    t = nulltransaction{
          tsourcepos = (pos, pos),
          tstatus    = Cleared,
          tpostings  = [
            nullposting{paccount=a
                      ,pamount=mixedAmount $ nullamt{acommodity=c, aquantity=hours, astyle=s}
                      ,ptype=VirtualPosting
                      ,pcomment=comment
                      ,ptags=tags
                      ,ptransaction=Just t
                      }
            ]
          }
  lift $ traceparse' "entryp"
  return t

type Hours = Quantity

durationp :: TextParser m Hours
durationp = do
  traceparse "durationp"
  try numericquantityp <|> dotquantityp
    -- <* traceparse' "durationp"

-- | Parse a duration of seconds, minutes, hours, days, weeks, months or years,
-- written as a decimal number followed by s, m, h, d, w, mo or y, assuming h
-- if there is no unit. Returns the duration as hours, assuming
-- 1m = 60s, 1h = 60m, 1d = 24h, 1w = 7d, 1mo = 30d, 1y=365d.
-- @
-- 1.5
-- 1.5h
-- 90m
-- @
numericquantityp :: TextParser m Hours
numericquantityp = do
  -- lift $ traceparse "numericquantityp"
  (q, _, _, _) <- numberp Nothing
  msymbol <- optional $ choice $ map (string . fst) timeUnits
  skipNonNewlineSpaces
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
dotquantityp :: TextParser m Quantity
dotquantityp = do
  -- lift $ traceparse "dotquantityp"
  dots <- filter (not.isSpace) <$> many (oneOf (". " :: [Char]))
  return $ fromIntegral (length dots) / 4

-- | XXX new comment line parser, move to Hledger.Read.Common.emptyorcommentlinep
-- Parse empty lines, all-blank lines, and lines beginning with any of the provided
-- comment-beginning characters.
emptyorcommentlinep :: [Char] -> TextParser m ()
emptyorcommentlinep cs =
  label ("empty line or comment line beginning with "++cs) $ do
    traceparse "emptyorcommentlinep" -- XXX possible to combine label and traceparse ?
    skipNonNewlineSpaces
    void newline <|> void commentp
    traceparse' "emptyorcommentlinep"
    where
      commentp = do
        choice (map (some.char) cs)
        takeWhileP Nothing (/='\n') <* newline

