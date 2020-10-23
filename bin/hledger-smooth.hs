#!/usr/bin/env stack
-- stack script --compile --resolver lts-16.3

-- This is an unfinished prototype, see https://github.com/simonmichael/hledger/issues/1171
-- Requires a contemporaneous version of the hledger package.
-- Requires journal entries to be sorted by date.
-- Run it inside an up to date hledger source tree, eg: bin/hledger-smooth.hs ACCT
-- Or add bin/ to $PATH and [stack ghc bin/hledger-smooth;] hledger smooth ACCT

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-name-shadowing #-}

import Data.List
import Data.Maybe
import Data.String.QQ (s)
import qualified Data.Text as T
import Data.Time.Calendar
import Safe
-- import Hledger
import Hledger.Cli

------------------------------------------------------------------------------
cmdmode = hledgerCommandMode
  [s| smooth
Like the print command, but splits any posting to ACCT (a full account name)
into multiple daily postings having a similar overall effect.

Each posting is smoothed across the period until the next ACCT posting, and
the last one is smoothed until the report end date, or today.
Eg: $30 on 1/1 and $50 on 1/4, if smoothed on 1/6 with no end date specified,
becomes $10 on 1/1, $10 on 1/2, $10 on 1/3, $25 on 1/4, $25 on 1/5.

The last new posting's amount is left blank to ensure a balanced transaction.
It can differ from the others.

Useful for preprocessing a journal to smooth out irregular revenues or
expenses in daily/weekly/monthly reports, eg:
hledger smooth revenues:consulting | hledger -f- incomestatement -W

_FLAGS
  |]
  []
  [generalflagsgroup1]
  []
  ([], Just $ argsFlag "ACCT")
------------------------------------------------------------------------------
-- we could smooth postings across the journal period, or within standard intervals: --smooth-interval=posting|journal|weekly|monthly|...
-- we could perhaps split transactions instead: --smooth-split=postings|transactions

main :: IO ()
main = do
  copts@CliOpts{reportopts_=ropts, rawopts_} <- getHledgerCliOpts cmdmode
  let copts' = copts{
        -- One of our postings will probably have a missing amount; this ensures it's
        -- explicit on all the others.
        rawopts_=setboolopt "explicit" rawopts_
        -- Don't let our ACCT argument be interpreted as a query by print
        ,reportopts_=ropts{query_=""}
        }
  withJournalDo copts' $ \j -> do
    today <- getCurrentDay
    let
      menddate = reportPeriodLastDay ropts
      args = words' $ query_ ropts
      q = queryFromOpts today ropts
      acct = T.pack $ headDef (error' "Please provide an account name argument") args
      pr = postingsReport ropts (And [Acct $ accountNameToAccountRegexCI acct, q]) j

      -- dates of postings to acct (in report)
      pdates = map (postingDate . fourth5) (snd pr)
      -- the specified report end date or today's date
      enddate = fromMaybe today menddate
      dates = pdates ++ [enddate]
      (_,ts') = mapAccumL (splitTransactionPostings q acct) dates $ jtxns j
      j' = j{jtxns=ts'}
    print' copts' j'

-- | Split a transaction's postings to acct, if the transaction is matched by q,
-- into equivalent daily postings up to the next given end date,
-- keeping track of remaining end dates.
splitTransactionPostings :: Query -> AccountName -> [Day] -> Transaction -> ([Day], Transaction)
splitTransactionPostings _q acct dates t
  -- | q `matchesTransaction` t = (dates', t')
  -- | otherwise                = (dates, t)
  | otherwise                = (dates', t')
  where
    (dates', pss') = mapAccumL (splitPosting acct) dates $ tpostings t
    t' = txnTieKnot t{tpostings=concat pss'}

-- | Split a posting to acct into equivalent daily postings
-- up to the next given end date, keeping track of remaining end dates.
-- We assume we will see postings in number and order corresponding the given end dates.
splitPosting :: AccountName -> [Day] -> Posting -> ([Day], [Posting])
splitPosting acct dates p@Posting{paccount,pamount}
  | paccount == acct = (dates', ps')
  | otherwise        = (dates, [p])
  where
    start = dbg4 "start" $ postingDate p
    (end, dates') =
      case dbg4 "dates" dates of
        -- XXX fragile, breaks if transactions are not date-ordered
        (d1:d2:ds) -> if d1==start then (d2, d2:ds) else error' "splitPosting got wrong date, should not happen (maybe sort your transactions by date)"
        [d]        -> (d, [])
        []         -> error' "splitPosting ran out of dates, should not happen (maybe sort your transactions by date)"
    days = initSafe [start..end]
    amt  = (genericLength days) `divideMixedAmount` pamount
    -- give one of the postings an exact balancing amount to ensure the transaction is balanced
    -- lastamt = pamount - ptrace (amt `multiplyMixedAmount` (fromIntegral $ length days))
    lastamt = missingmixedamt
    daysamts = zip days (take (length days - 1) (repeat amt) ++ [lastamt])
    ps'  = [postingSetDate (Just d) p{pamount=a} | (d,a) <- daysamts ]

-- | Set a posting's (primary) date, as if it had been parsed from the journal entry:
-- Updates the date field,
-- adds a "date" tag to the parsed tag list (replacing any existing "date" tags there),
-- and adds the "date" tag to the unparsed comment field as well, for display purposes.
-- If the date is Nothing, unsets the date and removes it from the tags list.
-- Does not remove existing date tags from the comment field.
postingSetDate :: Maybe Day -> Posting -> Posting
postingSetDate md p@Posting{ptags,pcomment} = p{pdate=md, ptags=ptags'', pcomment=pcomment'}
  where
    ptags'' = case md of
                Nothing -> ptags'
                Just d  -> ptags'++[("date", T.pack $ show d)]
      where
        ptags' = filter (not.(=="date").fst) ptags

    pcomment' = case md of
                  Nothing -> pcomment
                  Just d  -> commentAddTag pcomment ("date:", T.pack $ show d)
