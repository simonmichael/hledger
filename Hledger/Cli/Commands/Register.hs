{-# LANGUAGE CPP #-}
{-| 

A ledger-compatible @register@ command.

-}

module Hledger.Cli.Commands.Register (
  RegisterReport
 ,RegisterReportItem
 ,register
 ,registerReport
 ,registerReportAsText
 ,showPostingWithBalanceForVty
 ,tests_Register
) where

import Safe (headMay, lastMay)
import Hledger.Data
import Hledger.Cli.Options
#if __GLASGOW_HASKELL__ <= 610
import Prelude hiding ( putStr )
import System.IO.UTF8
#endif
import Text.ParserCombinators.Parsec


-- | A register report is a list of postings to an account or set of
-- accounts, with a running total. Postings may be actual postings, or
-- virtual postings aggregated over a reporting interval.
type RegisterReport = [RegisterReportItem] -- line items, one per posting

-- | The data for a single register report line item, representing one posting.
type RegisterReportItem = (Maybe (Day, String) -- transaction date and description if this is the first posting
                          ,Posting             -- the posting
                          ,MixedAmount         -- balance so far
                          )

-- | Print a register report.
register :: [Opt] -> [String] -> Journal -> IO ()
register opts args j = do
  t <- getCurrentLocalTime
  putStr $ registerReportAsText opts $ registerReport opts (optsToFilterSpec opts args t) j

-- | Render a register report as plain text suitable for console output.
registerReportAsText :: [Opt] -> RegisterReport -> String
registerReportAsText opts = unlines . map (registerReportItemAsText opts)

-- | Render one register report line item as plain text. Eg:
-- @
-- date (10)  description (20)     account (22)            amount (11)  balance (12)
-- DDDDDDDDDD dddddddddddddddddddd aaaaaaaaaaaaaaaaaaaaaa  AAAAAAAAAAA AAAAAAAAAAAA
-- ^ displayed for first postings^
--   only, otherwise blank
-- @
registerReportItemAsText :: [Opt] -> RegisterReportItem -> String
registerReportItemAsText _ (dd, p, b) = concatTopPadded [datedesc, pstr, " ", bal]
    where
      datedesc = case dd of Nothing -> replicate datedescwidth ' '
                            Just (da, de) -> printf "%s %s " date desc
                                where
                                  date = showDate da
                                  desc = printf ("%-"++(show descwidth)++"s") $ elideRight descwidth de :: String
          where
            descwidth = datedescwidth - datewidth - 2
            datedescwidth = 32
            datewidth = 10
      pstr = showPostingForRegister p
      bal = padleft 12 (showMixedAmountOrZeroWithoutPrice b)

showPostingWithBalanceForVty showtxninfo p b = registerReportItemAsText [] $ mkitem showtxninfo p b

-- | Get a register report with the specified options for this journal.
registerReport :: [Opt] -> FilterSpec -> Journal -> RegisterReport
registerReport opts fspec j = getitems ps nullposting startbal
    where
      ps | interval == NoInterval = displayableps
         | otherwise              = summarisePostings interval depth empty filterspan displayableps
      (precedingps, displayableps, _) =
          postingsMatchingDisplayExpr (displayExprFromOpts opts) $ journalPostings $ filterJournalPostings fspec j
      startbal = sumPostings precedingps
      (interval, depth, empty) = (intervalFromOpts opts, depthFromOpts opts, Empty `elem` opts)
      filterspan = datespan fspec

-- | Generate register report line items.
getitems :: [Posting] -> Posting -> MixedAmount -> [RegisterReportItem]
getitems [] _ _ = []
getitems (p:ps) pprev b = i:(getitems ps p b')
    where
      i = mkitem isfirst p b'
      isfirst = ptransaction p /= ptransaction pprev
      b' = b + pamount p

-- | Generate one register report line item, from a flag indicating
-- whether to include transaction info, a posting, and the current running
-- balance.
mkitem :: Bool -> Posting -> MixedAmount -> RegisterReportItem
mkitem False p b = (Nothing, p, b)
mkitem True p b = (ds, p, b)
    where ds = case ptransaction p of Just (Transaction{tdate=da,tdescription=de}) -> Just (da,de)
                                      Nothing -> Just (nulldate,"")

-- | Convert a list of postings into summary postings, one per interval.
summarisePostings :: Interval -> Maybe Int -> Bool -> DateSpan -> [Posting] -> [Posting]
summarisePostings interval depth empty filterspan ps = concatMap summarisespan $ splitSpan interval reportspan
    where
      summarisespan s = summarisePostingsInDateSpan s depth empty (postingsinspan s)
      postingsinspan s = filter (isPostingInDateSpan s) ps
      dataspan = postingsDateSpan ps
      reportspan | empty = filterspan `orDatesFrom` dataspan
                 | otherwise = dataspan

-- | Date-sort and split a list of postings into three spans - postings matched
-- by the given display expression, and the preceding and following postings.
postingsMatchingDisplayExpr :: Maybe String -> [Posting] -> ([Posting],[Posting],[Posting])
postingsMatchingDisplayExpr d ps = (before, matched, after)
    where 
      sorted = sortBy (comparing postingDate) ps
      (before, rest) = break (displayExprMatches d) sorted
      (matched, after) = span (displayExprMatches d) rest

-- | Does this display expression allow this posting to be displayed ?
-- Raises an error if the display expression can't be parsed.
displayExprMatches :: Maybe String -> Posting -> Bool
displayExprMatches Nothing  _ = True
displayExprMatches (Just d) p = (fromparse $ parsewith datedisplayexpr d) p
                        
-- | Parse a hledger display expression, which is a simple date test like
-- "d>[DATE]" or "d<=[DATE]", and return a "Posting"-matching predicate.
datedisplayexpr :: GenParser Char st (Posting -> Bool)
datedisplayexpr = do
  char 'd'
  op <- compareop
  char '['
  (y,m,d) <- smartdate
  char ']'
  let date    = parsedate $ printf "%04s/%02s/%02s" y m d
      test op = return $ (`op` date) . postingDate
  case op of
    "<"  -> test (<)
    "<=" -> test (<=)
    "="  -> test (==)
    "==" -> test (==)
    ">=" -> test (>=)
    ">"  -> test (>)
    _    -> mzero
 where
  compareop = choice $ map (try . string) ["<=",">=","==","<","=",">"]

-- XXX confusing, refactor
-- | Given a date span (representing a reporting interval) and a list of
-- postings within it: aggregate the postings so there is only one per
-- account, and adjust their date/description so that they will render
-- as a summary for this interval.
-- 
-- As usual with date spans the end date is exclusive, but for display
-- purposes we show the previous day as end date, like ledger.
-- 
-- When a depth argument is present, postings to accounts of greater
-- depth are aggregated where possible.
-- 
-- The showempty flag forces the display of a zero-posting span
-- and also zero-posting accounts within the span.
summarisePostingsInDateSpan :: DateSpan -> Maybe Int -> Bool -> [Posting] -> [Posting]
summarisePostingsInDateSpan (DateSpan b e) depth showempty ps
    | null ps && (isNothing b || isNothing e) = []
    | null ps && showempty = [summaryp]
    | otherwise = summaryps'
    where
      summaryp = summaryPosting b' ("- "++ showDate (addDays (-1) e'))
      b' = fromMaybe (maybe nulldate postingDate $ headMay ps) b
      e' = fromMaybe (maybe (addDays 1 nulldate) postingDate $ lastMay ps) e
      summaryPosting date desc = nullposting{ptransaction=Just nulltransaction{tdate=date,tdescription=desc}}

      summaryps' = (if showempty then id else filter (not . isZeroMixedAmount . pamount)) summaryps
      summaryps = [summaryp{paccount=a,pamount=balancetoshowfor a} | a <- clippedanames]
      anames = sort $ nub $ map paccount ps
      -- aggregate balances by account, like journalToLedger, then do depth-clipping
      (_,_,exclbalof,inclbalof) = groupPostings ps
      clippedanames = nub $ map (clipAccountName d) anames
      isclipped a = accountNameLevel a >= d
      d = fromMaybe 99999 $ depth
      balancetoshowfor a =
          (if isclipped a then inclbalof else exclbalof) (if null a then "top" else a)

tests_Register :: Test
tests_Register = TestList [

         "summarisePostings" ~: do
           summarisePostings Quarterly Nothing False (DateSpan Nothing Nothing) [] ~?= []

        ]
