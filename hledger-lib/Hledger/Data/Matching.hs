{-|

More generic matching, done in one step, unlike FilterSpec and filterJournal*. 
Currently used only by hledger-web.

-}

module Hledger.Data.Matching
where
-- import Data.List
-- import Data.Map (findWithDefault, (!))
-- import Data.Maybe
-- import Data.Ord
import Data.Time.Calendar
-- import Data.Time.LocalTime
-- import Data.Tree
import Safe (readDef)
-- import System.Time (ClockTime(TOD))
-- import Test.HUnit
import Text.ParserCombinators.Parsec
-- import Text.Printf
-- import qualified Data.Map as Map

import Hledger.Utils
import Hledger.Data.Types
-- import Hledger.Data.AccountName
-- import Hledger.Data.Amount
-- import Hledger.Data.Commodity (canonicaliseCommodities)
import Hledger.Data.Dates
-- import Hledger.Data.Transaction (journalTransactionWithDate,balanceTransaction)
-- import Hledger.Data.Posting
-- import Hledger.Data.TimeLog

-- | A more general way to match transactions and postings, successor to FilterSpec. (?)
-- If the first boolean is False, it's a negative match.
data Matcher = MatchOr [Matcher]          -- ^ match if any match
             | MatchAnd [Matcher]         -- ^ match if all match
             | MatchDesc Bool String      -- ^ match if description matches this regexp
             | MatchAcct Bool String      -- ^ match postings whose account matches this regexp
             | MatchOtherAcct Bool String -- ^ match postings whose transaction contains a posting to an account matching this regexp
             | MatchDate Bool DateSpan    -- ^ match if actual date in this date span
             | MatchEDate Bool DateSpan   -- ^ match if effective date in this date span
             | MatchStatus Bool Bool      -- ^ match if cleared status has this value
             | MatchReal Bool Bool        -- ^ match if "realness" (involves a real non-virtual account ?) has this value
             | MatchEmpty Bool Bool       -- ^ match if "emptiness" (amount is zero ?) has this value
             | MatchDepth Bool Int        -- ^ match if account depth is less than or equal to this value
    deriving (Show)

-- | Parse a query expression as a list of match patterns OR'd together.
parseMatcher :: Day -> String -> Matcher
parseMatcher refdate s = MatchOr $ map parseword $ words'' ["otheracct:"] s
  where
    parseword :: String -> Matcher
    parseword ('n':'o':'t':':':s) = negateMatch $ parseMatcher refdate s
    parseword ('d':'e':'s':'c':':':s) = MatchDesc True s
    parseword ('a':'c':'c':'t':':':s) = MatchAcct True s
    parseword ('o':'t':'h':'e':'r':'a':'c':'c':'t':':':s) = MatchOtherAcct True s
    parseword ('d':'a':'t':'e':':':s) = MatchDate True $ spanFromSmartDateString refdate s
    parseword ('e':'d':'a':'t':'e':':':s) = MatchEDate True $ spanFromSmartDateString refdate s
    parseword ('s':'t':'a':'t':'u':'s':':':s) = MatchStatus True $ parseStatus s
    parseword ('r':'e':'a':'l':':':s) = MatchReal True $ parseBool s
    parseword ('e':'m':'p':'t':'y':':':s) = MatchEmpty True $ parseBool s
    parseword ('d':'e':'p':'t':'h':':':s) = MatchDepth True $ readDef 0 s
    parseword s = parseword $ "acct:"++s

    parseStatus "*" = True
    parseStatus _ = False

    parseBool s = s `elem` ["t","true","1","on"]

-- | Quote-and-prefix-aware version of words - don't split on spaces which
-- are inside quotes, including quotes which may have one of the specified
-- prefixes in front.
words'' :: [String] -> String -> [String]
words'' prefixes = fromparse . parsewith maybeprefixedquotedphrases
    where
      maybeprefixedquotedphrases = choice' [prefixedQuotedPattern, quotedPattern, pattern] `sepBy` many1 spacenonewline
      prefixedQuotedPattern = do
        prefix <- choice' $ map string prefixes
        p <- quotedPattern
        return $ prefix ++ stripquotes p
      quotedPattern = do
        p <- between (oneOf "'\"") (oneOf "'\"") $ many $ noneOf "'\""
        return $ stripquotes p
      pattern = many (noneOf " \n\r\"")

-- -- | Parse the query string as a boolean tree of match patterns.
-- parseMatcher :: String -> Matcher
-- parseMatcher s = either (const (MatchOr [])) id $ runParser matcher () "" $ lexmatcher s

-- lexmatcher :: String -> [String]
-- lexmatcher s = words' s

-- matcher :: GenParser String () Matcher
-- matcher = undefined

matchesPosting :: Matcher -> Posting -> Bool
matchesPosting (MatchOr ms) p = any (`matchesPosting` p) ms
matchesPosting (MatchAnd ms) p = all (`matchesPosting` p) ms
matchesPosting (MatchDesc True r) p = regexMatches r $ maybe "" tdescription $ ptransaction p
matchesPosting (MatchDesc False r) p = not $ (MatchDesc True r) `matchesPosting` p
matchesPosting (MatchAcct True r) p = regexMatches r $ paccount p
matchesPosting (MatchAcct False r) p = not $ (MatchAcct True r) `matchesPosting` p
matchesPosting (MatchOtherAcct True r) p =
    case ptransaction p of
        Just t -> (MatchAcct True r) `matchesTransaction` t && (MatchAcct False r) `matchesPosting` p
        Nothing -> False
matchesPosting (MatchOtherAcct False r) p = not $ (MatchOtherAcct True r) `matchesPosting` p
matchesPosting _ _ = False

matchesTransaction :: Matcher -> Transaction -> Bool
matchesTransaction (MatchOr ms) t = any (`matchesTransaction` t) ms
matchesTransaction (MatchAnd ms) t = all (`matchesTransaction` t) ms
matchesTransaction (MatchDesc True r) t = regexMatches r $ tdescription t
matchesTransaction (MatchDesc False r) t = not $ (MatchDesc True r) `matchesTransaction` t
matchesTransaction m@(MatchAcct True _) t = any (m `matchesPosting`) $ tpostings t
matchesTransaction (MatchAcct False r) t = not $ (MatchAcct True r) `matchesTransaction` t
matchesTransaction m@(MatchOtherAcct sense r) t = (MatchAcct sense r) `matchesTransaction` t
matchesTransaction _ _ = False

negateMatch :: Matcher -> Matcher
negateMatch (MatchOr ms)               = MatchAnd $ map negateMatch ms
negateMatch (MatchAnd ms)              = MatchOr $ map negateMatch ms
negateMatch (MatchAcct sense arg)      = MatchAcct (not sense) arg
negateMatch (MatchDesc sense arg)      = MatchDesc (not sense) arg
negateMatch (MatchOtherAcct sense arg) = MatchOtherAcct (not sense) arg
negateMatch (MatchDate sense arg)      = MatchDate (not sense) arg
negateMatch (MatchEDate sense arg)     = MatchEDate (not sense) arg
negateMatch (MatchStatus sense arg)    = MatchStatus (not sense) arg
negateMatch (MatchReal sense arg)      = MatchReal (not sense) arg
negateMatch (MatchEmpty sense arg)     = MatchEmpty (not sense) arg
negateMatch (MatchDepth sense arg)     = MatchDepth (not sense) arg

