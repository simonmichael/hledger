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
data Matcher = MatchAny                   -- ^ always match
             | MatchOr [Matcher]          -- ^ match if any of these match
             | MatchAnd [Matcher]         -- ^ match if all of these match
             | MatchDesc Bool String      -- ^ match if description matches this regexp
             | MatchAcct Bool String      -- ^ match postings whose account matches this regexp
             | MatchInAcct Bool String    -- ^ XXX match postings whose transaction contains a posting to an account matching this regexp
             | MatchDate Bool DateSpan    -- ^ match if actual date in this date span
             | MatchEDate Bool DateSpan   -- ^ match if effective date in this date span
             | MatchStatus Bool Bool      -- ^ match if cleared status has this value
             | MatchReal Bool Bool        -- ^ match if "realness" (involves a real non-virtual account ?) has this value
             | MatchEmpty Bool Bool       -- ^ match if "emptiness" (amount is zero ?) has this value
             | MatchDepth Bool Int        -- ^ match if account depth is less than or equal to this value
    deriving (Show)

-- | Parse a query expression as a list of match patterns OR'd together.
parseMatcher :: Day -> String -> Matcher
parseMatcher refdate s = MatchAnd $ map parseword $ words'' ["not:","acct:","desc:"] s
  where
    parseword :: String -> Matcher
    parseword ('n':'o':'t':':':s) = negateMatch $ parseMatcher refdate s
    parseword ('d':'e':'s':'c':':':s) = MatchDesc True s
    parseword ('a':'c':'c':'t':':':s) = MatchAcct True s
    parseword ('i':'n':'a':'c':'c':'t':':':s) = MatchInAcct True s
    parseword ('i':'n':':':s) = MatchInAcct True s
    parseword ('d':'a':'t':'e':':':s) = MatchDate True $ spanFromSmartDateString refdate s
    parseword ('e':'d':'a':'t':'e':':':s) = MatchEDate True $ spanFromSmartDateString refdate s
    parseword ('s':'t':'a':'t':'u':'s':':':s) = MatchStatus True $ parseStatus s
    parseword ('r':'e':'a':'l':':':s) = MatchReal True $ parseBool s
    parseword ('e':'m':'p':'t':'y':':':s) = MatchEmpty True $ parseBool s
    parseword ('d':'e':'p':'t':'h':':':s) = MatchDepth True $ readDef 0 s
    parseword "" = MatchAny
    parseword s = parseword $ "in:"++s

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
-- parseMatcher s = either (const (MatchAny)) id $ runParser matcher () "" $ lexmatcher s

-- lexmatcher :: String -> [String]
-- lexmatcher s = words' s

-- matcher :: GenParser String () Matcher
-- matcher = undefined

matchesPosting :: Matcher -> Posting -> Bool
matchesPosting (MatchAny) p = True
matchesPosting (MatchOr ms) p = any (`matchesPosting` p) ms
matchesPosting (MatchAnd ms) p = all (`matchesPosting` p) ms
matchesPosting (MatchDesc True r) p = regexMatchesCI r $ maybe "" tdescription $ ptransaction p
matchesPosting (MatchDesc False r) p = not $ (MatchDesc True r) `matchesPosting` p
matchesPosting (MatchAcct True r) p = regexMatchesCI r $ paccount p
matchesPosting (MatchAcct False r) p = not $ (MatchAcct True r) `matchesPosting` p
matchesPosting (MatchInAcct True r) p =
    case ptransaction p of
        Just t -> (MatchAcct True r) `matchesTransaction` t && (MatchAcct False r) `matchesPosting` p
        Nothing -> False
matchesPosting (MatchInAcct False r) p = not $ (MatchInAcct True r) `matchesPosting` p
matchesPosting _ _ = False

matchesTransaction :: Matcher -> Transaction -> Bool
matchesTransaction (MatchAny) t = True
matchesTransaction (MatchOr ms) t = any (`matchesTransaction` t) ms
matchesTransaction (MatchAnd ms) t = all (`matchesTransaction` t) ms
matchesTransaction (MatchDesc True r) t = regexMatchesCI r $ tdescription t
matchesTransaction (MatchDesc False r) t = not $ (MatchDesc True r) `matchesTransaction` t
matchesTransaction m@(MatchAcct True _) t = any (m `matchesPosting`) $ tpostings t
matchesTransaction (MatchAcct False r) t = not $ (MatchAcct True r) `matchesTransaction` t
matchesTransaction (MatchInAcct sense r) t = (MatchAcct sense r) `matchesTransaction` t
matchesTransaction _ _ = False

-- | Does this matcher specify this account as the one we are "in" ?
-- For now, does a case-insensitive exact string match on the full account name.
matchesInAccount :: Matcher -> AccountName -> Bool
matchesInAccount (MatchAny) a = True
matchesInAccount (MatchOr ms) a = any (`matchesInAccount` a) ms
matchesInAccount (MatchAnd ms) a = all (`matchesInAccount` a) ms
matchesInAccount (MatchInAcct True r) a = lowercase r == lowercase a -- regexMatchesCI r a
matchesInAccount (MatchInAcct False r) a = not $ (MatchInAcct True r) `matchesInAccount` a
matchesInAccount _ _ = True

negateMatch :: Matcher -> Matcher
negateMatch (MatchOr ms)               = MatchAnd $ map negateMatch ms
negateMatch (MatchAnd ms)              = MatchOr $ map negateMatch ms
negateMatch (MatchAcct sense arg)      = MatchAcct (not sense) arg
negateMatch (MatchDesc sense arg)      = MatchDesc (not sense) arg
negateMatch (MatchInAcct sense arg)    = MatchInAcct (not sense) arg
negateMatch (MatchDate sense arg)      = MatchDate (not sense) arg
negateMatch (MatchEDate sense arg)     = MatchEDate (not sense) arg
negateMatch (MatchStatus sense arg)    = MatchStatus (not sense) arg
negateMatch (MatchReal sense arg)      = MatchReal (not sense) arg
negateMatch (MatchEmpty sense arg)     = MatchEmpty (not sense) arg
negateMatch (MatchDepth sense arg)     = MatchDepth (not sense) arg

