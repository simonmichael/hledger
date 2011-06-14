{-|

More generic matching, done in one step, unlike FilterSpec and filterJournal*. 
Currently used only by hledger-web.

-}

module Hledger.Data.Matching
where
import Data.Either
import Data.List
-- import Data.Map (findWithDefault, (!))
import Data.Maybe
-- import Data.Ord
import Data.Time.Calendar
-- import Data.Time.LocalTime
-- import Data.Tree
import Safe (readDef, headDef)
-- import System.Time (ClockTime(TOD))
import Test.HUnit
import Text.ParserCombinators.Parsec
-- import Text.Printf
-- import qualified Data.Map as Map

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount
-- import Hledger.Data.Commodity (canonicaliseCommodities)
import Hledger.Data.Dates
import Hledger.Data.Posting
import Hledger.Data.Transaction
-- import Hledger.Data.TimeLog

-- | A matcher is a single, or boolean composition of, search criteria,
-- which can be used to match postings, transactions, accounts and more.
-- Currently used by hledger-web, will likely replace FilterSpec at some point.
data Matcher = MatchAny              -- ^ always match
             | MatchNone             -- ^ never match
             | MatchNot Matcher      -- ^ negate this match
             | MatchOr [Matcher]     -- ^ match if any of these match
             | MatchAnd [Matcher]    -- ^ match if all of these match
             | MatchDesc String      -- ^ match if description matches this regexp
             | MatchAcct String      -- ^ match postings whose account matches this regexp
             | MatchDate DateSpan    -- ^ match if actual date in this date span
             | MatchEDate DateSpan   -- ^ match if effective date in this date span
             | MatchStatus Bool      -- ^ match if cleared status has this value
             | MatchReal Bool        -- ^ match if "realness" (involves a real non-virtual account ?) has this value
             | MatchEmpty Bool       -- ^ match if "emptiness" (from the --empty command-line flag) has this value.
                                     --   Currently this means a posting with zero amount.
             | MatchDepth Int        -- ^ match if account depth is less than or equal to this value
    deriving (Show, Eq)

-- | A query option changes a query's/report's behaviour and output in some way.

-- XXX could use regular CliOpts ?
data QueryOpt = QueryOptInAcctOnly AccountName  -- ^ show an account register focussed on this account
              | QueryOptInAcct AccountName      -- ^ as above but include sub-accounts in the account register
           -- | QueryOptCostBasis      -- ^ show amounts converted to cost where possible
           -- | QueryOptEffectiveDate  -- ^ show effective dates instead of actual dates
    deriving (Show, Eq)

-- | The account we are currently focussed on, if any, and whether subaccounts are included.
-- Just looks at the first query option.
inAccount :: [QueryOpt] -> Maybe (AccountName,Bool)
inAccount [] = Nothing
inAccount (QueryOptInAcctOnly a:_) = Just (a,False)
inAccount (QueryOptInAcct a:_) = Just (a,True)

-- | A matcher for the account(s) we are currently focussed on, if any.
-- Just looks at the first query option.
inAccountMatcher :: [QueryOpt] -> Maybe Matcher
inAccountMatcher [] = Nothing
inAccountMatcher (QueryOptInAcctOnly a:_) = Just $ MatchAcct $ accountNameToAccountOnlyRegex a
inAccountMatcher (QueryOptInAcct a:_) = Just $ MatchAcct $ accountNameToAccountRegex a

-- -- | A matcher restricting the account(s) to be shown in the sidebar, if any.
-- -- Just looks at the first query option.
-- showAccountMatcher :: [QueryOpt] -> Maybe Matcher
-- showAccountMatcher (QueryOptInAcctSubsOnly a:_) = Just $ MatchAcct True $ accountNameToAccountRegex a
-- showAccountMatcher _ = Nothing

-- | Convert a query expression containing zero or more space-separated
-- terms to a matcher and zero or more query options. A query term is either:
--
-- 1. a search criteria, used to match transactions. This is usually a prefixed pattern such as:
--    acct:REGEXP
--    date:PERIODEXP
--    not:desc:REGEXP
--
-- 2. a query option, which changes behaviour in some way. There is currently one of these:
--    inacct:FULLACCTNAME - should appear only once
--
-- Multiple search criteria are AND'ed together.
-- When a pattern contains spaces, it or the whole term should be enclosed in single or double quotes.
-- A reference date is required to interpret relative dates in period expressions.
--
parseQuery :: Day -> String -> (Matcher,[QueryOpt])
parseQuery d s = (m,qopts)
  where
    terms = words'' prefixes s
    (matchers, qopts) = partitionEithers $ map (parseMatcher d) terms
    m = case matchers of []      -> MatchAny
                         (m':[]) -> m'
                         ms      -> MatchAnd ms

-- | Quote-and-prefix-aware version of words - don't split on spaces which
-- are inside quotes, including quotes which may have one of the specified
-- prefixes in front, and maybe an additional not: prefix in front of that.
words'' :: [String] -> String -> [String]
words'' prefixes = fromparse . parsewith maybeprefixedquotedphrases -- XXX
    where
      maybeprefixedquotedphrases = choice' [prefixedQuotedPattern, quotedPattern, pattern] `sepBy` many1 spacenonewline
      prefixedQuotedPattern = do
        not' <- optionMaybe $ string "not:"
        prefix <- choice' $ map string prefixes
        p <- quotedPattern
        return $ fromMaybe "" not' ++ prefix ++ stripquotes p
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

-- keep synced with patterns below, excluding "not"
prefixes = map (++":") [
            "inacct","inacctonly",
            "desc","acct","date","edate","status","real","empty","depth"
           ]
defaultprefix = "acct"

-- | Parse a single query term as either a matcher or a query option.
parseMatcher :: Day -> String -> Either Matcher QueryOpt
parseMatcher _ ('i':'n':'a':'c':'c':'t':'o':'n':'l':'y':':':s) = Right $ QueryOptInAcctOnly s
parseMatcher _ ('i':'n':'a':'c':'c':'t':':':s) = Right $ QueryOptInAcct s
parseMatcher d ('n':'o':'t':':':s) = case parseMatcher d s of
                                       Left m  -> Left $ MatchNot m
                                       Right _ -> Left MatchAny -- not:somequeryoption will be ignored
parseMatcher _ ('d':'e':'s':'c':':':s) = Left $ MatchDesc s
parseMatcher _ ('a':'c':'c':'t':':':s) = Left $ MatchAcct s
parseMatcher d ('d':'a':'t':'e':':':s) =
        case parsePeriodExpr d s of Left _ -> Left MatchNone -- XXX should warn
                                    Right (_,span) -> Left $ MatchDate span
parseMatcher d ('e':'d':'a':'t':'e':':':s) =
        case parsePeriodExpr d s of Left _ -> Left MatchNone -- XXX should warn
                                    Right (_,span) -> Left $ MatchEDate span
parseMatcher _ ('s':'t':'a':'t':'u':'s':':':s) = Left $ MatchStatus $ parseStatus s
parseMatcher _ ('r':'e':'a':'l':':':s) = Left $ MatchReal $ parseBool s
parseMatcher _ ('e':'m':'p':'t':'y':':':s) = Left $ MatchEmpty $ parseBool s
parseMatcher _ ('d':'e':'p':'t':'h':':':s) = Left $ MatchDepth $ readDef 0 s
parseMatcher _ "" = Left $ MatchAny
parseMatcher d s = parseMatcher d $ defaultprefix++":"++s

-- | Parse the boolean value part of a "status:" matcher, allowing "*" as
-- another way to spell True, similar to the journal file format.
parseStatus :: String -> Bool
parseStatus s = s `elem` (truestrings ++ ["*"])

-- | Parse the boolean value part of a "status:" matcher. A true value can
-- be spelled as "1", "t" or "true".
parseBool :: String -> Bool
parseBool s = s `elem` truestrings

truestrings :: [String]
truestrings = ["1","t","true"]

-- | Convert a match expression to its inverse.
negateMatcher :: Matcher -> Matcher
negateMatcher =  MatchNot

-- | Does the match expression match this posting ?
matchesPosting :: Matcher -> Posting -> Bool
matchesPosting (MatchNot m) p = not $ matchesPosting m p
matchesPosting (MatchAny) _ = True
matchesPosting (MatchNone) _ = False
matchesPosting (MatchOr ms) p = any (`matchesPosting` p) ms
matchesPosting (MatchAnd ms) p = all (`matchesPosting` p) ms
matchesPosting (MatchDesc r) p = regexMatchesCI r $ maybe "" tdescription $ ptransaction p
matchesPosting (MatchAcct r) p = regexMatchesCI r $ paccount p
matchesPosting (MatchDate span) p =
    case d of Just d'  -> spanContainsDate span d'
              Nothing -> False
    where d = maybe Nothing (Just . tdate) $ ptransaction p
matchesPosting (MatchEDate span) p =
    case postingEffectiveDate p of Just d  -> spanContainsDate span d
                                   Nothing -> False
matchesPosting (MatchStatus v) p = v == postingCleared p
matchesPosting (MatchReal v) p = v == isReal p
matchesPosting (MatchEmpty v) Posting{pamount=a} = v == isZeroMixedAmount a
matchesPosting _ _ = False

-- | Does the match expression match this transaction ?
matchesTransaction :: Matcher -> Transaction -> Bool
matchesTransaction (MatchNot m) t = not $ matchesTransaction m t
matchesTransaction (MatchAny) _ = True
matchesTransaction (MatchNone) _ = False
matchesTransaction (MatchOr ms) t = any (`matchesTransaction` t) ms
matchesTransaction (MatchAnd ms) t = all (`matchesTransaction` t) ms
matchesTransaction (MatchDesc r) t = regexMatchesCI r $ tdescription t
matchesTransaction m@(MatchAcct _) t = any (m `matchesPosting`) $ tpostings t
matchesTransaction (MatchDate span) t = spanContainsDate span $ tdate t
matchesTransaction (MatchEDate span) t = spanContainsDate span $ transactionEffectiveDate t
matchesTransaction (MatchStatus v) t = v == tstatus t
matchesTransaction (MatchReal v) t = v == hasRealPostings t
matchesTransaction _ _ = False

postingEffectiveDate :: Posting -> Maybe Day
postingEffectiveDate p = maybe Nothing (Just . transactionEffectiveDate) $ ptransaction p

transactionEffectiveDate :: Transaction -> Day
transactionEffectiveDate t = case teffectivedate t of Just d  -> d
                                                      Nothing -> tdate t

-- | Does the match expression match this account ?
-- A matching in: clause is also considered a match.
matchesAccount :: Matcher -> AccountName -> Bool
matchesAccount (MatchNot m) a = not $ matchesAccount m a
matchesAccount (MatchAny) _ = True
matchesAccount (MatchNone) _ = False
matchesAccount (MatchOr ms) a = any (`matchesAccount` a) ms
matchesAccount (MatchAnd ms) a = all (`matchesAccount` a) ms
matchesAccount (MatchAcct r) a = regexMatchesCI r a
matchesAccount _ _ = False

-- | What start date does this matcher specify, if any ?
-- If the matcher is an OR expression, returns the earliest of the alternatives.
-- When the flag is true, look for a starting effective date instead.
matcherStartDate :: Bool -> Matcher -> Maybe Day
matcherStartDate effective (MatchOr ms) = earliestMaybeDate $ map (matcherStartDate effective) ms
matcherStartDate effective (MatchAnd ms) = latestMaybeDate $ map (matcherStartDate effective) ms
matcherStartDate False (MatchDate (DateSpan (Just d) _)) = Just d
matcherStartDate True (MatchEDate (DateSpan (Just d) _)) = Just d
matcherStartDate _ _ = Nothing

-- | Does this matcher specify a start date and nothing else (that would
-- filter postings prior to the date) ?
-- When the flag is true, look for a starting effective date instead.
matcherIsStartDateOnly :: Bool -> Matcher -> Bool
matcherIsStartDateOnly _ MatchAny = False
matcherIsStartDateOnly _ MatchNone = False
matcherIsStartDateOnly effective (MatchOr ms) = and $ map (matcherIsStartDateOnly effective) ms
matcherIsStartDateOnly effective (MatchAnd ms) = and $ map (matcherIsStartDateOnly effective) ms
matcherIsStartDateOnly False (MatchDate (DateSpan (Just _) _)) = True
matcherIsStartDateOnly True (MatchEDate (DateSpan (Just _) _)) = True
matcherIsStartDateOnly _ _ = False

-- | Does this matcher match everything ?
matcherIsNull MatchAny = True
matcherIsNull (MatchAnd []) = True
matcherIsNull (MatchNot (MatchOr [])) = True
matcherIsNull _ = False

-- | What is the earliest of these dates, where Nothing is earliest ?
earliestMaybeDate :: [Maybe Day] -> Maybe Day
earliestMaybeDate = headDef Nothing . sortBy compareMaybeDates

-- | What is the latest of these dates, where Nothing is earliest ?
latestMaybeDate :: [Maybe Day] -> Maybe Day
latestMaybeDate = headDef Nothing . sortBy (flip compareMaybeDates)

-- | Compare two maybe dates, Nothing is earliest.
compareMaybeDates :: Maybe Day -> Maybe Day -> Ordering
compareMaybeDates Nothing Nothing = EQ
compareMaybeDates Nothing (Just _) = LT
compareMaybeDates (Just _) Nothing = GT
compareMaybeDates (Just a) (Just b) = compare a b

tests_Hledger_Data_Matching :: Test
tests_Hledger_Data_Matching = TestList
 [

  "parseQuery" ~: do
    let d = parsedate "2011/1/1"
    parseQuery d "a" `is` (MatchAcct "a", [])
    parseQuery d "acct:a" `is` (MatchAcct "a", [])
    parseQuery d "acct:a desc:b" `is` (MatchAnd [MatchAcct "a", MatchDesc "b"], [])
    parseQuery d "\"acct:expenses:autres d\233penses\"" `is` (MatchAcct "expenses:autres d\233penses", [])
    parseQuery d "not:desc:'a b'" `is` (MatchNot $ MatchDesc "a b", [])

    parseQuery d "inacct:a desc:b" `is` (MatchDesc "b", [QueryOptInAcct "a"])
    parseQuery d "inacct:a inacct:b" `is` (MatchAny, [QueryOptInAcct "a", QueryOptInAcct "b"])

    parseQuery d "status:1" `is` (MatchStatus True, [])
    parseQuery d "status:0" `is` (MatchStatus False, [])
    parseQuery d "status:" `is` (MatchStatus False, [])
    parseQuery d "real:1" `is` (MatchReal True, [])

  ,"matchesAccount" ~: do
    assertBool "positive acct match" $ matchesAccount (MatchAcct "b:c") "a:bb:c:d"
    -- assertBool "acct should match at beginning" $ not $ matchesAccount (MatchAcct True "a:b") "c:a:b"

  ,"matchesPosting" ~: do
    -- matching posting status..
    assertBool "positive match on true posting status"  $
                   (MatchStatus True)  `matchesPosting` nullposting{pstatus=True}
    assertBool "negative match on true posting status"  $
               not $ (MatchNot $ MatchStatus True)  `matchesPosting` nullposting{pstatus=True}
    assertBool "positive match on false posting status" $
                   (MatchStatus False) `matchesPosting` nullposting{pstatus=False}
    assertBool "negative match on false posting status" $
               not $ (MatchNot $ MatchStatus False) `matchesPosting` nullposting{pstatus=False}
    assertBool "positive match on true posting status acquired from transaction" $
                   (MatchStatus True) `matchesPosting` nullposting{pstatus=False,ptransaction=Just nulltransaction{tstatus=True}}
    assertBool "real:1 on real posting" $ (MatchReal True) `matchesPosting` nullposting{ptype=RegularPosting}
    assertBool "real:1 on virtual posting fails" $ not $ (MatchReal True) `matchesPosting` nullposting{ptype=VirtualPosting}
    assertBool "real:1 on balanced virtual posting fails" $ not $ (MatchReal True) `matchesPosting` nullposting{ptype=BalancedVirtualPosting}

 ]
