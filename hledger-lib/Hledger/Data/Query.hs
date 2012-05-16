{-|

More generic matching, done in one step, unlike FilterSpec and filterJournal*. 
Currently used only by hledger-web.

-}

module Hledger.Data.Query (
  Matcher(..),
  queryIsNull,
  queryIsStartDateOnly,
  queryStartDate,
  matchesTransaction,
  matchesPosting,
  inAccount,
  inAccountQuery,
  tests_Hledger_Data_Query
)
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

-- | A query is a composition of search criteria, which can be used to
-- match postings, transactions, accounts and more.
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

-- | A query for the account(s) we are currently focussed on, if any.
-- Just looks at the first query option.
inAccountQuery :: [QueryOpt] -> Maybe Matcher
inAccountQuery [] = Nothing
inAccountQuery (QueryOptInAcctOnly a:_) = Just $ MatchAcct $ accountNameToAccountOnlyRegex a
inAccountQuery (QueryOptInAcct a:_) = Just $ MatchAcct $ accountNameToAccountRegex a

-- -- | A query restricting the account(s) to be shown in the sidebar, if any.
-- -- Just looks at the first query option.
-- showAccountMatcher :: [QueryOpt] -> Maybe Matcher
-- showAccountMatcher (QueryOptInAcctSubsOnly a:_) = Just $ MatchAcct True $ accountNameToAccountRegex a
-- showAccountMatcher _ = Nothing

-- | Convert a query expression containing zero or more space-separated
-- terms to a query and zero or more query options. A query term is either:
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
    (queries, qopts) = partitionEithers $ map (parseQueryTerm d) terms
    m = case queries of []      -> MatchAny
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
        not' <- fromMaybe "" `fmap` (optionMaybe $ string "not:")
        let allowednexts | null not' = prefixes
                         | otherwise = prefixes ++ [""]
        next <- choice' $ map string allowednexts
        let prefix = not' ++ next
        p <- quotedPattern
        return $ prefix ++ stripquotes p
      quotedPattern = do
        p <- between (oneOf "'\"") (oneOf "'\"") $ many $ noneOf "'\""
        return $ stripquotes p
      pattern = many (noneOf " \n\r\"")

-- -- | Parse the query string as a boolean tree of match patterns.
-- parseQueryTerm :: String -> Matcher
-- parseQueryTerm s = either (const (MatchAny)) id $ runParser query () "" $ lexmatcher s

-- lexmatcher :: String -> [String]
-- lexmatcher s = words' s

-- query :: GenParser String () Matcher
-- query = undefined

-- keep synced with patterns below, excluding "not"
prefixes = map (++":") [
            "inacct","inacctonly",
            "desc","acct","date","edate","status","real","empty","depth"
           ]
defaultprefix = "acct"

-- | Parse a single query term as either a query or a query option.
parseQueryTerm :: Day -> String -> Either Matcher QueryOpt
parseQueryTerm _ ('i':'n':'a':'c':'c':'t':'o':'n':'l':'y':':':s) = Right $ QueryOptInAcctOnly s
parseQueryTerm _ ('i':'n':'a':'c':'c':'t':':':s) = Right $ QueryOptInAcct s
parseQueryTerm d ('n':'o':'t':':':s) = case parseQueryTerm d s of
                                       Left m  -> Left $ MatchNot m
                                       Right _ -> Left MatchAny -- not:somequeryoption will be ignored
parseQueryTerm _ ('d':'e':'s':'c':':':s) = Left $ MatchDesc s
parseQueryTerm _ ('a':'c':'c':'t':':':s) = Left $ MatchAcct s
parseQueryTerm d ('d':'a':'t':'e':':':s) =
        case parsePeriodExpr d s of Left _ -> Left MatchNone -- XXX should warn
                                    Right (_,span) -> Left $ MatchDate span
parseQueryTerm d ('e':'d':'a':'t':'e':':':s) =
        case parsePeriodExpr d s of Left _ -> Left MatchNone -- XXX should warn
                                    Right (_,span) -> Left $ MatchEDate span
parseQueryTerm _ ('s':'t':'a':'t':'u':'s':':':s) = Left $ MatchStatus $ parseStatus s
parseQueryTerm _ ('r':'e':'a':'l':':':s) = Left $ MatchReal $ parseBool s
parseQueryTerm _ ('e':'m':'p':'t':'y':':':s) = Left $ MatchEmpty $ parseBool s
parseQueryTerm _ ('d':'e':'p':'t':'h':':':s) = Left $ MatchDepth $ readDef 0 s
parseQueryTerm _ "" = Left $ MatchAny
parseQueryTerm d s = parseQueryTerm d $ defaultprefix++":"++s

-- | Parse the boolean value part of a "status:" query, allowing "*" as
-- another way to spell True, similar to the journal file format.
parseStatus :: String -> Bool
parseStatus s = s `elem` (truestrings ++ ["*"])

-- | Parse the boolean value part of a "status:" query. A true value can
-- be spelled as "1", "t" or "true".
parseBool :: String -> Bool
parseBool s = s `elem` truestrings

truestrings :: [String]
truestrings = ["1","t","true"]

-- -- | Convert a query to its inverse.
-- negateQuery :: Matcher -> Matcher
-- negateQuery =  MatchNot

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

-- | What start date does this query specify, if any ?
-- If the query is an OR expression, returns the earliest of the alternatives.
-- When the flag is true, look for a starting effective date instead.
queryStartDate :: Bool -> Matcher -> Maybe Day
queryStartDate effective (MatchOr ms) = earliestMaybeDate $ map (queryStartDate effective) ms
queryStartDate effective (MatchAnd ms) = latestMaybeDate $ map (queryStartDate effective) ms
queryStartDate False (MatchDate (DateSpan (Just d) _)) = Just d
queryStartDate True (MatchEDate (DateSpan (Just d) _)) = Just d
queryStartDate _ _ = Nothing

-- | Does this query specify a start date and nothing else (that would
-- filter postings prior to the date) ?
-- When the flag is true, look for a starting effective date instead.
queryIsStartDateOnly :: Bool -> Matcher -> Bool
queryIsStartDateOnly _ MatchAny = False
queryIsStartDateOnly _ MatchNone = False
queryIsStartDateOnly effective (MatchOr ms) = and $ map (queryIsStartDateOnly effective) ms
queryIsStartDateOnly effective (MatchAnd ms) = and $ map (queryIsStartDateOnly effective) ms
queryIsStartDateOnly False (MatchDate (DateSpan (Just _) _)) = True
queryIsStartDateOnly True (MatchEDate (DateSpan (Just _) _)) = True
queryIsStartDateOnly _ _ = False

-- | Does this query match everything ?
queryIsNull MatchAny = True
queryIsNull (MatchAnd []) = True
queryIsNull (MatchNot (MatchOr [])) = True
queryIsNull _ = False

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

tests_Hledger_Data_Query :: Test
tests_Hledger_Data_Query = TestList
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

  ,"words''" ~: do
    assertEqual "1" ["a","b"]        (words'' [] "a b")
    assertEqual "2" ["a b"]          (words'' [] "'a b'")
    assertEqual "3" ["not:a","b"]    (words'' [] "not:a b")
    assertEqual "4" ["not:a b"]    (words'' [] "not:'a b'")
    assertEqual "5" ["not:a b"]    (words'' [] "'not:a b'")
    assertEqual "6" ["not:desc:a b"]    (words'' ["desc:"] "not:desc:'a b'")

 ]
