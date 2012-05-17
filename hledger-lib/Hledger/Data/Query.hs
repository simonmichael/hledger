{-|

A general query system for matching items by standard criteria, in one
step unlike FilterSpec and filterJournal*.  Currently used by hledger-web.

-}

module Hledger.Data.Query (
  -- * Query and QueryOpt
  Query(..),
  QueryOpt(..),
  -- * parsing
  parseQuery,
  simplifyQuery,
  -- * accessors
  queryIsNull,
  queryStartDate,
  queryIsStartDateOnly,
  inAccount,
  inAccountQuery,
  -- * matching
  matchesTransaction,
  matchesPosting,
  -- * tests
  tests_Hledger_Data_Query
)
where
import Data.Either
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Safe (readDef, headDef)
import Test.HUnit
import Text.ParserCombinators.Parsec

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount
import Hledger.Data.Dates
import Hledger.Data.Posting
import Hledger.Data.Transaction


-- | A query is a composition of search criteria, which can be used to
-- match postings, transactions, accounts and more.
data Query = Any              -- ^ always match
           | None             -- ^ never match
           | Not Query        -- ^ negate this match
           | Or [Query]       -- ^ match if any of these match
           | And [Query]      -- ^ match if all of these match
           | Desc String      -- ^ match if description matches this regexp
           | Acct String      -- ^ match postings whose account matches this regexp
           | Date DateSpan    -- ^ match if actual date in this date span
           | EDate DateSpan   -- ^ match if effective date in this date span
           | Status Bool      -- ^ match if cleared status has this value
           | Real Bool        -- ^ match if "realness" (involves a real non-virtual account ?) has this value
           | Empty Bool       -- ^ match if "emptiness" (from the --empty command-line flag) has this value.
                              --   Currently this means a posting with zero amount.
           | Depth Int        -- ^ match if account depth is less than or equal to this value
    deriving (Show, Eq)

-- | A query option changes a query's/report's behaviour and output in some way.

-- XXX could use regular CliOpts ?
data QueryOpt = QueryOptInAcctOnly AccountName  -- ^ show an account register focussed on this account
              | QueryOptInAcct AccountName      -- ^ as above but include sub-accounts in the account register
           -- | QueryOptCostBasis      -- ^ show amounts converted to cost where possible
           -- | QueryOptEffectiveDate  -- ^ show effective dates instead of actual dates
    deriving (Show, Eq)

-- parsing

-- -- | A query restricting the account(s) to be shown in the sidebar, if any.
-- -- Just looks at the first query option.
-- showAccountMatcher :: [QueryOpt] -> Maybe Query
-- showAccountMatcher (QueryOptInAcctSubsOnly a:_) = Just $ Acct True $ accountNameToAccountRegex a
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
parseQuery :: Day -> String -> (Query,[QueryOpt])
parseQuery d s = (m,qopts)
  where
    terms = words'' prefixes s
    (queries, qopts) = partitionEithers $ map (parseQueryTerm d) terms
    m = case queries of []      -> Any
                        (m':[]) -> m'
                        ms      -> And ms

tests_parseQuery = [
  "parseQuery" ~: do
    let d = parsedate "2011/1/1"
    parseQuery d "acct:'expenses:autres d\233penses' desc:b" `is` (And [Acct "expenses:autres d\233penses", Desc "b"], [])
    parseQuery d "inacct:a desc:\"b b\"" `is` (Desc "b b", [QueryOptInAcct "a"])
    parseQuery d "inacct:a inacct:b" `is` (Any, [QueryOptInAcct "a", QueryOptInAcct "b"])
 ]

-- keep synced with patterns below, excluding "not"
prefixes = map (++":") [
            "inacct","inacctonly",
            "desc","acct","date","edate","status","real","empty","depth"
           ]
defaultprefix = "acct"

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

tests_words'' = [
   "words''" ~: do
    assertEqual "1" ["a","b"]        (words'' [] "a b")
    assertEqual "2" ["a b"]          (words'' [] "'a b'")
    assertEqual "3" ["not:a","b"]    (words'' [] "not:a b")
    assertEqual "4" ["not:a b"]    (words'' [] "not:'a b'")
    assertEqual "5" ["not:a b"]    (words'' [] "'not:a b'")
    assertEqual "6" ["not:desc:a b"]    (words'' ["desc:"] "not:desc:'a b'")
    let s `gives` r = assertEqual "" r (words'' prefixes s)
    "\"acct:expenses:autres d\233penses\"" `gives` ["acct:expenses:autres d\233penses"]
 ]

-- -- | Parse the query string as a boolean tree of match patterns.
-- parseQueryTerm :: String -> Query
-- parseQueryTerm s = either (const (Any)) id $ runParser query () "" $ lexmatcher s

-- lexmatcher :: String -> [String]
-- lexmatcher s = words' s

-- query :: GenParser String () Query
-- query = undefined

-- | Parse a single query term as either a query or a query option.
parseQueryTerm :: Day -> String -> Either Query QueryOpt
parseQueryTerm _ ('i':'n':'a':'c':'c':'t':'o':'n':'l':'y':':':s) = Right $ QueryOptInAcctOnly s
parseQueryTerm _ ('i':'n':'a':'c':'c':'t':':':s) = Right $ QueryOptInAcct s
parseQueryTerm d ('n':'o':'t':':':s) = case parseQueryTerm d s of
                                       Left m  -> Left $ Not m
                                       Right _ -> Left Any -- not:somequeryoption will be ignored
parseQueryTerm _ ('d':'e':'s':'c':':':s) = Left $ Desc s
parseQueryTerm _ ('a':'c':'c':'t':':':s) = Left $ Acct s
parseQueryTerm d ('d':'a':'t':'e':':':s) =
        case parsePeriodExpr d s of Left _ -> Left None -- XXX should warn
                                    Right (_,span) -> Left $ Date span
parseQueryTerm d ('e':'d':'a':'t':'e':':':s) =
        case parsePeriodExpr d s of Left _ -> Left None -- XXX should warn
                                    Right (_,span) -> Left $ EDate span
parseQueryTerm _ ('s':'t':'a':'t':'u':'s':':':s) = Left $ Status $ parseStatus s
parseQueryTerm _ ('r':'e':'a':'l':':':s) = Left $ Real $ parseBool s
parseQueryTerm _ ('e':'m':'p':'t':'y':':':s) = Left $ Empty $ parseBool s
parseQueryTerm _ ('d':'e':'p':'t':'h':':':s) = Left $ Depth $ readDef 0 s
parseQueryTerm _ "" = Left $ Any
parseQueryTerm d s = parseQueryTerm d $ defaultprefix++":"++s

tests_parseQueryTerm = [
  "parseQueryTerm" ~: do
    let s `gives` r = parseQueryTerm nulldate s `is` r
    "a" `gives` (Left $ Acct "a")
    "acct:expenses:autres d\233penses" `gives` (Left $ Acct "expenses:autres d\233penses")
    "not:desc:a b" `gives` (Left $ Not $ Desc "a b")
    "status:1" `gives` (Left $ Status True)
    "status:0" `gives` (Left $ Status False)
    "status:" `gives` (Left $ Status False)
    "real:1" `gives` (Left $ Real True)
    "date:2008" `gives` (Left $ Date $ DateSpan (Just $ parsedate "2008/01/01") (Just $ parsedate "2009/01/01"))
    "date:from 2012/5/17" `gives` (Left $ Date $ DateSpan (Just $ parsedate "2012/05/17") Nothing)
    "inacct:a" `gives` (Right $ QueryOptInAcct "a")
 ]

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

simplifyQuery :: Query -> Query
simplifyQuery (And [q]) = q
simplifyQuery q = q

-- * accessors

-- | Does this query match everything ?
queryIsNull Any = True
queryIsNull (And []) = True
queryIsNull (Not (Or [])) = True
queryIsNull _ = False

-- | What start date does this query specify, if any ?
-- If the query is an OR expression, returns the earliest of the alternatives.
-- When the flag is true, look for a starting effective date instead.
queryStartDate :: Bool -> Query -> Maybe Day
queryStartDate effective (Or ms) = earliestMaybeDate $ map (queryStartDate effective) ms
queryStartDate effective (And ms) = latestMaybeDate $ map (queryStartDate effective) ms
queryStartDate False (Date (DateSpan (Just d) _)) = Just d
queryStartDate True (EDate (DateSpan (Just d) _)) = Just d
queryStartDate _ _ = Nothing

-- | Does this query specify a start date and nothing else (that would
-- filter postings prior to the date) ?
-- When the flag is true, look for a starting effective date instead.
queryIsStartDateOnly :: Bool -> Query -> Bool
queryIsStartDateOnly _ Any = False
queryIsStartDateOnly _ None = False
queryIsStartDateOnly effective (Or ms) = and $ map (queryIsStartDateOnly effective) ms
queryIsStartDateOnly effective (And ms) = and $ map (queryIsStartDateOnly effective) ms
queryIsStartDateOnly False (Date (DateSpan (Just _) _)) = True
queryIsStartDateOnly True (EDate (DateSpan (Just _) _)) = True
queryIsStartDateOnly _ _ = False

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

-- | The account we are currently focussed on, if any, and whether subaccounts are included.
-- Just looks at the first query option.
inAccount :: [QueryOpt] -> Maybe (AccountName,Bool)
inAccount [] = Nothing
inAccount (QueryOptInAcctOnly a:_) = Just (a,False)
inAccount (QueryOptInAcct a:_) = Just (a,True)

-- | A query for the account(s) we are currently focussed on, if any.
-- Just looks at the first query option.
inAccountQuery :: [QueryOpt] -> Maybe Query
inAccountQuery [] = Nothing
inAccountQuery (QueryOptInAcctOnly a:_) = Just $ Acct $ accountNameToAccountOnlyRegex a
inAccountQuery (QueryOptInAcct a:_) = Just $ Acct $ accountNameToAccountRegex a

-- -- | Convert a query to its inverse.
-- negateQuery :: Query -> Query
-- negateQuery =  Not

-- matching

-- | Does the match expression match this posting ?
matchesPosting :: Query -> Posting -> Bool
matchesPosting (Not m) p = not $ matchesPosting m p
matchesPosting (Any) _ = True
matchesPosting (None) _ = False
matchesPosting (Or ms) p = any (`matchesPosting` p) ms
matchesPosting (And ms) p = all (`matchesPosting` p) ms
matchesPosting (Desc r) p = regexMatchesCI r $ maybe "" tdescription $ ptransaction p
matchesPosting (Acct r) p = regexMatchesCI r $ paccount p
matchesPosting (Date span) p =
    case d of Just d'  -> spanContainsDate span d'
              Nothing -> False
    where d = maybe Nothing (Just . tdate) $ ptransaction p
matchesPosting (EDate span) p =
    case postingEffectiveDate p of Just d  -> spanContainsDate span d
                                   Nothing -> False
matchesPosting (Status v) p = v == postingCleared p
matchesPosting (Real v) p = v == isReal p
matchesPosting (Empty v) Posting{pamount=a} = v == isZeroMixedAmount a
matchesPosting _ _ = False

tests_matchesPosting = [
   "matchesPosting" ~: do
    -- matching posting status..
    assertBool "positive match on true posting status"  $
                   (Status True)  `matchesPosting` nullposting{pstatus=True}
    assertBool "negative match on true posting status"  $
               not $ (Not $ Status True)  `matchesPosting` nullposting{pstatus=True}
    assertBool "positive match on false posting status" $
                   (Status False) `matchesPosting` nullposting{pstatus=False}
    assertBool "negative match on false posting status" $
               not $ (Not $ Status False) `matchesPosting` nullposting{pstatus=False}
    assertBool "positive match on true posting status acquired from transaction" $
                   (Status True) `matchesPosting` nullposting{pstatus=False,ptransaction=Just nulltransaction{tstatus=True}}
    assertBool "real:1 on real posting" $ (Real True) `matchesPosting` nullposting{ptype=RegularPosting}
    assertBool "real:1 on virtual posting fails" $ not $ (Real True) `matchesPosting` nullposting{ptype=VirtualPosting}
    assertBool "real:1 on balanced virtual posting fails" $ not $ (Real True) `matchesPosting` nullposting{ptype=BalancedVirtualPosting}
 ]

-- | Does the match expression match this transaction ?
matchesTransaction :: Query -> Transaction -> Bool
matchesTransaction (Not m) t = not $ matchesTransaction m t
matchesTransaction (Any) _ = True
matchesTransaction (None) _ = False
matchesTransaction (Or ms) t = any (`matchesTransaction` t) ms
matchesTransaction (And ms) t = all (`matchesTransaction` t) ms
matchesTransaction (Desc r) t = regexMatchesCI r $ tdescription t
matchesTransaction m@(Acct _) t = any (m `matchesPosting`) $ tpostings t
matchesTransaction (Date span) t = spanContainsDate span $ tdate t
matchesTransaction (EDate span) t = spanContainsDate span $ transactionEffectiveDate t
matchesTransaction (Status v) t = v == tstatus t
matchesTransaction (Real v) t = v == hasRealPostings t
matchesTransaction _ _ = False

postingEffectiveDate :: Posting -> Maybe Day
postingEffectiveDate p = maybe Nothing (Just . transactionEffectiveDate) $ ptransaction p

-- | Does the match expression match this account ?
-- A matching in: clause is also considered a match.
matchesAccount :: Query -> AccountName -> Bool
matchesAccount (Not m) a = not $ matchesAccount m a
matchesAccount (Any) _ = True
matchesAccount (None) _ = False
matchesAccount (Or ms) a = any (`matchesAccount` a) ms
matchesAccount (And ms) a = all (`matchesAccount` a) ms
matchesAccount (Acct r) a = regexMatchesCI r a
matchesAccount _ _ = False

tests_matchesAccount = [
   "matchesAccount" ~: do
    assertBool "positive acct match" $ matchesAccount (Acct "b:c") "a:bb:c:d"
    -- assertBool "acct should match at beginning" $ not $ matchesAccount (Acct True "a:b") "c:a:b"
 ]

-- tests

tests_Hledger_Data_Query :: Test
tests_Hledger_Data_Query = TestList $
 tests_words''
 ++ tests_parseQueryTerm
 ++ tests_parseQuery
 ++ tests_matchesAccount
 ++ tests_matchesPosting

