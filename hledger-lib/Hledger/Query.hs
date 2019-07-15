{-|

A general query system for matching things (accounts, postings,
transactions..)  by various criteria, and a SimpleTextParser for query expressions.

-}

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ViewPatterns #-}
{-# LANGUAGE CPP #-}

module Hledger.Query (
  -- * Query and QueryOpt
  Query(..),
  QueryOpt(..),
  -- * parsing
  parseQuery,
  simplifyQuery,
  filterQuery,
  -- * accessors
  queryIsNull,
  queryIsAcct,
  queryIsAmt,
  queryIsDepth,
  queryIsDate,
  queryIsDate2,
  queryIsDateOrDate2,
  queryIsStartDateOnly,
  queryIsSym,
  queryIsReal,
  queryIsStatus,
  queryIsEmpty,
  queryStartDate,
  queryEndDate,
  queryDateSpan,
  queryDateSpan',
  queryDepth,
  inAccount,
  inAccountQuery,
  -- * matching
  matchesTransaction,
  matchesPosting,
  matchesAccount,
  matchesMixedAmount,
  matchesAmount,
  matchesCommodity,
  matchesPriceDirective,
  words'',
  -- * tests
  tests_Query
)
where

import Data.Data
import Data.Either
import Data.List
import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import Data.Time.Calendar
import Safe (readDef, headDef)
import Text.Megaparsec
import Text.Megaparsec.Char

import Hledger.Utils hiding (words')
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount (nullamt, usd)
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
           | Code Regexp      -- ^ match if code matches this regexp
           | Desc Regexp      -- ^ match if description matches this regexp
           | Acct Regexp      -- ^ match postings whose account matches this regexp
           | Date DateSpan    -- ^ match if primary date in this date span
           | Date2 DateSpan   -- ^ match if secondary date in this date span
           | StatusQ Status  -- ^ match txns/postings with this status
           | Real Bool        -- ^ match if "realness" (involves a real non-virtual account ?) has this value
           | Amt OrdPlus Quantity  -- ^ match if the amount's numeric quantity is less than/greater than/equal to/unsignedly equal to some value
           | Sym Regexp       -- ^ match if the entire commodity symbol is matched by this regexp
           | Empty Bool       -- ^ if true, show zero-amount postings/accounts which are usually not shown
                              --   more of a query option than a query criteria ?
           | Depth Int        -- ^ match if account depth is less than or equal to this value.
                              --   Depth is sometimes used like a query (for filtering report data)
                              --   and sometimes like a query option (for controlling display)
           | Tag Regexp (Maybe Regexp)  -- ^ match if a tag's name, and optionally its value, is matched by these respective regexps
                                        -- matching the regexp if provided, exists
    deriving (Eq,Data,Typeable)

-- custom Show implementation to show strings more accurately, eg for debugging regexps
instance Show Query where
  show Any           = "Any"
  show None          = "None"
  show (Not q)       = "Not ("   ++ show q  ++ ")"
  show (Or qs)       = "Or ("    ++ show qs ++ ")"
  show (And qs)      = "And ("   ++ show qs ++ ")"
  show (Code r)      = "Code "   ++ show r
  show (Desc r)      = "Desc "   ++ show r
  show (Acct r)      = "Acct "   ++ show r
  show (Date ds)     = "Date ("  ++ show ds ++ ")"
  show (Date2 ds)    = "Date2 (" ++ show ds ++ ")"
  show (StatusQ b)    = "StatusQ " ++ show b
  show (Real b)      = "Real "   ++ show b
  show (Amt ord qty) = "Amt "    ++ show ord ++ " " ++ show qty
  show (Sym r)       = "Sym "    ++ show r
  show (Empty b)     = "Empty "  ++ show b
  show (Depth n)     = "Depth "  ++ show n
  show (Tag s ms)    = "Tag "    ++ show s ++ " (" ++ show ms ++ ")"

-- | A more expressive Ord, used for amt: queries. The Abs* variants
-- compare with the absolute value of a number, ignoring sign.
data OrdPlus = Lt | LtEq | Gt | GtEq | Eq | AbsLt | AbsLtEq | AbsGt | AbsGtEq | AbsEq
 deriving (Show,Eq,Data,Typeable)

-- | A query option changes a query's/report's behaviour and output in some way.
data QueryOpt = QueryOptInAcctOnly AccountName  -- ^ show an account register focussed on this account
              | QueryOptInAcct AccountName      -- ^ as above but include sub-accounts in the account register
           -- | QueryOptCostBasis      -- ^ show amounts converted to cost where possible
           -- | QueryOptDate2  -- ^ show secondary dates instead of primary dates
    deriving (Show, Eq, Data, Typeable)

-- parsing

-- -- | A query restricting the account(s) to be shown in the sidebar, if any.
-- -- Just looks at the first query option.
-- showAccountMatcher :: [QueryOpt] -> Maybe Query
-- showAccountMatcher (QueryOptInAcctSubsOnly a:_) = Just $ Acct True $ accountNameToAccountRegex a
-- showAccountMatcher _ = Nothing


-- | Convert a query expression containing zero or more space-separated
-- terms to a query and zero or more query options. A query term is either:
--
-- 1. a search pattern, which matches on one or more fields, eg:
--
--      acct:REGEXP     - match the account name with a regular expression
--      desc:REGEXP     - match the transaction description
--      date:PERIODEXP  - match the date with a period expression
--
--    The prefix indicates the field to match, or if there is no prefix
--    account name is assumed.
--
-- 2. a query option, which modifies the reporting behaviour in some
--    way. There is currently one of these, which may appear only once:
--
--      inacct:FULLACCTNAME
--
-- The usual shell quoting rules are assumed. When a pattern contains
-- whitespace, it (or the whole term including prefix) should be enclosed
-- in single or double quotes.
--
-- Period expressions may contain relative dates, so a reference date is
-- required to fully parse these.
--
-- Multiple terms are combined as follows:
-- 1. multiple account patterns are OR'd together
-- 2. multiple description patterns are OR'd together
-- 3. multiple status patterns are OR'd together
-- 4. then all terms are AND'd together
parseQuery :: Day -> T.Text -> (Query,[QueryOpt])
parseQuery d s = (q, opts)
  where
    terms = words'' prefixes s
    (pats, opts) = partitionEithers $ map (parseQueryTerm d) terms
    (descpats, pats') = partition queryIsDesc pats
    (acctpats, pats'') = partition queryIsAcct pats'
    (statuspats, otherpats) = partition queryIsStatus pats''
    q = simplifyQuery $ And $ [Or acctpats, Or descpats, Or statuspats] ++ otherpats

-- XXX
-- | Quote-and-prefix-aware version of words - don't split on spaces which
-- are inside quotes, including quotes which may have one of the specified
-- prefixes in front, and maybe an additional not: prefix in front of that.
words'' :: [T.Text] -> T.Text -> [T.Text]
words'' prefixes = fromparse . parsewith maybeprefixedquotedphrases -- XXX
    where
      maybeprefixedquotedphrases :: SimpleTextParser [T.Text]
      maybeprefixedquotedphrases = choice' [prefixedQuotedPattern, singleQuotedPattern, doubleQuotedPattern, pattern] `sepBy` skipSome spacenonewline
      prefixedQuotedPattern :: SimpleTextParser T.Text
      prefixedQuotedPattern = do
        not' <- fromMaybe "" `fmap` (optional $ string "not:")
        let allowednexts | T.null not' = prefixes
                         | otherwise   = prefixes ++ [""]
        next <- choice' $ map string allowednexts
        let prefix :: T.Text
            prefix = not' <> next
        p <- singleQuotedPattern <|> doubleQuotedPattern
        return $ prefix <> stripquotes p
      singleQuotedPattern :: SimpleTextParser T.Text
      singleQuotedPattern = between (char '\'') (char '\'') (many $ noneOf ("'" :: [Char])) >>= return . stripquotes . T.pack
      doubleQuotedPattern :: SimpleTextParser T.Text
      doubleQuotedPattern = between (char '"') (char '"') (many $ noneOf ("\"" :: [Char])) >>= return . stripquotes . T.pack
      pattern :: SimpleTextParser T.Text
      pattern = fmap T.pack $ many (noneOf (" \n\r" :: [Char]))

-- XXX
-- keep synced with patterns below, excluding "not"
prefixes :: [T.Text]
prefixes = map (<>":") [
     "inacctonly"
    ,"inacct"
    ,"amt"
    ,"code"
    ,"desc"
    ,"payee"
    ,"note"
    ,"acct"
    ,"date"
    ,"date2"
    ,"status"
    ,"cur"
    ,"real"
    ,"empty"
    ,"depth"
    ,"tag"
    ]

defaultprefix :: T.Text
defaultprefix = "acct"

-- -- | Parse the query string as a boolean tree of match patterns.
-- parseQueryTerm :: String -> Query
-- parseQueryTerm s = either (const (Any)) id $ runParser query () "" $ lexmatcher s

-- lexmatcher :: String -> [String]
-- lexmatcher s = words' s

-- query :: GenParser String () Query
-- query = undefined

-- | Parse a single query term as either a query or a query option,
-- or raise an error if it has invalid syntax.
parseQueryTerm :: Day -> T.Text -> Either Query QueryOpt
parseQueryTerm _ (T.stripPrefix "inacctonly:" -> Just s) = Right $ QueryOptInAcctOnly s
parseQueryTerm _ (T.stripPrefix "inacct:" -> Just s) = Right $ QueryOptInAcct s
parseQueryTerm d (T.stripPrefix "not:" -> Just s) =
  case parseQueryTerm d s of
    Left m -> Left $ Not m
    Right _ -> Left Any -- not:somequeryoption will be ignored
parseQueryTerm _ (T.stripPrefix "code:" -> Just s) = Left $ Code $ T.unpack s
parseQueryTerm _ (T.stripPrefix "desc:" -> Just s) = Left $ Desc $ T.unpack s
parseQueryTerm _ (T.stripPrefix "payee:" -> Just s) = Left $ Tag "payee" $ Just $ T.unpack s
parseQueryTerm _ (T.stripPrefix "note:" -> Just s) = Left $ Tag "note" $ Just $ T.unpack s
parseQueryTerm _ (T.stripPrefix "acct:" -> Just s) = Left $ Acct $ T.unpack s
parseQueryTerm d (T.stripPrefix "date2:" -> Just s) =
        case parsePeriodExpr d s of Left e         -> error' $ "\"date2:"++T.unpack s++"\" gave a "++showDateParseError e
                                    Right (_,span) -> Left $ Date2 span
parseQueryTerm d (T.stripPrefix "date:" -> Just s) =
        case parsePeriodExpr d s of Left e         -> error' $ "\"date:"++T.unpack s++"\" gave a "++showDateParseError e
                                    Right (_,span) -> Left $ Date span
parseQueryTerm _ (T.stripPrefix "status:" -> Just s) =
        case parseStatus s of Left e   -> error' $ "\"status:"++T.unpack s++"\" gave a parse error: " ++ e
                              Right st -> Left $ StatusQ st
parseQueryTerm _ (T.stripPrefix "real:" -> Just s) = Left $ Real $ parseBool s || T.null s
parseQueryTerm _ (T.stripPrefix "amt:" -> Just s) = Left $ Amt ord q where (ord, q) = parseAmountQueryTerm s
parseQueryTerm _ (T.stripPrefix "empty:" -> Just s) = Left $ Empty $ parseBool s
parseQueryTerm _ (T.stripPrefix "depth:" -> Just s)
  | n >= 0    = Left $ Depth n
  | otherwise = error' "depth: should have a positive number"
  where n = readDef 0 (T.unpack s)

parseQueryTerm _ (T.stripPrefix "cur:" -> Just s) = Left $ Sym (T.unpack s) -- support cur: as an alias
parseQueryTerm _ (T.stripPrefix "tag:" -> Just s) = Left $ Tag n v where (n,v) = parseTag s
parseQueryTerm _ "" = Left $ Any
parseQueryTerm d s = parseQueryTerm d $ defaultprefix<>":"<>s

-- | Parse what comes after amt: .
parseAmountQueryTerm :: T.Text -> (OrdPlus, Quantity)
parseAmountQueryTerm s' =
  case s' of
    -- feel free to do this a smarter way
    ""              -> err
    (T.stripPrefix "<+" -> Just s)  -> (Lt, readDef err (T.unpack s))
    (T.stripPrefix "<=+" -> Just s) -> (LtEq, readDef err (T.unpack s))
    (T.stripPrefix ">+" -> Just s)  -> (Gt, readDef err (T.unpack s))
    (T.stripPrefix ">=+" -> Just s) -> (GtEq, readDef err (T.unpack s))
    (T.stripPrefix "=+" -> Just s)  -> (Eq, readDef err (T.unpack s))
    (T.stripPrefix "+" -> Just s)   -> (Eq, readDef err (T.unpack s))
    (T.stripPrefix "<-" -> Just s)  -> (Lt, negate $ readDef err (T.unpack s))
    (T.stripPrefix "<=-" -> Just s) -> (LtEq, negate $ readDef err (T.unpack s))
    (T.stripPrefix ">-" -> Just s)  -> (Gt, negate $ readDef err (T.unpack s))
    (T.stripPrefix ">=-" -> Just s) -> (GtEq, negate $ readDef err (T.unpack s))
    (T.stripPrefix "=-" -> Just s)  -> (Eq, negate $ readDef err (T.unpack s))
    (T.stripPrefix "-" -> Just s)   -> (Eq, negate $ readDef err (T.unpack s))
    (T.stripPrefix "<=" -> Just s)  -> let n = readDef err (T.unpack s) in
                                         case n of
                                           0 -> (LtEq, 0)
                                           _ -> (AbsLtEq, n)
    (T.stripPrefix "<" -> Just s)   -> let n = readDef err (T.unpack s) in
                                         case n of 0 -> (Lt, 0)
                                                   _ -> (AbsLt, n)
    (T.stripPrefix ">=" -> Just s)  -> let n = readDef err (T.unpack s) in
                                         case n of 0 -> (GtEq, 0)
                                                   _ -> (AbsGtEq, n)
    (T.stripPrefix ">" -> Just s)   -> let n = readDef err (T.unpack s) in
                                         case n of 0 -> (Gt, 0)
                                                   _ -> (AbsGt, n)
    (T.stripPrefix "=" -> Just s)           -> (AbsEq, readDef err (T.unpack s))
    s               -> (AbsEq, readDef err (T.unpack s))
  where
    err = error' $ "could not parse as '=', '<', or '>' (optional) followed by a (optionally signed) numeric quantity: " ++ T.unpack s'

parseTag :: T.Text -> (Regexp, Maybe Regexp)
parseTag s | "=" `T.isInfixOf` s = (T.unpack n, Just $ tail $ T.unpack v)
           | otherwise    = (T.unpack s, Nothing)
           where (n,v) = T.break (=='=') s

-- | Parse the value part of a "status:" query, or return an error.
parseStatus :: T.Text -> Either String Status
parseStatus s | s `elem` ["*","1"] = Right Cleared
              | s `elem` ["!"]     = Right Pending
              | s `elem` ["","0"]  = Right Unmarked
              | otherwise          = Left $ "could not parse "++show s++" as a status (should be *, ! or empty)"

-- | Parse the boolean value part of a "status:" query. "1" means true,
-- anything else will be parsed as false without error.
parseBool :: T.Text -> Bool
parseBool s = s `elem` truestrings

truestrings :: [T.Text]
truestrings = ["1"]

simplifyQuery :: Query -> Query
simplifyQuery q =
  let q' = simplify q
  in if q' == q then q else simplifyQuery q'
  where
    simplify (And []) = Any
    simplify (And [q]) = simplify q
    simplify (And qs) | same qs = simplify $ head qs
                      | any (==None) qs = None
                      | all queryIsDate qs = Date $ spansIntersect $ mapMaybe queryTermDateSpan qs
                      | otherwise = And $ concat $ [map simplify dateqs, map simplify otherqs]
                      where (dateqs, otherqs) = partition queryIsDate $ filter (/=Any) qs
    simplify (Or []) = Any
    simplify (Or [q]) = simplifyQuery q
    simplify (Or qs) | same qs = simplify $ head qs
                     | any (==Any) qs = Any
                     -- all queryIsDate qs = Date $ spansUnion $ mapMaybe queryTermDateSpan qs  ?
                     | otherwise = Or $ map simplify $ filter (/=None) qs
    simplify (Date (DateSpan Nothing Nothing)) = Any
    simplify (Date2 (DateSpan Nothing Nothing)) = Any
    simplify q = q

same [] = True
same (a:as) = all (a==) as

-- | Remove query terms (or whole sub-expressions) not matching the given
-- predicate from this query.  XXX Semantics not completely clear.
filterQuery :: (Query -> Bool) -> Query -> Query
filterQuery p = simplifyQuery . filterQuery' p

filterQuery' :: (Query -> Bool) -> Query -> Query
filterQuery' p (And qs) = And $ map (filterQuery p) qs
filterQuery' p (Or qs) = Or $ map (filterQuery p) qs
-- filterQuery' p (Not q) = Not $ filterQuery p q
filterQuery' p q = if p q then q else Any

-- * accessors

-- | Does this query match everything ?
queryIsNull :: Query -> Bool
queryIsNull Any = True
queryIsNull (And []) = True
queryIsNull (Not (Or [])) = True
queryIsNull _ = False

queryIsDepth :: Query -> Bool
queryIsDepth (Depth _) = True
queryIsDepth _ = False

queryIsDate :: Query -> Bool
queryIsDate (Date _) = True
queryIsDate _ = False

queryIsDate2 :: Query -> Bool
queryIsDate2 (Date2 _) = True
queryIsDate2 _ = False

queryIsDateOrDate2 :: Query -> Bool
queryIsDateOrDate2 (Date _) = True
queryIsDateOrDate2 (Date2 _) = True
queryIsDateOrDate2 _ = False

queryIsDesc :: Query -> Bool
queryIsDesc (Desc _) = True
queryIsDesc _ = False

queryIsAcct :: Query -> Bool
queryIsAcct (Acct _) = True
queryIsAcct _ = False

queryIsAmt :: Query -> Bool
queryIsAmt (Amt _ _) = True
queryIsAmt _         = False

queryIsSym :: Query -> Bool
queryIsSym (Sym _) = True
queryIsSym _ = False

queryIsReal :: Query -> Bool
queryIsReal (Real _) = True
queryIsReal _ = False

queryIsStatus :: Query -> Bool
queryIsStatus (StatusQ _) = True
queryIsStatus _ = False

queryIsEmpty :: Query -> Bool
queryIsEmpty (Empty _) = True
queryIsEmpty _ = False

-- | Does this query specify a start date and nothing else (that would
-- filter postings prior to the date) ?
-- When the flag is true, look for a starting secondary date instead.
queryIsStartDateOnly :: Bool -> Query -> Bool
queryIsStartDateOnly _ Any = False
queryIsStartDateOnly _ None = False
queryIsStartDateOnly secondary (Or ms) = and $ map (queryIsStartDateOnly secondary) ms
queryIsStartDateOnly secondary (And ms) = and $ map (queryIsStartDateOnly secondary) ms
queryIsStartDateOnly False (Date (DateSpan (Just _) _)) = True
queryIsStartDateOnly True (Date2 (DateSpan (Just _) _)) = True
queryIsStartDateOnly _ _ = False

-- | What start date (or secondary date) does this query specify, if any ?
-- For OR expressions, use the earliest of the dates. NOT is ignored.
queryStartDate :: Bool -> Query -> Maybe Day
queryStartDate secondary (Or ms) = earliestMaybeDate $ map (queryStartDate secondary) ms
queryStartDate secondary (And ms) = latestMaybeDate $ map (queryStartDate secondary) ms
queryStartDate False (Date (DateSpan (Just d) _)) = Just d
queryStartDate True (Date2 (DateSpan (Just d) _)) = Just d
queryStartDate _ _ = Nothing

-- | What end date (or secondary date) does this query specify, if any ?
-- For OR expressions, use the latest of the dates. NOT is ignored.
queryEndDate :: Bool -> Query -> Maybe Day
queryEndDate secondary (Or ms) = latestMaybeDate' $ map (queryEndDate secondary) ms
queryEndDate secondary (And ms) = earliestMaybeDate' $ map (queryEndDate secondary) ms
queryEndDate False (Date (DateSpan _ (Just d))) = Just d
queryEndDate True (Date2 (DateSpan _ (Just d))) = Just d
queryEndDate _ _ = Nothing

queryTermDateSpan (Date span) = Just span
queryTermDateSpan _ = Nothing

-- | What date span (or with a true argument, what secondary date span) does this query specify ?
-- OR clauses specifying multiple spans return their union (the span enclosing all of them).
-- AND clauses specifying multiple spans return their intersection.
-- NOT clauses are ignored.
queryDateSpan :: Bool -> Query -> DateSpan
queryDateSpan secondary (Or qs)  = spansUnion     $ map (queryDateSpan secondary) qs
queryDateSpan secondary (And qs) = spansIntersect $ map (queryDateSpan secondary) qs
queryDateSpan False (Date span)  = span
queryDateSpan True (Date2 span)  = span
queryDateSpan _ _                = nulldatespan

-- | What date span does this query specify, treating primary and secondary dates as equivalent ?
-- OR clauses specifying multiple spans return their union (the span enclosing all of them).
-- AND clauses specifying multiple spans return their intersection.
-- NOT clauses are ignored.
queryDateSpan' :: Query -> DateSpan
queryDateSpan' (Or qs)      = spansUnion     $ map queryDateSpan' qs
queryDateSpan' (And qs)     = spansIntersect $ map queryDateSpan' qs
queryDateSpan' (Date span)  = span
queryDateSpan' (Date2 span) = span
queryDateSpan' _            = nulldatespan

-- | What is the earliest of these dates, where Nothing is latest ?
earliestMaybeDate :: [Maybe Day] -> Maybe Day
earliestMaybeDate mds = head $ sortBy compareMaybeDates mds ++ [Nothing]

-- | What is the latest of these dates, where Nothing is earliest ?
latestMaybeDate :: [Maybe Day] -> Maybe Day
latestMaybeDate = headDef Nothing . sortBy (flip compareMaybeDates)

-- | What is the earliest of these dates, ignoring Nothings ?
earliestMaybeDate' :: [Maybe Day] -> Maybe Day
earliestMaybeDate' = headDef Nothing . sortBy compareMaybeDates . filter isJust

-- | What is the latest of these dates, ignoring Nothings ?
latestMaybeDate' :: [Maybe Day] -> Maybe Day
latestMaybeDate' = headDef Nothing . sortBy (flip compareMaybeDates) . filter isJust

-- | Compare two maybe dates, Nothing is earliest.
compareMaybeDates :: Maybe Day -> Maybe Day -> Ordering
compareMaybeDates Nothing Nothing = EQ
compareMaybeDates Nothing (Just _) = LT
compareMaybeDates (Just _) Nothing = GT
compareMaybeDates (Just a) (Just b) = compare a b

-- | The depth limit this query specifies, or a large number if none.
queryDepth :: Query -> Int
queryDepth q = case queryDepth' q of [] -> 99999
                                     ds -> minimum ds
  where
    queryDepth' (Depth d) = [d]
    queryDepth' (Or qs) = concatMap queryDepth' qs
    queryDepth' (And qs) = concatMap queryDepth' qs
    queryDepth' _ = []

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
inAccountQuery (QueryOptInAcctOnly a : _) = Just $ Acct $ accountNameToAccountOnlyRegex a
inAccountQuery (QueryOptInAcct a     : _) = Just $ Acct $ accountNameToAccountRegex a

-- -- | Convert a query to its inverse.
-- negateQuery :: Query -> Query
-- negateQuery =  Not

-- matching

-- | Does the match expression match this account ?
-- A matching in: clause is also considered a match.
matchesAccount :: Query -> AccountName -> Bool
matchesAccount (None) _ = False
matchesAccount (Not m) a = not $ matchesAccount m a
matchesAccount (Or ms) a = any (`matchesAccount` a) ms
matchesAccount (And ms) a = all (`matchesAccount` a) ms
matchesAccount (Acct r) a = regexMatchesCI r (T.unpack a) -- XXX pack
matchesAccount (Depth d) a = accountNameLevel a <= d
matchesAccount (Tag _ _) _ = False
matchesAccount _ _ = True

matchesMixedAmount :: Query -> MixedAmount -> Bool
matchesMixedAmount q (Mixed []) = q `matchesAmount` nullamt
matchesMixedAmount q (Mixed as) = any (q `matchesAmount`) as

matchesCommodity :: Query -> CommoditySymbol -> Bool
matchesCommodity (Sym r) s = regexMatchesCI ("^" ++ r ++ "$") (T.unpack s)
matchesCommodity _ _ = True

-- | Does the match expression match this (simple) amount ?
matchesAmount :: Query -> Amount -> Bool
matchesAmount (Not q) a = not $ q `matchesAmount` a
matchesAmount (Any) _ = True
matchesAmount (None) _ = False
matchesAmount (Or qs) a = any (`matchesAmount` a) qs
matchesAmount (And qs) a = all (`matchesAmount` a) qs
--
matchesAmount (Amt ord n) a = compareAmount ord n a
matchesAmount (Sym r) a = matchesCommodity (Sym r) (acommodity a)
--
matchesAmount _ _ = True

-- | Is this simple (single-amount) mixed amount's quantity less than, greater than, equal to, or unsignedly equal to this number ?
-- For multi-amount (multiple commodities, or just unsimplified) mixed amounts this is always true.

-- | Is this amount's quantity less than, greater than, equal to, or unsignedly equal to this number ?
compareAmount :: OrdPlus -> Quantity -> Amount -> Bool
compareAmount ord q Amount{aquantity=aq} = case ord of Lt      -> aq <  q
                                                       LtEq    -> aq <= q
                                                       Gt      -> aq >  q
                                                       GtEq    -> aq >= q
                                                       Eq      -> aq == q
                                                       AbsLt   -> abs aq <  abs q
                                                       AbsLtEq -> abs aq <= abs q
                                                       AbsGt   -> abs aq >  abs q
                                                       AbsGtEq -> abs aq >= abs q
                                                       AbsEq   -> abs aq == abs q

-- | Does the match expression match this posting ?
--
-- Note that for account match we try both original and effective account
matchesPosting :: Query -> Posting -> Bool
matchesPosting (Not q) p = not $ q `matchesPosting` p
matchesPosting (Any) _ = True
matchesPosting (None) _ = False
matchesPosting (Or qs) p = any (`matchesPosting` p) qs
matchesPosting (And qs) p = all (`matchesPosting` p) qs
matchesPosting (Code r) p = regexMatchesCI r $ maybe "" (T.unpack . tcode) $ ptransaction p
matchesPosting (Desc r) p = regexMatchesCI r $ maybe "" (T.unpack . tdescription) $ ptransaction p
matchesPosting (Acct r) p = matchesPosting p || matchesPosting (originalPosting p)
    where matchesPosting p = regexMatchesCI r $ T.unpack $ paccount p -- XXX pack
matchesPosting (Date span) p = span `spanContainsDate` postingDate p
matchesPosting (Date2 span) p = span `spanContainsDate` postingDate2 p
matchesPosting (StatusQ s) p = postingStatus p == s
matchesPosting (Real v) p = v == isReal p
matchesPosting q@(Depth _) Posting{paccount=a} = q `matchesAccount` a
matchesPosting q@(Amt _ _) Posting{pamount=amt} = q `matchesMixedAmount` amt
-- matchesPosting q@(Amt _ _) Posting{pamount=amt} = q `matchesMixedAmount` amt
-- matchesPosting (Empty v) Posting{pamount=a} = v == isZeroMixedAmount a
-- matchesPosting (Empty False) Posting{pamount=a} = True
-- matchesPosting (Empty True) Posting{pamount=a} = isZeroMixedAmount a
matchesPosting (Empty _) _ = True
matchesPosting (Sym r) Posting{pamount=Mixed as} = any (matchesCommodity (Sym r)) $ map acommodity as
matchesPosting (Tag n v) p = case (n, v) of
  ("payee", Just v) -> maybe False (regexMatchesCI v . T.unpack . transactionPayee) $ ptransaction p
  ("note", Just v) -> maybe False (regexMatchesCI v . T.unpack . transactionNote) $ ptransaction p
  (n, v) -> matchesTags n v $ postingAllTags p

-- | Does the match expression match this transaction ?
matchesTransaction :: Query -> Transaction -> Bool
matchesTransaction (Not q) t = not $ q `matchesTransaction` t
matchesTransaction (Any) _ = True
matchesTransaction (None) _ = False
matchesTransaction (Or qs) t = any (`matchesTransaction` t) qs
matchesTransaction (And qs) t = all (`matchesTransaction` t) qs
matchesTransaction (Code r) t = regexMatchesCI r $ T.unpack $ tcode t
matchesTransaction (Desc r) t = regexMatchesCI r $ T.unpack $ tdescription t
matchesTransaction q@(Acct _) t = any (q `matchesPosting`) $ tpostings t
matchesTransaction (Date span) t = spanContainsDate span $ tdate t
matchesTransaction (Date2 span) t = spanContainsDate span $ transactionDate2 t
matchesTransaction (StatusQ s) t = tstatus t == s
matchesTransaction (Real v) t = v == hasRealPostings t
matchesTransaction q@(Amt _ _) t = any (q `matchesPosting`) $ tpostings t
matchesTransaction (Empty _) _ = True
matchesTransaction (Depth d) t = any (Depth d `matchesPosting`) $ tpostings t
matchesTransaction q@(Sym _) t = any (q `matchesPosting`) $ tpostings t
matchesTransaction (Tag n v) t = case (n, v) of
  ("payee", Just v) -> regexMatchesCI v . T.unpack . transactionPayee $ t
  ("note", Just v) -> regexMatchesCI v . T.unpack . transactionNote $ t
  (n, v) -> matchesTags n v $ transactionAllTags t

-- | Filter a list of tags by matching against their names and
-- optionally also their values.
matchesTags :: Regexp -> Maybe Regexp -> [Tag] -> Bool
matchesTags namepat valuepat = not . null . filter (match namepat valuepat)
  where
    match npat Nothing     (n,_) = regexMatchesCI npat (T.unpack n) -- XXX
    match npat (Just vpat) (n,v) = regexMatchesCI npat (T.unpack n) && regexMatchesCI vpat (T.unpack v)

-- | Does the query match this market price ?
matchesPriceDirective :: Query -> PriceDirective -> Bool
matchesPriceDirective (None) _      = False
matchesPriceDirective (Not q) p     = not $ matchesPriceDirective q p
matchesPriceDirective (Or qs) p     = any (`matchesPriceDirective` p) qs
matchesPriceDirective (And qs) p    = all (`matchesPriceDirective` p) qs
matchesPriceDirective q@(Amt _ _) p = matchesAmount q (pdamount p)
matchesPriceDirective q@(Sym _) p   = matchesCommodity q (pdcommodity p)
matchesPriceDirective (Date span) p = spanContainsDate span (pddate p)
matchesPriceDirective _ _           = True


-- tests

tests_Query = tests "Query" [
   tests "simplifyQuery" [

     (simplifyQuery $ Or [Acct "a"])      `is` (Acct "a")
    ,(simplifyQuery $ Or [Any,None])      `is` (Any)
    ,(simplifyQuery $ And [Any,None])     `is` (None)
    ,(simplifyQuery $ And [Any,Any])      `is` (Any)
    ,(simplifyQuery $ And [Acct "b",Any]) `is` (Acct "b")
    ,(simplifyQuery $ And [Any,And [Date (DateSpan Nothing Nothing)]]) `is` (Any)
    ,(simplifyQuery $ And [Date (DateSpan Nothing (Just $ parsedate "2013-01-01")), Date (DateSpan (Just $ parsedate "2012-01-01") Nothing)])
      `is` (Date (DateSpan (Just $ parsedate "2012-01-01") (Just $ parsedate "2013-01-01")))
    ,(simplifyQuery $ And [Or [],Or [Desc "b b"]]) `is` (Desc "b b")
   ]

  ,tests "parseQuery" [
     (parseQuery nulldate "acct:'expenses:autres d\233penses' desc:b") `is` (And [Acct "expenses:autres d\233penses", Desc "b"], [])
    ,parseQuery nulldate "inacct:a desc:\"b b\""                     `is` (Desc "b b", [QueryOptInAcct "a"])
    ,parseQuery nulldate "inacct:a inacct:b"                         `is` (Any, [QueryOptInAcct "a", QueryOptInAcct "b"])
    ,parseQuery nulldate "desc:'x x'"                                `is` (Desc "x x", [])
    ,parseQuery nulldate "'a a' 'b"                                  `is` (Or [Acct "a a",Acct "'b"], [])
    ,parseQuery nulldate "\""                                        `is` (Acct "\"", [])
   ]

  ,tests "words''" [
      (words'' [] "a b")                   `is` ["a","b"]
    , (words'' [] "'a b'")                 `is` ["a b"]
    , (words'' [] "not:a b")               `is` ["not:a","b"]
    , (words'' [] "not:'a b'")             `is` ["not:a b"]
    , (words'' [] "'not:a b'")             `is` ["not:a b"]
    , (words'' ["desc:"] "not:desc:'a b'") `is` ["not:desc:a b"]
    , (words'' prefixes "\"acct:expenses:autres d\233penses\"") `is` ["acct:expenses:autres d\233penses"]
    , (words'' prefixes "\"")              `is` ["\""]
    ]

  ,tests "filterQuery" [
     filterQuery queryIsDepth Any       `is` Any
    ,filterQuery queryIsDepth (Depth 1) `is` Depth 1
    ,filterQuery (not.queryIsDepth) (And [And [StatusQ Cleared,Depth 1]]) `is` StatusQ Cleared
    ,filterQuery queryIsDepth (And [Date nulldatespan, Not (Or [Any, Depth 1])]) `is` Any   -- XXX unclear
   ]

  ,tests "parseQueryTerm" [
     parseQueryTerm nulldate "a"                                `is` (Left $ Acct "a")
    ,parseQueryTerm nulldate "acct:expenses:autres d\233penses" `is` (Left $ Acct "expenses:autres d\233penses")
    ,parseQueryTerm nulldate "not:desc:a b"                     `is` (Left $ Not $ Desc "a b")
    ,parseQueryTerm nulldate "status:1"                         `is` (Left $ StatusQ Cleared)
    ,parseQueryTerm nulldate "status:*"                         `is` (Left $ StatusQ Cleared)
    ,parseQueryTerm nulldate "status:!"                         `is` (Left $ StatusQ Pending)
    ,parseQueryTerm nulldate "status:0"                         `is` (Left $ StatusQ Unmarked)
    ,parseQueryTerm nulldate "status:"                          `is` (Left $ StatusQ Unmarked)
    ,parseQueryTerm nulldate "payee:x"                          `is` (Left $ Tag "payee" (Just "x"))
    ,parseQueryTerm nulldate "note:x"                           `is` (Left $ Tag "note" (Just "x"))
    ,parseQueryTerm nulldate "real:1"                           `is` (Left $ Real True)
    ,parseQueryTerm nulldate "date:2008"                        `is` (Left $ Date $ DateSpan (Just $ parsedate "2008/01/01") (Just $ parsedate "2009/01/01"))
    ,parseQueryTerm nulldate "date:from 2012/5/17"              `is` (Left $ Date $ DateSpan (Just $ parsedate "2012/05/17") Nothing)
    ,parseQueryTerm nulldate "date:20180101-201804"             `is` (Left $ Date $ DateSpan (Just $ parsedate "2018/01/01") (Just $ parsedate "2018/04/01"))
    ,parseQueryTerm nulldate "inacct:a"                         `is` (Right $ QueryOptInAcct "a")
    ,parseQueryTerm nulldate "tag:a"                            `is` (Left $ Tag "a" Nothing)
    ,parseQueryTerm nulldate "tag:a=some value"                 `is` (Left $ Tag "a" (Just "some value"))
    ,parseQueryTerm nulldate "amt:<0"                           `is` (Left $ Amt Lt 0)
    ,parseQueryTerm nulldate "amt:>10000.10"                    `is` (Left $ Amt AbsGt 10000.1)
   ]

  ,tests "parseAmountQueryTerm" [
     parseAmountQueryTerm "<0"        `is` (Lt,0) -- special case for convenience, since AbsLt 0 would be always false
    ,parseAmountQueryTerm ">0"        `is` (Gt,0) -- special case for convenience and consistency with above
    ,parseAmountQueryTerm ">10000.10" `is` (AbsGt,10000.1)
    ,parseAmountQueryTerm "=0.23"     `is` (AbsEq,0.23)
    ,parseAmountQueryTerm "0.23"      `is` (AbsEq,0.23)
    ,parseAmountQueryTerm "<=+0.23"   `is` (LtEq,0.23)
    ,parseAmountQueryTerm "-0.23"     `is` (Eq,(-0.23))
    ,_test "number beginning with decimal mark" $ parseAmountQueryTerm "=.23" `is` (AbsEq,0.23)  -- XXX
    ]

  ,tests "matchesAccount" [
     expect $ (Acct "b:c") `matchesAccount` "a:bb:c:d"
    ,expect $ not $ (Acct "^a:b") `matchesAccount` "c:a:b"
    ,expect $ Depth 2 `matchesAccount` "a"
    ,expect $ Depth 2 `matchesAccount` "a:b"
    ,expect $ not $ Depth 2 `matchesAccount` "a:b:c"
    ,expect $ Date nulldatespan `matchesAccount` "a"
    ,expect $ Date2 nulldatespan `matchesAccount` "a"
    ,expect $ not $ (Tag "a" Nothing) `matchesAccount` "a"
  ]

  ,tests "matchesPosting" [
     test "positive match on cleared posting status"  $
      expect $ (StatusQ Cleared)  `matchesPosting` nullposting{pstatus=Cleared}
    ,test "negative match on cleared posting status"  $
      expect $ not $ (Not $ StatusQ Cleared)  `matchesPosting` nullposting{pstatus=Cleared}
    ,test "positive match on unmarked posting status" $
      expect $ (StatusQ Unmarked) `matchesPosting` nullposting{pstatus=Unmarked}
    ,test "negative match on unmarked posting status" $
      expect $ not $ (Not $ StatusQ Unmarked) `matchesPosting` nullposting{pstatus=Unmarked}
    ,test "positive match on true posting status acquired from transaction" $
      expect $ (StatusQ Cleared) `matchesPosting` nullposting{pstatus=Unmarked,ptransaction=Just nulltransaction{tstatus=Cleared}}
    ,test "real:1 on real posting" $ expect $ (Real True) `matchesPosting` nullposting{ptype=RegularPosting}
    ,test "real:1 on virtual posting fails" $ expect $ not $ (Real True) `matchesPosting` nullposting{ptype=VirtualPosting}
    ,test "real:1 on balanced virtual posting fails" $ expect $ not $ (Real True) `matchesPosting` nullposting{ptype=BalancedVirtualPosting}
    ,test "a" $ expect $ (Acct "'b") `matchesPosting` nullposting{paccount="'b"}
    ,test "b" $ expect $ not $ (Tag "a" (Just "r$")) `matchesPosting` nullposting
    ,test "c" $ expect $ (Tag "foo" Nothing) `matchesPosting` nullposting{ptags=[("foo","")]}
    ,test "d" $ expect $ (Tag "foo" Nothing) `matchesPosting` nullposting{ptags=[("foo","baz")]}
    ,test "e" $ expect $ (Tag "foo" (Just "a")) `matchesPosting` nullposting{ptags=[("foo","bar")]}
    ,test "f" $ expect $ not $ (Tag "foo" (Just "a$")) `matchesPosting` nullposting{ptags=[("foo","bar")]}
    ,test "g" $ expect $ not $ (Tag " foo " (Just "a")) `matchesPosting` nullposting{ptags=[("foo","bar")]}
    ,test "h" $ expect $ not $ (Tag "foo foo" (Just " ar ba ")) `matchesPosting` nullposting{ptags=[("foo foo","bar bar")]}
     -- a tag match on a posting also sees inherited tags
    ,test "i" $ expect $ (Tag "txntag" Nothing) `matchesPosting` nullposting{ptransaction=Just nulltransaction{ttags=[("txntag","")]}}
    ,test "j" $ expect $ not $ (Sym "$") `matchesPosting` nullposting{pamount=Mixed [usd 1]} -- becomes "^$$", ie testing for null symbol
    ,test "k" $ expect $ (Sym "\\$") `matchesPosting` nullposting{pamount=Mixed [usd 1]} -- have to quote $ for regexpr
    ,test "l" $ expect $ (Sym "shekels") `matchesPosting` nullposting{pamount=Mixed [nullamt{acommodity="shekels"}]}
    ,test "m" $ expect $ not $ (Sym "shek") `matchesPosting` nullposting{pamount=Mixed [nullamt{acommodity="shekels"}]}
  ]

  ,tests "matchesTransaction" [
     expect $ Any `matchesTransaction` nulltransaction
    ,expect $ not $ (Desc "x x") `matchesTransaction` nulltransaction{tdescription="x"}
    ,expect $ (Desc "x x") `matchesTransaction` nulltransaction{tdescription="x x"}
     -- see posting for more tag tests
    ,expect $ (Tag "foo" (Just "a")) `matchesTransaction` nulltransaction{ttags=[("foo","bar")]}
    ,expect $ (Tag "payee" (Just "payee")) `matchesTransaction` nulltransaction{tdescription="payee|note"}
    ,expect $ (Tag "note" (Just "note")) `matchesTransaction` nulltransaction{tdescription="payee|note"}
     -- a tag match on a transaction also matches posting tags
    ,expect $ (Tag "postingtag" Nothing) `matchesTransaction` nulltransaction{tpostings=[nullposting{ptags=[("postingtag","")]}]}
  ]

 ]
