{-|

A general query system for matching things (accounts, postings,
transactions..)  by various criteria, and a SimpleTextParser for query expressions.

-}

-- Silence safe 0.3.18's deprecation warnings for (max|min)imum(By)?Def for now
-- (may hide other deprecation warnings too). https://github.com/ndmitchell/safe/issues/26
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

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
  matchesTransaction_,
  matchesPosting,
  matchesPosting_,
  matchesAccount,
  matchesAccount_,
  matchesMixedAmount,
  matchesAmount,
  matchesAmount_,
  matchesCommodity,
  matchesCommodity_,
  matchesTags,
  matchesTags_,
  matchesPriceDirective,
  matchesPriceDirective_,
  words'',
  prefixes,
  -- * tests
  tests_Query
)
where

import Control.Arrow ((>>>))
import Data.Data
import Data.Either
import Data.List
import Data.Maybe
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Text as T
import Data.Time.Calendar
import Safe (readDef, readMay, maximumByMay, maximumMay, minimumMay)
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


-- | Convert a query expression containing zero or more
-- space-separated terms to a query and zero or more query options; or
-- return an error message if query parsing fails.
--
-- A query term is either:
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
--
-- >>> parseQuery nulldate "expenses:dining out"
-- Right (Or ([Acct "expenses:dining",Acct "out"]),[])
--
-- >>> parseQuery nulldate "\"expenses:dining out\""
-- Right (Acct "expenses:dining out",[])
--
parseQuery :: Day -> T.Text -> Either String (Query,[QueryOpt])
parseQuery d s = do
  let termstrs = words'' prefixes s
  eterms <- sequence $ map (parseQueryTerm d) termstrs
  let (pats, opts) = partitionEithers eterms
      (descpats, pats') = partition queryIsDesc pats
      (acctpats, pats'') = partition queryIsAcct pats'
      (statuspats, otherpats) = partition queryIsStatus pats''
      q = simplifyQuery $ And $ [Or acctpats, Or descpats, Or statuspats] ++ otherpats
  Right (q, opts)

-- XXX
-- | Quote-and-prefix-aware version of words - don't split on spaces which
-- are inside quotes, including quotes which may have one of the specified
-- prefixes in front, and maybe an additional not: prefix in front of that.
words'' :: [T.Text] -> T.Text -> [T.Text]
words'' prefixes = fromparse . parsewith maybeprefixedquotedphrases -- XXX
    where
      maybeprefixedquotedphrases :: SimpleTextParser [T.Text]
      maybeprefixedquotedphrases = choice' [prefixedQuotedPattern, singleQuotedPattern, doubleQuotedPattern, pattern] `sepBy` skipNonNewlineSpaces1
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
-- or return an error message if parsing fails.
parseQueryTerm :: Day -> T.Text -> Either String (Either Query QueryOpt)
parseQueryTerm _ (T.stripPrefix "inacctonly:" -> Just s) = Right $ Right $ QueryOptInAcctOnly s
parseQueryTerm _ (T.stripPrefix "inacct:" -> Just s) = Right $ Right $ QueryOptInAcct s
parseQueryTerm d (T.stripPrefix "not:" -> Just s) =
  case parseQueryTerm d s of
    Right (Left m)  -> Right $ Left $ Not m
    Right (Right _) -> Right $ Left Any -- not:somequeryoption will be ignored
    Left err        -> Left err
parseQueryTerm _ (T.stripPrefix "code:" -> Just s) = Right $ Left $ Code $ T.unpack s
parseQueryTerm _ (T.stripPrefix "desc:" -> Just s) = Right $ Left $ Desc $ T.unpack s
parseQueryTerm _ (T.stripPrefix "payee:" -> Just s) = Right $ Left $ Tag "payee" $ Just $ T.unpack s
parseQueryTerm _ (T.stripPrefix "note:" -> Just s) = Right $ Left $ Tag "note" $ Just $ T.unpack s
parseQueryTerm _ (T.stripPrefix "acct:" -> Just s) = Right $ Left $ Acct $ T.unpack s
parseQueryTerm d (T.stripPrefix "date2:" -> Just s) =
        case parsePeriodExpr d s of Left e         -> Left $ "\"date2:"++T.unpack s++"\" gave a "++showDateParseError e
                                    Right (_,span) -> Right $ Left $ Date2 span
parseQueryTerm d (T.stripPrefix "date:" -> Just s) =
        case parsePeriodExpr d s of Left e         -> Left $ "\"date:"++T.unpack s++"\" gave a "++showDateParseError e
                                    Right (_,span) -> Right $ Left $ Date span
parseQueryTerm _ (T.stripPrefix "status:" -> Just s) =
        case parseStatus s of Left e   -> Left $ "\"status:"++T.unpack s++"\" gave a parse error: " ++ e
                              Right st -> Right $ Left $ StatusQ st
parseQueryTerm _ (T.stripPrefix "real:" -> Just s) = Right $ Left $ Real $ parseBool s || T.null s
parseQueryTerm _ (T.stripPrefix "amt:" -> Just s) = Right $ Left $ Amt ord q where (ord, q) = either error id $ parseAmountQueryTerm s  -- PARTIAL:
parseQueryTerm _ (T.stripPrefix "empty:" -> Just s) = Right $ Left $ Empty $ parseBool s
parseQueryTerm _ (T.stripPrefix "depth:" -> Just s)
  | n >= 0    = Right $ Left $ Depth n
  | otherwise = Left "depth: should have a positive number"
  where n = readDef 0 (T.unpack s)

parseQueryTerm _ (T.stripPrefix "cur:" -> Just s) = Right $ Left $ Sym (T.unpack s) -- support cur: as an alias
parseQueryTerm _ (T.stripPrefix "tag:" -> Just s) = Right $ Left $ Tag n v where (n,v) = parseTag s
parseQueryTerm _ "" = Right $ Left $ Any
parseQueryTerm d s = parseQueryTerm d $ defaultprefix<>":"<>s

-- | Parse the argument of an amt query term ([OP][SIGN]NUM), to an
-- OrdPlus and a Quantity, or if parsing fails, an error message. OP
-- can be <=, <, >=, >, or = . NUM can be a simple integer or decimal.
-- If a decimal, the decimal mark must be period, and it must have
-- digits preceding it. Digit group marks are not allowed.
parseAmountQueryTerm :: T.Text -> Either String (OrdPlus, Quantity)
parseAmountQueryTerm amtarg =
  case amtarg of
    -- number has a + sign, do a signed comparison
    (parse "<=+" -> Just q) -> Right (LtEq    ,q)
    (parse "<+"  -> Just q) -> Right (Lt      ,q)
    (parse ">=+" -> Just q) -> Right (GtEq    ,q)
    (parse ">+"  -> Just q) -> Right (Gt      ,q)
    (parse "=+"  -> Just q) -> Right (Eq      ,q)
    (parse "+"   -> Just q) -> Right (Eq      ,q)
    -- number has a - sign, do a signed comparison
    (parse "<-"  -> Just q) -> Right (Lt      ,-q)
    (parse "<=-" -> Just q) -> Right (LtEq    ,-q)
    (parse ">-"  -> Just q) -> Right (Gt      ,-q)
    (parse ">=-" -> Just q) -> Right (GtEq    ,-q)
    (parse "=-"  -> Just q) -> Right (Eq      ,-q)
    (parse "-"   -> Just q) -> Right (Eq      ,-q)
    -- number is unsigned and zero, do a signed comparison (more useful)
    (parse "<="  -> Just 0) -> Right (LtEq    ,0)
    (parse "<"   -> Just 0) -> Right (Lt      ,0)
    (parse ">="  -> Just 0) -> Right (GtEq    ,0)
    (parse ">"   -> Just 0) -> Right (Gt      ,0)
    -- number is unsigned and non-zero, do an absolute magnitude comparison
    (parse "<="  -> Just q) -> Right (AbsLtEq ,q)
    (parse "<"   -> Just q) -> Right (AbsLt   ,q)
    (parse ">="  -> Just q) -> Right (AbsGtEq ,q)
    (parse ">"   -> Just q) -> Right (AbsGt   ,q)
    (parse "="   -> Just q) -> Right (AbsEq   ,q)
    (parse ""    -> Just q) -> Right (AbsEq   ,q)
    _ -> Left $
         "could not parse as a comparison operator followed by an optionally-signed number: "
         ++ T.unpack amtarg
  where
    -- Strip outer whitespace from the text, require and remove the
    -- specified prefix, remove all whitespace from the remainder, and
    -- read it as a simple integer or decimal if possible.
    parse :: T.Text -> T.Text -> Maybe Quantity
    parse p s = (T.stripPrefix p . T.strip) s >>= readMay . filter (not.(==' ')) . T.unpack

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

-- | What is the earliest of these dates, where Nothing is earliest ?
earliestMaybeDate :: [Maybe Day] -> Maybe Day
earliestMaybeDate = fromMaybe Nothing . minimumMay

-- | What is the latest of these dates, where Nothing is earliest ?
latestMaybeDate :: [Maybe Day] -> Maybe Day
latestMaybeDate = fromMaybe Nothing . maximumMay

-- | What is the earliest of these dates, where Nothing is the latest ?
earliestMaybeDate' :: [Maybe Day] -> Maybe Day
earliestMaybeDate' = fromMaybe Nothing . minimumMay . filter isJust

-- | What is the latest of these dates, where Nothing is the latest ?
latestMaybeDate' :: [Maybe Day] -> Maybe Day
latestMaybeDate' = fromMaybe Nothing . maximumByMay compareNothingMax
  where
    compareNothingMax Nothing  Nothing  = EQ
    compareNothingMax (Just _) Nothing  = LT
    compareNothingMax Nothing  (Just _) = GT
    compareNothingMax (Just a) (Just b) = compare a b

-- | The depth limit this query specifies, if it has one
queryDepth :: Query -> Maybe Int
queryDepth = minimumMay . queryDepth'
  where
    queryDepth' (Depth d) = [d]
    queryDepth' (Or qs)   = concatMap queryDepth' qs
    queryDepth' (And qs)  = concatMap queryDepth' qs
    queryDepth' _         = []

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
-- When matching by account name pattern, if there's a regular
-- expression error, this function calls error.
matchesAccount :: Query -> AccountName -> Bool
matchesAccount (None) _ = False
matchesAccount (Not m) a = not $ matchesAccount m a
matchesAccount (Or ms) a = any (`matchesAccount` a) ms
matchesAccount (And ms) a = all (`matchesAccount` a) ms
matchesAccount (Acct r) a = regexMatchesCI r (T.unpack a) -- XXX pack
matchesAccount (Depth d) a = accountNameLevel a <= d
matchesAccount (Tag _ _) _ = False
matchesAccount _ _ = True

-- | Total version of matchesAccount, which will return any error
-- arising from a malformed regular expression in the query.
matchesAccount_ :: Query -> AccountName -> Either RegexError Bool
matchesAccount_ (None) _    = Right False
matchesAccount_ (Not m) a   = Right $ not $ matchesAccount m a
matchesAccount_ (Or ms) a   = sequence (map (`matchesAccount_` a) ms) >>= pure . or
matchesAccount_ (And ms) a  = sequence (map (`matchesAccount_` a) ms) >>= pure . and
matchesAccount_ (Acct r) a  = regexMatchesCI_ r (T.unpack a) -- XXX pack
matchesAccount_ (Depth d) a = Right $ accountNameLevel a <= d
matchesAccount_ (Tag _ _) _ = Right False
matchesAccount_ _ _         = Right True

matchesMixedAmount :: Query -> MixedAmount -> Bool
matchesMixedAmount q (Mixed []) = q `matchesAmount` nullamt
matchesMixedAmount q (Mixed as) = any (q `matchesAmount`) as

matchesCommodity :: Query -> CommoditySymbol -> Bool
matchesCommodity (Sym r) s = regexMatchesCI ("^" ++ r ++ "$") (T.unpack s)
matchesCommodity _ _ = True

-- | Total version of matchesCommodity, which will return any error
-- arising from a malformed regular expression in the query.
matchesCommodity_ :: Query -> CommoditySymbol -> Either RegexError Bool
matchesCommodity_ (Sym r) s = regexMatchesCI_ ("^" ++ r ++ "$") (T.unpack s)
matchesCommodity_ _ _ = Right True

-- | Does the match expression match this (simple) amount ?
matchesAmount :: Query -> Amount -> Bool
matchesAmount (Not q) a = not $ q `matchesAmount` a
matchesAmount (Any) _ = True
matchesAmount (None) _ = False
matchesAmount (Or qs) a = any (`matchesAmount` a) qs
matchesAmount (And qs) a = all (`matchesAmount` a) qs
matchesAmount (Amt ord n) a = compareAmount ord n a
matchesAmount (Sym r) a = matchesCommodity (Sym r) (acommodity a)
matchesAmount _ _ = True

-- | Total version of matchesAmount, returning any error from a
-- malformed regular expression in the query.
matchesAmount_ :: Query -> Amount -> Either RegexError Bool
matchesAmount_ (Not q) a     = not <$> q `matchesAmount_` a
matchesAmount_ (Any) _       = Right True
matchesAmount_ (None) _      = Right False
matchesAmount_ (Or qs) a     = sequence (map (`matchesAmount_` a) qs) >>= pure . or
matchesAmount_ (And qs) a    = sequence (map (`matchesAmount_` a) qs) >>= pure . and
matchesAmount_ (Amt ord n) a = Right $ compareAmount ord n a
matchesAmount_ (Sym r) a     = matchesCommodity_ (Sym r) (acommodity a)
matchesAmount_ _ _           = Right True

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
matchesPosting (Acct r) p = matches p || matches (originalPosting p)
  where matches p = regexMatchesCI r $ T.unpack $ paccount p -- XXX pack
matchesPosting (Date span) p = span `spanContainsDate` postingDate p
matchesPosting (Date2 span) p = span `spanContainsDate` postingDate2 p
matchesPosting (StatusQ s) p = postingStatus p == s
matchesPosting (Real v) p = v == isReal p
matchesPosting q@(Depth _) Posting{paccount=a} = q `matchesAccount` a
matchesPosting q@(Amt _ _) Posting{pamount=amt} = q `matchesMixedAmount` amt
-- matchesPosting q@(Amt _ _) Posting{pamount=amt} = q `matchesMixedAmount` amt
-- matchesPosting (Empty v) Posting{pamount=a} = v == mixedAmountLooksZero a
-- matchesPosting (Empty False) Posting{pamount=a} = True
-- matchesPosting (Empty True) Posting{pamount=a} = mixedAmountLooksZero a
matchesPosting (Empty _) _ = True
matchesPosting (Sym r) Posting{pamount=Mixed as} = any (matchesCommodity (Sym r)) $ map acommodity as
matchesPosting (Tag n v) p = case (n, v) of
  ("payee", Just v) -> maybe False (regexMatchesCI v . T.unpack . transactionPayee) $ ptransaction p
  ("note", Just v) -> maybe False (regexMatchesCI v . T.unpack . transactionNote) $ ptransaction p
  (n, v) -> matchesTags n v $ postingAllTags p

-- | Total version of matchesPosting, returning any error from a
-- malformed regular expression in the query.
matchesPosting_ :: Query -> Posting -> Either RegexError Bool
matchesPosting_ (Not q) p                         = not <$> q `matchesPosting_` p
matchesPosting_ (Any) _                           = Right True
matchesPosting_ (None) _                          = Right False
matchesPosting_ (Or qs) p                         = sequence (map (`matchesPosting_` p) qs) >>= pure.or
matchesPosting_ (And qs) p                        = sequence (map (`matchesPosting_` p) qs) >>= pure.and
matchesPosting_ (Code r) p                        = regexMatchesCI_ r $ maybe "" (T.unpack . tcode) $ ptransaction p
matchesPosting_ (Desc r) p                        = regexMatchesCI_ r $ maybe "" (T.unpack . tdescription) $ ptransaction p
matchesPosting_ (Acct r) p                        = sequence [matches p, matches (originalPosting p)] >>= pure.or
  where matches p = regexMatchesCI_ r $ T.unpack $ paccount p -- XXX pack
matchesPosting_ (Date span) p                     = Right $ span `spanContainsDate` postingDate p
matchesPosting_ (Date2 span) p                    = Right $ span `spanContainsDate` postingDate2 p
matchesPosting_ (StatusQ s) p                     = Right $ postingStatus p == s
matchesPosting_ (Real v) p                        = Right $ v == isReal p
matchesPosting_ q@(Depth _) Posting{paccount=a}   = q `matchesAccount_` a
matchesPosting_ q@(Amt _ _) Posting{pamount=amt}  = Right $ q `matchesMixedAmount` amt
matchesPosting_ (Empty _) _                       = Right True
matchesPosting_ (Sym r) Posting{pamount=Mixed as} = sequence (map (matchesCommodity_ (Sym r)) $ map acommodity as) >>= pure.or
matchesPosting_ (Tag n v) p                       = case (n, v) of
  ("payee", Just v) -> maybe (Right False) (T.unpack . transactionPayee >>> regexMatchesCI_ v) $ ptransaction p
  ("note", Just v)  -> maybe (Right False) (T.unpack . transactionNote  >>> regexMatchesCI_ v) $ ptransaction p
  (n, v)            -> matchesTags_ n v $ postingAllTags p

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

-- | Total version of matchesTransaction, returning any error from a
-- malformed regular expression in the query.
matchesTransaction_ :: Query -> Transaction -> Either RegexError Bool
matchesTransaction_ (Not q) t      = not <$> q `matchesTransaction_` t
matchesTransaction_ (Any) _        = Right True
matchesTransaction_ (None) _       = Right False
matchesTransaction_ (Or qs) t      = sequence (map (`matchesTransaction_` t) qs) >>= pure.or
matchesTransaction_ (And qs) t     = sequence (map (`matchesTransaction_` t) qs) >>= pure.and
matchesTransaction_ (Code r) t     = regexMatchesCI_ r $ T.unpack $ tcode t
matchesTransaction_ (Desc r) t     = regexMatchesCI_ r $ T.unpack $ tdescription t
matchesTransaction_ q@(Acct _) t   = sequence (map (q `matchesPosting_`) $ tpostings t) >>= pure.or
matchesTransaction_ (Date span) t  = Right $ spanContainsDate span $ tdate t
matchesTransaction_ (Date2 span) t = Right $ spanContainsDate span $ transactionDate2 t
matchesTransaction_ (StatusQ s) t  = Right $ tstatus t == s
matchesTransaction_ (Real v) t     = Right $ v == hasRealPostings t
matchesTransaction_ q@(Amt _ _) t  = sequence (map (q `matchesPosting_`) $ tpostings t) >>= pure.or
matchesTransaction_ (Empty _) _    = Right True
matchesTransaction_ (Depth d) t    = sequence (map (Depth d `matchesPosting_`) $ tpostings t) >>= pure.or
matchesTransaction_ q@(Sym _) t    = sequence (map (q `matchesPosting_`) $ tpostings t) >>= pure.or
matchesTransaction_ (Tag n v) t    = case (n, v) of
  ("payee", Just v) -> regexMatchesCI_ v . T.unpack . transactionPayee $ t
  ("note", Just v)  -> regexMatchesCI_ v . T.unpack . transactionNote $ t
  (n, v)            -> matchesTags_ n v $ transactionAllTags t

-- | Does the query match the name and optionally the value of any of these tags ?
matchesTags :: Regexp -> Maybe Regexp -> [Tag] -> Bool
matchesTags namepat valuepat = not . null . filter (match namepat valuepat)
  where
    match npat Nothing     (n,_) = regexMatchesCI npat (T.unpack n) -- XXX
    match npat (Just vpat) (n,v) = regexMatchesCI npat (T.unpack n) && regexMatchesCI vpat (T.unpack v)

-- | Total version of matchesTags, returning any error from a
-- malformed regular expression in the query.
matchesTags_ :: Regexp -> Maybe Regexp -> [Tag] -> Either RegexError Bool
matchesTags_ namepat valuepat tags =
  sequence (map (match namepat valuepat) tags) >>= pure.or
  where
    match npat Nothing     (n,_) = regexMatchesCI_ npat (T.unpack n) -- XXX
    match npat (Just vpat) (n,v) =
      sequence [regexMatchesCI_ npat (T.unpack n), regexMatchesCI_ vpat (T.unpack v)] >>= pure.and

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

-- | Total version of matchesPriceDirective, returning any error from
-- a malformed regular expression in the query.
matchesPriceDirective_ :: Query -> PriceDirective -> Either RegexError Bool
matchesPriceDirective_ (None) _      = Right False
matchesPriceDirective_ (Not q) p     = not <$> matchesPriceDirective_ q p
matchesPriceDirective_ (Or qs) p     = sequence (map (`matchesPriceDirective_` p) qs) >>= pure.or
matchesPriceDirective_ (And qs) p    = sequence (map (`matchesPriceDirective_` p) qs) >>= pure.and
matchesPriceDirective_ q@(Amt _ _) p = matchesAmount_ q (pdamount p)
matchesPriceDirective_ q@(Sym _) p   = matchesCommodity_ q (pdcommodity p)
matchesPriceDirective_ (Date span) p = Right $ spanContainsDate span (pddate p)
matchesPriceDirective_ _ _           = Right True


-- tests

tests_Query = tests "Query" [
   test "simplifyQuery" $ do
     (simplifyQuery $ Or [Acct "a"])      @?= (Acct "a")
     (simplifyQuery $ Or [Any,None])      @?= (Any)
     (simplifyQuery $ And [Any,None])     @?= (None)
     (simplifyQuery $ And [Any,Any])      @?= (Any)
     (simplifyQuery $ And [Acct "b",Any]) @?= (Acct "b")
     (simplifyQuery $ And [Any,And [Date (DateSpan Nothing Nothing)]]) @?= (Any)
     (simplifyQuery $ And [Date (DateSpan Nothing (Just $ fromGregorian 2013 01 01)), Date (DateSpan (Just $ fromGregorian 2012 01 01) Nothing)])
       @?= (Date (DateSpan (Just $ fromGregorian 2012 01 01) (Just $ fromGregorian 2013 01 01)))
     (simplifyQuery $ And [Or [],Or [Desc "b b"]]) @?= (Desc "b b")

  ,test "parseQuery" $ do
     (parseQuery nulldate "acct:'expenses:autres d\233penses' desc:b") @?= Right (And [Acct "expenses:autres d\233penses", Desc "b"], [])
     parseQuery nulldate "inacct:a desc:\"b b\""                       @?= Right (Desc "b b", [QueryOptInAcct "a"])
     parseQuery nulldate "inacct:a inacct:b"                           @?= Right (Any, [QueryOptInAcct "a", QueryOptInAcct "b"])
     parseQuery nulldate "desc:'x x'"                                  @?= Right (Desc "x x", [])
     parseQuery nulldate "'a a' 'b"                                    @?= Right (Or [Acct "a a",Acct "'b"], [])
     parseQuery nulldate "\""                                          @?= Right (Acct "\"", [])

  ,test "words''" $ do
      (words'' [] "a b")                   @?= ["a","b"]
      (words'' [] "'a b'")                 @?= ["a b"]
      (words'' [] "not:a b")               @?= ["not:a","b"]
      (words'' [] "not:'a b'")             @?= ["not:a b"]
      (words'' [] "'not:a b'")             @?= ["not:a b"]
      (words'' ["desc:"] "not:desc:'a b'") @?= ["not:desc:a b"]
      (words'' prefixes "\"acct:expenses:autres d\233penses\"") @?= ["acct:expenses:autres d\233penses"]
      (words'' prefixes "\"")              @?= ["\""]

  ,test "filterQuery" $ do
     filterQuery queryIsDepth Any       @?= Any
     filterQuery queryIsDepth (Depth 1) @?= Depth 1
     filterQuery (not.queryIsDepth) (And [And [StatusQ Cleared,Depth 1]]) @?= StatusQ Cleared
     filterQuery queryIsDepth (And [Date nulldatespan, Not (Or [Any, Depth 1])]) @?= Any   -- XXX unclear

  ,test "parseQueryTerm" $ do
     parseQueryTerm nulldate "a"                                @?= Right (Left $ Acct "a")
     parseQueryTerm nulldate "acct:expenses:autres d\233penses" @?= Right (Left $ Acct "expenses:autres d\233penses")
     parseQueryTerm nulldate "not:desc:a b"                     @?= Right (Left $ Not $ Desc "a b")
     parseQueryTerm nulldate "status:1"                         @?= Right (Left $ StatusQ Cleared)
     parseQueryTerm nulldate "status:*"                         @?= Right (Left $ StatusQ Cleared)
     parseQueryTerm nulldate "status:!"                         @?= Right (Left $ StatusQ Pending)
     parseQueryTerm nulldate "status:0"                         @?= Right (Left $ StatusQ Unmarked)
     parseQueryTerm nulldate "status:"                          @?= Right (Left $ StatusQ Unmarked)
     parseQueryTerm nulldate "payee:x"                          @?= Right (Left $ Tag "payee" (Just "x"))
     parseQueryTerm nulldate "note:x"                           @?= Right (Left $ Tag "note" (Just "x"))
     parseQueryTerm nulldate "real:1"                           @?= Right (Left $ Real True)
     parseQueryTerm nulldate "date:2008"                        @?= Right (Left $ Date $ DateSpan (Just $ fromGregorian 2008 01 01) (Just $ fromGregorian 2009 01 01))
     parseQueryTerm nulldate "date:from 2012/5/17"              @?= Right (Left $ Date $ DateSpan (Just $ fromGregorian 2012 05 17) Nothing)
     parseQueryTerm nulldate "date:20180101-201804"             @?= Right (Left $ Date $ DateSpan (Just $ fromGregorian 2018 01 01) (Just $ fromGregorian 2018 04 01))
     parseQueryTerm nulldate "inacct:a"                         @?= Right (Right $ QueryOptInAcct "a")
     parseQueryTerm nulldate "tag:a"                            @?= Right (Left $ Tag "a" Nothing)
     parseQueryTerm nulldate "tag:a=some value"                 @?= Right (Left $ Tag "a" (Just "some value"))
     parseQueryTerm nulldate "amt:<0"                           @?= Right (Left $ Amt Lt 0)
     parseQueryTerm nulldate "amt:>10000.10"                    @?= Right (Left $ Amt AbsGt 10000.1)

  ,test "parseAmountQueryTerm" $ do
     parseAmountQueryTerm "<0"        @?= Right (Lt,0) -- special case for convenience, since AbsLt 0 would be always false
     parseAmountQueryTerm ">0"        @?= Right (Gt,0) -- special case for convenience and consistency with above
     parseAmountQueryTerm " > - 0 "   @?= Right (Gt,0) -- accept whitespace around the argument parts
     parseAmountQueryTerm ">10000.10" @?= Right (AbsGt,10000.1)
     parseAmountQueryTerm "=0.23"     @?= Right (AbsEq,0.23)
     parseAmountQueryTerm "0.23"      @?= Right (AbsEq,0.23)
     parseAmountQueryTerm "<=+0.23"   @?= Right (LtEq,0.23)
     parseAmountQueryTerm "-0.23"     @?= Right (Eq,(-0.23))
     assertLeft $ parseAmountQueryTerm "-0,23"
     assertLeft $ parseAmountQueryTerm "=.23"

  ,test "queryStartDate" $ do
     let small = Just $ fromGregorian 2000 01 01
         big   = Just $ fromGregorian 2000 01 02
     queryStartDate False (And [Date $ DateSpan small Nothing, Date $ DateSpan big Nothing])     @?= big
     queryStartDate False (And [Date $ DateSpan small Nothing, Date $ DateSpan Nothing Nothing]) @?= small
     queryStartDate False (Or  [Date $ DateSpan small Nothing, Date $ DateSpan big Nothing])     @?= small
     queryStartDate False (Or  [Date $ DateSpan small Nothing, Date $ DateSpan Nothing Nothing]) @?= Nothing

  ,test "queryEndDate" $ do
     let small = Just $ fromGregorian 2000 01 01
         big   = Just $ fromGregorian 2000 01 02
     queryEndDate False (And [Date $ DateSpan Nothing small, Date $ DateSpan Nothing big])     @?= small
     queryEndDate False (And [Date $ DateSpan Nothing small, Date $ DateSpan Nothing Nothing]) @?= small
     queryEndDate False (Or  [Date $ DateSpan Nothing small, Date $ DateSpan Nothing big])     @?= big
     queryEndDate False (Or  [Date $ DateSpan Nothing small, Date $ DateSpan Nothing Nothing]) @?= Nothing

  ,test "matchesAccount" $ do
     assertBool "" $ (Acct "b:c") `matchesAccount` "a:bb:c:d"
     assertBool "" $ not $ (Acct "^a:b") `matchesAccount` "c:a:b"
     assertBool "" $ Depth 2 `matchesAccount` "a"
     assertBool "" $ Depth 2 `matchesAccount` "a:b"
     assertBool "" $ not $ Depth 2 `matchesAccount` "a:b:c"
     assertBool "" $ Date nulldatespan `matchesAccount` "a"
     assertBool "" $ Date2 nulldatespan `matchesAccount` "a"
     assertBool "" $ not $ (Tag "a" Nothing) `matchesAccount` "a"

  ,tests "matchesPosting" [
     test "positive match on cleared posting status"  $
      assertBool "" $ (StatusQ Cleared)  `matchesPosting` nullposting{pstatus=Cleared}
    ,test "negative match on cleared posting status"  $
      assertBool "" $ not $ (Not $ StatusQ Cleared)  `matchesPosting` nullposting{pstatus=Cleared}
    ,test "positive match on unmarked posting status" $
      assertBool "" $ (StatusQ Unmarked) `matchesPosting` nullposting{pstatus=Unmarked}
    ,test "negative match on unmarked posting status" $
      assertBool "" $ not $ (Not $ StatusQ Unmarked) `matchesPosting` nullposting{pstatus=Unmarked}
    ,test "positive match on true posting status acquired from transaction" $
      assertBool "" $ (StatusQ Cleared) `matchesPosting` nullposting{pstatus=Unmarked,ptransaction=Just nulltransaction{tstatus=Cleared}}
    ,test "real:1 on real posting" $ assertBool "" $ (Real True) `matchesPosting` nullposting{ptype=RegularPosting}
    ,test "real:1 on virtual posting fails" $ assertBool "" $ not $ (Real True) `matchesPosting` nullposting{ptype=VirtualPosting}
    ,test "real:1 on balanced virtual posting fails" $ assertBool "" $ not $ (Real True) `matchesPosting` nullposting{ptype=BalancedVirtualPosting}
    ,test "acct:" $ assertBool "" $ (Acct "'b") `matchesPosting` nullposting{paccount="'b"}
    ,test "tag:" $ do
      assertBool "" $ not $ (Tag "a" (Just "r$")) `matchesPosting` nullposting
      assertBool "" $ (Tag "foo" Nothing) `matchesPosting` nullposting{ptags=[("foo","")]}
      assertBool "" $ (Tag "foo" Nothing) `matchesPosting` nullposting{ptags=[("foo","baz")]}
      assertBool "" $ (Tag "foo" (Just "a")) `matchesPosting` nullposting{ptags=[("foo","bar")]}
      assertBool "" $ not $ (Tag "foo" (Just "a$")) `matchesPosting` nullposting{ptags=[("foo","bar")]}
      assertBool "" $ not $ (Tag " foo " (Just "a")) `matchesPosting` nullposting{ptags=[("foo","bar")]}
      assertBool "" $ not $ (Tag "foo foo" (Just " ar ba ")) `matchesPosting` nullposting{ptags=[("foo foo","bar bar")]}
    ,test "a tag match on a posting also sees inherited tags" $ assertBool "" $ (Tag "txntag" Nothing) `matchesPosting` nullposting{ptransaction=Just nulltransaction{ttags=[("txntag","")]}}
    ,test "cur:" $ do
      assertBool "" $ not $ (Sym "$") `matchesPosting` nullposting{pamount=Mixed [usd 1]} -- becomes "^$$", ie testing for null symbol
      assertBool "" $ (Sym "\\$") `matchesPosting` nullposting{pamount=Mixed [usd 1]} -- have to quote $ for regexpr
      assertBool "" $ (Sym "shekels") `matchesPosting` nullposting{pamount=Mixed [nullamt{acommodity="shekels"}]}
      assertBool "" $ not $ (Sym "shek") `matchesPosting` nullposting{pamount=Mixed [nullamt{acommodity="shekels"}]}
  ]

  ,test "matchesTransaction" $ do
     assertBool "" $ Any `matchesTransaction` nulltransaction
     assertBool "" $ not $ (Desc "x x") `matchesTransaction` nulltransaction{tdescription="x"}
     assertBool "" $ (Desc "x x") `matchesTransaction` nulltransaction{tdescription="x x"}
     -- see posting for more tag tests
     assertBool "" $ (Tag "foo" (Just "a")) `matchesTransaction` nulltransaction{ttags=[("foo","bar")]}
     assertBool "" $ (Tag "payee" (Just "payee")) `matchesTransaction` nulltransaction{tdescription="payee|note"}
     assertBool "" $ (Tag "note" (Just "note")) `matchesTransaction` nulltransaction{tdescription="payee|note"}
     -- a tag match on a transaction also matches posting tags
     assertBool "" $ (Tag "postingtag" Nothing) `matchesTransaction` nulltransaction{tpostings=[nullposting{ptags=[("postingtag","")]}]}

 ]
