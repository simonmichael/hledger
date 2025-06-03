{-|

A general query system for matching things (accounts, postings,
transactions..)  by various criteria, and a SimpleTextParser for query expressions.

-}

{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE TupleSections      #-}

module Hledger.Query (
  -- * Query and QueryOpt
  Query(..),
  QueryOpt(..),
  OrdPlus(..),
  payeeTag,
  noteTag,
  generatedTransactionTag,
  -- * parsing
  parseQuery,
  parseQueryList,
  parseQueryTerm,
  parseAccountType,
  parseDepthSpec,
  -- * modifying
  simplifyQuery,
  filterQuery,
  filterQueryOrNotQuery,
  matchesQuery,
  -- * predicates
  queryIsNull,
  queryIsDate,
  queryIsDate2,
  queryIsDateOrDate2,
  queryIsStatus,
  queryIsCode,
  queryIsDesc,
  queryIsTag,
  queryIsAcct,
  queryIsType,
  queryIsDepth,
  queryIsReal,
  queryIsAmt,
  queryIsSym,
  queryIsStartDateOnly,
  queryIsTransactionRelated,
  -- * accessors
  queryStartDate,
  queryEndDate,
  queryDateSpan,
  queryDateSpan',
  queryDepth,
  inAccount,
  inAccountQuery,
  -- * matching things with queries
  matchesTransaction,
  matchesTransactionExtra,
  matchesDescription,
  matchesPayeeWIP,
  matchesPosting,
  matchesPostingExtra,
  matchesAccount,
  matchesAccountExtra,
  matchesMixedAmount,
  matchesAmount,
  matchesCommodity,
  matchesTags,
  matchesPriceDirective,
  words'',
  queryprefixes,
  -- * tests
  tests_Query
)
where

import Control.Applicative ((<|>), many, optional)
import Data.Default (Default(..))
import Data.Either (partitionEithers)
import Data.List (partition, intercalate)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, fromGregorian )
import Safe (headErr, readMay, maximumByMay, maximumMay, minimumMay)
import Text.Megaparsec (between, noneOf, sepBy, try, (<?>), notFollowedBy)
import Text.Megaparsec.Char (char, string, string')


import Hledger.Utils hiding (words')
import Hledger.Data.Types
import Hledger.Data.AccountName
import Hledger.Data.Amount (amountsRaw, mixedAmount, nullamt, usd)
import Hledger.Data.Dates
import Hledger.Data.Posting
import Hledger.Data.Transaction
import Data.Bifunctor


-- | A query is a composition of search criteria, which can be used to
-- match postings, transactions, accounts and more.
data Query = 
  -- no-op queries
    Any                       -- ^ always match
  | None                      -- ^ never match
  -- data queries (in "standard" order, roughly as they appear in a transaction)
  | Date DateSpan             -- ^ match primary dates in this date span
  | Date2 DateSpan            -- ^ match secondary dates in this date span
  | StatusQ Status            -- ^ match this txn/posting status
  | Code Regexp               -- ^ match txn codes infix-matched by this regexp
  | Desc Regexp               -- ^ match txn descriptions infix-matched by this regexp
  | Tag Regexp (Maybe Regexp) -- ^ match if a tag's name, and optionally its value, is infix-matched by the respective regexps
  | Acct Regexp               -- ^ match account names infix-matched by this regexp
  | Type [AccountType]        -- ^ match accounts whose type is one of these (or with no types, any account)
  | Depth Int                 -- ^ match if account depth is less than or equal to this value (or, sometimes used as a display option)
  | DepthAcct Regexp Int      -- ^ match if the account matches and account depth is less than or equal to this value (usually used as a display option)
  | Real Bool                 -- ^ match postings with this "realness" value
  | Amt OrdPlus Quantity      -- ^ match if the amount's numeric quantity is less than/greater than/equal to/unsignedly equal to some value
  | Sym Regexp                -- ^ match if the commodity symbol is fully-matched by this regexp
  -- compound queries (expr:)
  | Not Query                 -- ^ negate this match
  | And [Query]               -- ^ match if all of these match
  | Or  [Query]               -- ^ match if any of these match
  -- compound queries for transactions (any:, all:)
  -- If used in a non transaction-matching context, these are equivalent to And.
  | AnyPosting  [Query]       -- ^ match if any one posting is matched by all of these
  | AllPostings [Query]       -- ^ match if all postings are matched by all of these
  deriving (Eq,Show)

instance Default Query where def = Any

-- | Construct a query for the payee: tag
payeeTag :: Maybe Text -> Either RegexError Query
payeeTag = fmap (Tag (toRegexCI' "payee")) . maybe (pure Nothing) (fmap Just . toRegexCI)

-- | Construct a query for the note: tag
noteTag :: Maybe Text -> Either RegexError Query
noteTag = fmap (Tag (toRegexCI' "note")) . maybe (pure Nothing) (fmap Just . toRegexCI)

-- | Construct a query for the generated-transaction: tag
generatedTransactionTag :: Query
generatedTransactionTag = Tag (toRegexCI' "generated-transaction") Nothing

-- | A more expressive Ord, used for amt: queries. The Abs* variants
-- compare with the absolute value of a number, ignoring sign.
data OrdPlus = Lt | LtEq | Gt | GtEq | Eq | AbsLt | AbsLtEq | AbsGt | AbsGtEq | AbsEq
 deriving (Show,Eq)

-- | A query option changes a query's/report's behaviour and output in some way.
data QueryOpt = QueryOptInAcctOnly AccountName  -- ^ show an account register focussed on this account
              | QueryOptInAcct AccountName      -- ^ as above but include sub-accounts in the account register
           -- | QueryOptCostBasis      -- ^ show amounts converted to cost where possible
           -- | QueryOptDate2  -- ^ show secondary dates instead of primary dates
    deriving (Show, Eq)

-- parsing

-- -- | A query restricting the account(s) to be shown in the sidebar, if any.
-- -- Just looks at the first query option.
-- showAccountMatcher :: [QueryOpt] -> Maybe Query
-- showAccountMatcher (QueryOptInAcctSubsOnly a:_) = Just $ Acct True $ accountNameToAccountRegex a
-- showAccountMatcher _ = Nothing


-- | A version of parseQueryList which acts on a single Text of
-- space-separated terms.
--
-- The usual shell quoting rules are assumed. When a pattern contains
-- whitespace, it (or the whole term including prefix) should be enclosed
-- in single or double quotes.
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
-- Period expressions may contain relative dates, so a reference date is
-- required to fully parse these.
--
-- >>> parseQuery nulldate "expenses:dining out"
-- Right (Or [Acct (RegexpCI "expenses:dining"),Acct (RegexpCI "out")],[])
--
-- >>> parseQuery nulldate "\"expenses:dining out\""
-- Right (Acct (RegexpCI "expenses:dining out"),[])
parseQuery :: Day -> T.Text -> Either String (Query,[QueryOpt])
parseQuery d t = parseQueryList d $ words'' queryprefixes t

-- | Convert a list of space-separated queries to a single query
--
-- Multiple terms are combined as follows:
-- 1. multiple account patterns are OR'd together
-- 2. multiple description patterns are OR'd together
-- 3. multiple status patterns are OR'd together
-- 4. then all terms are AND'd together
parseQueryList :: Day -> [T.Text] -> Either String (Query, [QueryOpt])
parseQueryList d termstrs = do
  eterms <- mapM (parseQueryTerm d) termstrs
  let (pats, optss) = unzip eterms
      q = combineQueriesByType pats
  Right (q, concat optss)

combineQueriesByType :: [Query] -> Query
combineQueriesByType pats = q
  where
    (descpats, pats') = partition queryIsDesc pats
    (acctpats, pats'') = partition queryIsAcct pats'
    (statuspats, otherpats) = partition queryIsStatus pats''
    q = simplifyQuery $ And $ [Or acctpats, Or descpats, Or statuspats] ++ otherpats
  
-- XXX
-- | Quote-and-prefix-aware version of words - don't split on spaces which
-- are inside quotes, including quotes which may have one of the specified
-- prefixes in front, and maybe an additional not: prefix in front of that.
words'' :: [T.Text] -> T.Text -> [T.Text]
words'' prefixes = fromparse . parsewith maybePrefixedQuotedPhrases -- XXX
   where
      maybePrefixedQuotedPhrases :: SimpleTextParser [T.Text]
      maybePrefixedQuotedPhrases = choice' [prefixedQuotedPattern, singleQuotedPattern, doubleQuotedPattern, patterns] `sepBy`
          (notFollowedBy (skipNonNewlineSpaces >> char ')') >> skipNonNewlineSpaces1)
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
      singleQuotedPattern = stripquotes . T.pack <$> between (char '\'') (char '\'') (many $ noneOf ("'" :: [Char]))
      doubleQuotedPattern :: SimpleTextParser T.Text
      doubleQuotedPattern = stripquotes . T.pack <$> between (char '"') (char '"') (many $ noneOf ("\"" :: [Char]))
      patterns :: SimpleTextParser T.Text
      patterns = T.pack <$> many (noneOf (" \n\r" :: [Char]))

-- XXX
-- keep synced with patterns below, excluding "not"
queryprefixes :: [T.Text]
queryprefixes = map (<>":") [
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
    ,"type"
    ,"expr"
    ,"any"
    ,"all"
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
parseQueryTerm :: Day -> T.Text -> Either String (Query, [QueryOpt])
parseQueryTerm _ (T.stripPrefix "inacctonly:" -> Just s) = Right (Any, [QueryOptInAcctOnly s])
parseQueryTerm _ (T.stripPrefix "inacct:" -> Just s) = Right (Any, [QueryOptInAcct s])
parseQueryTerm d (T.stripPrefix "not:" -> Just s) =
  case parseQueryTerm d s of
    Right (q, qopts) -> Right (Not q, qopts)
    Left err         -> Left err
parseQueryTerm _ (T.stripPrefix "code:" -> Just s) = (,[]) . Code <$> toRegexCI s
parseQueryTerm _ (T.stripPrefix "desc:" -> Just s) = (,[]) . Desc <$> toRegexCI s
parseQueryTerm _ (T.stripPrefix "payee:" -> Just s) = (,[]) <$> payeeTag (Just s)
parseQueryTerm _ (T.stripPrefix "note:" -> Just s) = (,[]) <$> noteTag (Just s)
parseQueryTerm _ (T.stripPrefix "acct:" -> Just s) = (,[]) . Acct <$> toRegexCI s
parseQueryTerm d (T.stripPrefix "date2:" -> Just s) =
        case parsePeriodExpr d s of Left e         -> Left $ "\"date2:"++T.unpack s++"\" gave a "++showDateParseError e
                                    Right (_,spn) -> Right (Date2 spn, [])
parseQueryTerm d (T.stripPrefix "date:" -> Just s) =
        case parsePeriodExpr d s of Left e         -> Left $ "\"date:"++T.unpack s++"\" gave a "++showDateParseError e
                                    Right (_,spn) -> Right (Date spn, [])
parseQueryTerm _ (T.stripPrefix "status:" -> Just s) =
        case parseStatus s of Left e   -> Left $ "\"status:"++T.unpack s++"\" gave a parse error: " ++ e
                              Right st -> Right (StatusQ st, [])
parseQueryTerm _ (T.stripPrefix "real:" -> Just s) = Right (Real $ parseBool s || T.null s, [])
parseQueryTerm _ (T.stripPrefix "amt:" -> Just s) = Right (Amt ord q, []) where (ord, q) = either error' id $ parseAmountQueryTerm s  -- PARTIAL:
parseQueryTerm _ (T.stripPrefix "depth:" -> Just s) = (,[]) <$> parseDepthSpecQuery s
parseQueryTerm _ (T.stripPrefix "cur:" -> Just s) = (,[]) . Sym <$> toRegexCI ("^" <> s <> "$") -- support cur: as an alias
parseQueryTerm _ (T.stripPrefix "tag:" -> Just s) = (,[]) <$> parseTag s
parseQueryTerm _ (T.stripPrefix "type:" -> Just s) = (,[]) <$> parseTypeCodes s
parseQueryTerm d (T.stripPrefix "expr:" -> Just s) = parseBooleanQuery d s
parseQueryTerm d (T.stripPrefix "any:" -> Just s) = first (AnyPosting  . (:[])) <$> parseBooleanQuery d s
parseQueryTerm d (T.stripPrefix "all:" -> Just s) = first (AllPostings . (:[])) <$> parseBooleanQuery d s
parseQueryTerm _ "" = Right (Any, [])
parseQueryTerm d s = parseQueryTerm d $ defaultprefix<>":"<>s

-- | Parses a boolean query expression.
--
-- Boolean queries combine smaller queries into larger ones. The boolean operators
-- made available through this function are "NOT e", "e AND e", "e OR e", and "e e".
-- Query options defined in multiple sub-queries are simply combined by concatenating
-- all options into one list.
--
-- Boolean operators in queries take precedence over one another. For instance, the
-- prefix-operator "NOT e" is always parsed before "e AND e", "e AND e" before "e OR e",
-- and "e OR e" before "e e".
--
-- The "space" operator still works as it did before the introduction of boolean operators:
-- it combines terms according to their types, using parseQueryList.
-- Whereas the new NOT, OR, and AND operators work uniformly for all term types.
-- There is an exception: queries being OR'd may not specify a date period,
-- because that can produce multiple, possibly disjoint, report periods and result sets,
-- and we don't have report semantics worked out for it yet. (#2178)
--
-- The result of this function is either an error encountered during parsing of the
-- expression or the combined query and query options.
--
-- >>> parseBooleanQuery nulldate "expenses:dining AND out"
-- Right (And [Acct (RegexpCI "expenses:dining"),Acct (RegexpCI "out")],[])
--
-- >>> parseBooleanQuery nulldate "expenses:dining AND desc:a OR desc:b"
-- Right (Or [And [Acct (RegexpCI "expenses:dining"),Desc (RegexpCI "a")],Desc (RegexpCI "b")],[])
--
parseBooleanQuery :: Day -> T.Text -> Either String (Query,[QueryOpt])
parseBooleanQuery d t =
  either (Left . ("failed to parse query:" <>) . customErrorBundlePretty) Right $
    parsewith spacedExprsP t

  where
    -- Our "boolean queries" are compound query expressions built with a hierarchy of combinators.
    -- At the top level we have one or more query expressions separated by space.
    -- These are combined in the default way according to their types (see combineQueriesByType).
    spacedExprsP :: SimpleTextParser (Query, [QueryOpt])
    spacedExprsP = combineWith combineQueriesByType <$> orExprsP `sepBy` skipNonNewlineSpaces1

      where
        combineWith :: ([Query] -> Query) -> [(Query, [QueryOpt])] -> (Query, [QueryOpt])
        combineWith f res =
          let (qs, qoptss) = unzip res
              qoptss'      = concat qoptss
          in case qs of
              []     -> (Any,                  qoptss')
              (q:[]) -> (simplifyQuery q,      qoptss')
              _      -> (simplifyQuery $ f qs, qoptss')

        -- Containing query expressions separated by "or".
        -- If there's more than one, make sure none contains a "date:".
        orExprsP :: SimpleTextParser (Query, [QueryOpt])
        orExprsP = do
          exprs <- andExprsP `sepBy` (try $ skipNonNewlineSpaces >> string' "or" >> skipNonNewlineSpaces1)
          if ( length exprs > 1
            && (any (/=Any) $ map (filterQuery queryIsDateOrDate2 . fst) exprs))
          then fail "sorry, using date: in OR expressions is not supported."
          else return $ combineWith Or exprs

          where
            -- Containing query expressions separated by "and".
            andExprsP :: SimpleTextParser (Query, [QueryOpt])
            andExprsP = combineWith And <$> maybeNotExprP `sepBy` (try $ skipNonNewlineSpaces >> string' "and" >> skipNonNewlineSpaces1)

              where
                -- Containing query expressions optionally preceded by "not".
                maybeNotExprP :: SimpleTextParser (Query, [QueryOpt])
                maybeNotExprP = (maybe id (\_ (q, qopts) -> (Not q, qopts)) <$>
                  optional (try $ string' "not" >> notFollowedBy (char ':') >> skipNonNewlineSpaces1)) <*> termOrParenthesisedExprP

                  where
                    -- Each of which is a parenthesised query expression or a single query term.
                    termOrParenthesisedExprP :: SimpleTextParser (Query, [QueryOpt])
                    termOrParenthesisedExprP = 
                      between (char '(' >> skipNonNewlineSpaces) (try $ skipNonNewlineSpaces >> char ')') spacedExprsP
                      <|> queryTermP

                      where
                        -- A simple query term: foo, acct:foo, desc:foo, payee:foo etc.
                        queryTermP :: SimpleTextParser (Query, [QueryOpt])
                        queryTermP = do
                          prefix <- optional queryPrefixP
                          arg <- queryArgP
                          case parseQueryTerm d (fromMaybe "" prefix <> arg) of
                            Right q  -> return q
                            Left err -> error' err

                          where
                            -- One of the query prefixes: acct:, desc:, payee: etc (plus zero or more not: prefixes).
                            queryPrefixP :: SimpleTextParser T.Text
                            queryPrefixP =
                              (string "not:" <> (fromMaybe "" <$> optional queryPrefixP))
                              <|> choice' (string <$> queryprefixes)
                              <?> "query prefix"

                            -- A query term's argument, the part after the prefix:
                            -- any text enclosed in single quotes or double quotes,
                            -- or any text up to the next space, closing parenthesis, or end of line,
                            -- if it is not one of the keywords "not", "and", "or".
                            queryArgP :: SimpleTextParser T.Text
                            queryArgP = choice'
                              [ stripquotes . T.pack <$> between (char '\'') (char '\'') (many $ noneOf ("'" :: [Char])),
                                stripquotes . T.pack <$> between (char '"') (char '"') (many $ noneOf ("\"" :: [Char])),
                                T.pack <$> (notFollowedBy keywordP >> (many $ noneOf (") \n\r" :: [Char]))) ]

                              where
                                -- Any of the combinator keywords used above (not/and/or), terminated by a space.
                                keywordP :: SimpleTextParser T.Text
                                keywordP = choice' (string' <$> ["not ", "and ", "or "])

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
    _ -> Left . T.unpack $
         "could not parse as a comparison operator followed by an optionally-signed number: " <> amtarg
  where
    -- Strip outer whitespace from the text, require and remove the
    -- specified prefix, remove all whitespace from the remainder, and
    -- read it as a simple integer or decimal if possible.
    parse :: T.Text -> T.Text -> Maybe Quantity
    parse p s = (T.stripPrefix p . T.strip) s >>= readMay . T.unpack . T.filter (/=' ')

parseTag :: T.Text -> Either RegexError Query
parseTag s = do
    tag <- toRegexCI $ if T.null v then s else n
    body <- if T.null v then pure Nothing else Just <$> toRegexCI (T.tail v)
    return $ Tag tag body
  where (n,v) = T.break (=='=') s

parseDepthSpec :: T.Text -> Either RegexError DepthSpec
parseDepthSpec s = do
    let depthString = T.unpack $ if T.null b then a else T.tail b
    depth <- case readMay depthString of
        Just d | d >= 0 -> Right d
        _ -> Left $ "depth: should be a positive number, but received " ++ depthString
    regexp <- mapM toRegexCI $ if T.null b then Nothing else Just a
    return $ case regexp of
      Nothing -> DepthSpec (Just depth) []
      Just r  -> DepthSpec Nothing [(r, depth)]
  where
    (a,b) = T.break (=='=') s

parseDepthSpecQuery :: T.Text -> Either RegexError Query
parseDepthSpecQuery s = do
    DepthSpec flat rs <- parseDepthSpec s
    let regexps = map (uncurry DepthAcct) rs
    return . And $ maybe id (\d -> (Depth d :)) flat regexps

-- | Parse one or more account type code letters to a query matching any of those types.
parseTypeCodes :: T.Text -> Either String Query
parseTypeCodes s =
  case partitionEithers $ map (parseAccountType False . T.singleton) $ T.unpack s of
    ((e:_),_) -> Left $ "could not parse " <> show e <> " as an account type code.\n" <> help
    ([],[])   -> Left help
    ([],ts)   -> Right $ Type ts
  where
    help = "type:'s argument should be one or more of " ++ accountTypeChoices False ++ " (case insensitive)."

accountTypeChoices :: Bool -> String
accountTypeChoices allowlongform = 
  intercalate ", " 
    -- keep synced with parseAccountType
    $ ["A","L","E","R","X","C","V"]
    ++ if allowlongform then ["Asset","Liability","Equity","Revenue","Expense","Cash","Conversion"] else []

-- | Case-insensitively parse one single-letter code, or one long-form word if permitted, to an account type.
-- On failure, returns the unparseable text.
parseAccountType :: Bool -> Text -> Either String AccountType
parseAccountType allowlongform s =
  case T.toLower s of
    -- keep synced with accountTypeChoices
    "a"                          -> Right Asset
    "l"                          -> Right Liability
    "e"                          -> Right Equity
    "r"                          -> Right Revenue
    "x"                          -> Right Expense
    "c"                          -> Right Cash
    "v"                          -> Right Conversion
    "asset"      | allowlongform -> Right Asset
    "liability"  | allowlongform -> Right Liability
    "equity"     | allowlongform -> Right Equity
    "revenue"    | allowlongform -> Right Revenue
    "expense"    | allowlongform -> Right Expense
    "cash"       | allowlongform -> Right Cash
    "conversion" | allowlongform -> Right Conversion
    _                            -> Left $ T.unpack s

-- | Parse the value part of a "status:" query, or return an error.
parseStatus :: T.Text -> Either String Status
parseStatus s | s `elem` ["*","1"] = Right Cleared
              | s `elem` ["","0"]  = Right Unmarked
              | s == "!"           = Right Pending
              | otherwise          = Left $ "could not parse "++show s++" as a status (should be *, ! or empty)"

-- | Parse the boolean value part of a "status:" query. "1" means true,
-- anything else will be parsed as false without error.
parseBool :: T.Text -> Bool
parseBool s = s `elem` truestrings

truestrings :: [T.Text]
truestrings = ["1"]

-- * modifying

simplifyQuery :: Query -> Query
simplifyQuery q0 =
  let q1 = simplify q0
  in if q1 == q0 then q0 else simplifyQuery q1
  where
    simplify (And []) = Any
    simplify (And [q]) = simplify q
    simplify (And qs) | same qs = simplify $ headErr qs  -- PARTIAL headErr succeeds because pattern ensures non-null qs
                      | None `elem` qs = None
                      | all queryIsDate qs = Date $ spansIntersect $ mapMaybe queryTermDateSpan qs
                      | otherwise = And $ map simplify dateqs ++ map simplify otherqs
                      where (dateqs, otherqs) = partition queryIsDate $ filter (/=Any) qs
    simplify (Or []) = Any
    simplify (Or [q]) = simplifyQuery q
    simplify (Or qs) | same qs = simplify $ headErr qs  -- PARTIAL headErr succeeds because pattern ensures non-null qs
                     | Any `elem` qs = Any
                     -- all queryIsDate qs = Date $ spansUnion $ mapMaybe queryTermDateSpan qs  ?
                     | otherwise = Or $ map simplify $ filter (/=None) qs
    simplify (Date (DateSpan Nothing Nothing)) = Any
    simplify (Date2 (DateSpan Nothing Nothing)) = Any
    simplify q = q

same [] = True
same (a:as) = all (a==) as

-- | Remove query terms (or whole sub-expressions) from this query
-- which do not match the given predicate. XXX Semantics not completely clear.
-- Also calls simplifyQuery on the result.
filterQuery :: (Query -> Bool) -> Query -> Query
filterQuery p = simplifyQuery . filterQuery' p

-- | Like filterQuery, but returns the filtered query as is, without simplifying.
filterQuery' :: (Query -> Bool) -> Query -> Query
filterQuery' p (And qs) = And $ map (filterQuery p) qs
filterQuery' p (Or qs) = Or $ map (filterQuery p) qs
filterQuery' p q = if p q then q else Any

-- | Remove query terms (or whole sub-expressions) from this query
-- which match neither the given predicate nor that predicate negated 
-- (eg, if predicate is queryIsAcct, this will keep both "acct:" and "not:acct:" terms).
-- Also calls simplifyQuery on the result.
-- (Since 1.24.1, might be merged into filterQuery in future.)
-- XXX Semantics not completely clear.
filterQueryOrNotQuery :: (Query -> Bool) -> Query -> Query
filterQueryOrNotQuery p0 = simplifyQuery . filterQueryOrNotQuery' p0
  where
    filterQueryOrNotQuery' :: (Query -> Bool) -> Query -> Query
    filterQueryOrNotQuery' p (And qs)      = And $ map (filterQueryOrNotQuery p) qs
    filterQueryOrNotQuery' p (Or qs)       = Or  $ map (filterQueryOrNotQuery p) qs
    filterQueryOrNotQuery' p (Not q) | p q = Not $ filterQueryOrNotQuery p q
    filterQueryOrNotQuery' p q = if p q then q else Any

-- * predicates

-- | Does this simple query predicate match any part of this possibly compound query ?
matchesQuery :: (Query -> Bool) -> Query -> Bool
matchesQuery p (And qs) = any (matchesQuery p) qs
matchesQuery p (Or qs)  = any (matchesQuery p) qs
matchesQuery p (Not q)  = p q
matchesQuery p q        = p q

-- | Does this query match everything ?
queryIsNull :: Query -> Bool
queryIsNull Any = True
queryIsNull (And []) = True
queryIsNull (Not (Or [])) = True
queryIsNull _ = False

-- | Is this a simple query of this type (date:) ? 
-- Does not match a compound query involving and/or/not.
-- Likewise for the following functions.
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

queryIsStatus :: Query -> Bool
queryIsStatus (StatusQ _) = True
queryIsStatus _ = False

queryIsCode :: Query -> Bool
queryIsCode (Code _) = True
queryIsCode _ = False

queryIsDesc :: Query -> Bool
queryIsDesc (Desc _) = True
queryIsDesc _ = False

queryIsTag :: Query -> Bool
queryIsTag (Tag _ _) = True
queryIsTag _ = False

queryIsAcct :: Query -> Bool
queryIsAcct (Acct _) = True
queryIsAcct _ = False

queryIsType :: Query -> Bool
queryIsType (Type _) = True
queryIsType _ = False

queryIsDepth :: Query -> Bool
queryIsDepth (Depth _) = True
queryIsDepth (DepthAcct _ _) = True
queryIsDepth _ = False

queryIsReal :: Query -> Bool
queryIsReal (Real _) = True
queryIsReal _ = False

queryIsAmt :: Query -> Bool
queryIsAmt (Amt _ _) = True
queryIsAmt _         = False

queryIsSym :: Query -> Bool
queryIsSym (Sym _) = True
queryIsSym _ = False

-- | Does this query specify a start date and nothing else (that would
-- filter postings prior to the date) ?
-- When the flag is true, look for a starting secondary date instead.
queryIsStartDateOnly :: Bool -> Query -> Bool
queryIsStartDateOnly _ Any = False
queryIsStartDateOnly _ None = False
queryIsStartDateOnly secondary (Or ms) = all (queryIsStartDateOnly secondary) ms
queryIsStartDateOnly secondary (And ms) = all (queryIsStartDateOnly secondary) ms
queryIsStartDateOnly False (Date (DateSpan (Just _) _)) = True
queryIsStartDateOnly True (Date2 (DateSpan (Just _) _)) = True
queryIsStartDateOnly _ _ = False

-- | Does this query involve a property of transactions (or their postings),
-- making it inapplicable to account declarations ?
queryIsTransactionRelated :: Query -> Bool
queryIsTransactionRelated = matchesQuery (
      queryIsDate
  ||| queryIsDate2
  ||| queryIsStatus
  ||| queryIsCode
  ||| queryIsDesc
  ||| queryIsReal
  ||| queryIsAmt
  ||| queryIsSym
  )

(|||) :: (a->Bool) -> (a->Bool) -> (a->Bool)
p ||| q = \v -> p v || q v

-- * accessors

-- | What start date (or secondary date) does this query specify, if any ?
-- For OR expressions, use the earliest of the dates. NOT is ignored.
queryStartDate :: Bool -> Query -> Maybe Day
queryStartDate secondary (Or ms) = earliestMaybeDate $ map (queryStartDate secondary) ms
queryStartDate secondary (And ms) = latestMaybeDate $ map (queryStartDate secondary) ms
queryStartDate False (Date (DateSpan (Just d) _)) = Just $ fromEFDay d
queryStartDate True (Date2 (DateSpan (Just d) _)) = Just $ fromEFDay d
queryStartDate _ _ = Nothing

-- | What end date (or secondary date) does this query specify, if any ?
-- For OR expressions, use the latest of the dates. NOT is ignored.
queryEndDate :: Bool -> Query -> Maybe Day
queryEndDate secondary (Or ms) = latestMaybeDate' $ map (queryEndDate secondary) ms
queryEndDate secondary (And ms) = earliestMaybeDate' $ map (queryEndDate secondary) ms
queryEndDate False (Date (DateSpan _ (Just d))) = Just $ fromEFDay d
queryEndDate True (Date2 (DateSpan _ (Just d))) = Just $ fromEFDay d
queryEndDate _ _ = Nothing

queryTermDateSpan (Date spn) = Just spn
queryTermDateSpan _ = Nothing

-- | What date span (or with a true argument, what secondary date span) does this query specify ?
-- OR clauses specifying multiple spans return their union (the span enclosing all of them).
-- AND clauses specifying multiple spans return their intersection.
-- NOT clauses are ignored.
queryDateSpan :: Bool -> Query -> DateSpan
queryDateSpan secondary (Or qs)  = spansUnion     $ map (queryDateSpan secondary) qs
queryDateSpan secondary (And qs) = spansIntersect $ map (queryDateSpan secondary) qs
queryDateSpan _     (Date spn)  = spn
queryDateSpan True (Date2 spn)  = spn
queryDateSpan _ _                = nulldatespan

-- | What date span does this query specify, treating primary and secondary dates as equivalent ?
-- OR clauses specifying multiple spans return their union (the span enclosing all of them).
-- AND clauses specifying multiple spans return their intersection.
-- NOT clauses are ignored.
queryDateSpan' :: Query -> DateSpan
queryDateSpan' (Or qs)      = spansUnion     $ map queryDateSpan' qs
queryDateSpan' (And qs)     = spansIntersect $ map queryDateSpan' qs
queryDateSpan' (Date spn)  = spn
queryDateSpan' (Date2 spn) = spn
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
queryDepth :: Query -> DepthSpec
queryDepth (Or qs)         = foldMap queryDepth qs
queryDepth (And qs)        = foldMap queryDepth qs
queryDepth (Depth d)       = DepthSpec (Just d) []
queryDepth (DepthAcct r d) = DepthSpec Nothing [(r,d)]
queryDepth _               = mempty

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
inAccountQuery (QueryOptInAcctOnly a : _) = Just . Acct $ accountNameToAccountOnlyRegex a
inAccountQuery (QueryOptInAcct a     : _) = Just . Acct $ accountNameToAccountRegex a

-- -- | Convert a query to its inverse.
-- negateQuery :: Query -> Query
-- negateQuery =  Not

-- matching things with queries

matchesCommodity :: Query -> CommoditySymbol -> Bool
matchesCommodity (Sym r) = regexMatchText r
matchesCommodity _ = const True

-- | Does the match expression match this (simple) amount ?
matchesAmount :: Query -> Amount -> Bool
matchesAmount (Not q) a = not $ q `matchesAmount` a
matchesAmount (Any) _ = True
matchesAmount (None) _ = False
matchesAmount (Or qs) a = any (`matchesAmount` a) qs
matchesAmount (And qs) a = all (`matchesAmount` a) qs
matchesAmount (AnyPosting  qs) a = all (`matchesAmount` a) qs
matchesAmount (AllPostings qs) a = all (`matchesAmount` a) qs
matchesAmount (Amt ord n) a = compareAmount ord n a
matchesAmount (Sym r) a = matchesCommodity (Sym r) (acommodity a)
matchesAmount _ _ = True

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

matchesMixedAmount :: Query -> MixedAmount -> Bool
matchesMixedAmount q ma = case amountsRaw ma of
    [] -> q `matchesAmount` nullamt
    as -> any (q `matchesAmount`) as

-- | Does the query match this account name ?
-- A matching in: clause is also considered a match.
matchesAccount :: Query -> AccountName -> Bool
matchesAccount (None) _ = False
matchesAccount (Not m) a = not $ matchesAccount m a
matchesAccount (Or ms) a = any (`matchesAccount` a) ms
matchesAccount (And ms) a = all (`matchesAccount` a) ms
matchesAccount (AnyPosting  qs) a = all (`matchesAccount` a) qs
matchesAccount (AllPostings qs) a = all (`matchesAccount` a) qs
matchesAccount (Acct r) a = regexMatchText r a
matchesAccount (Depth d) a = accountNameLevel a <= d
matchesAccount (DepthAcct r d) a = accountNameLevel a <= d || not (regexMatchText r a)
matchesAccount (Tag _ _) _ = False
matchesAccount _ _ = True

-- | Like matchesAccount, but with optional extra matching features:
--
-- - If the account's type is provided, any type: terms in the query
--   must match it (and any negated type: terms must not match it).
--
-- - If the account's tags are provided, any tag: terms must match
--   at least one of them (and any negated tag: terms must match none).
--
matchesAccountExtra :: (AccountName -> Maybe AccountType) -> (AccountName -> [Tag]) -> Query -> AccountName -> Bool
matchesAccountExtra atypes atags (Not q  ) a = not $ matchesAccountExtra atypes atags q a
matchesAccountExtra atypes atags (Or  qs ) a = any (\q -> matchesAccountExtra atypes atags q a) qs
matchesAccountExtra atypes atags (And qs ) a = all (\q -> matchesAccountExtra atypes atags q a) qs
matchesAccountExtra atypes atags (AnyPosting  qs ) a = all (\q -> matchesAccountExtra atypes atags q a) qs
matchesAccountExtra atypes atags (AllPostings qs ) a = all (\q -> matchesAccountExtra atypes atags q a) qs
matchesAccountExtra atypes _     (Type ts) a = maybe False (\t -> any (t `isAccountSubtypeOf`) ts) $ atypes a
matchesAccountExtra _      atags (Tag npat vpat) a = matchesTags npat vpat $ atags a
matchesAccountExtra _      _     q         a = matchesAccount q a

-- | Does the match expression match this posting ?
-- When matching account name, and the posting has been transformed
-- in some way, we will match either the original or transformed name.
matchesPosting :: Query -> Posting -> Bool
matchesPosting (Not q) p = not $ q `matchesPosting` p
matchesPosting (Any) _ = True
matchesPosting (None) _ = False
matchesPosting (Or qs) p = any (`matchesPosting` p) qs
matchesPosting (And qs) p = all (`matchesPosting` p) qs
matchesPosting (AnyPosting  qs) p = all (`matchesPosting` p) qs
matchesPosting (AllPostings qs) p = all (`matchesPosting` p) qs
matchesPosting (Code r) p = maybe False (regexMatchText r . tcode) $ ptransaction p
matchesPosting (Desc r) p = maybe False (regexMatchText r . tdescription) $ ptransaction p
matchesPosting (Acct r) p = matches p || maybe False matches (poriginal p) where matches = regexMatchText r . paccount
matchesPosting (Date spn) p = spn `spanContainsDate` postingDate p
matchesPosting (Date2 spn) p = spn `spanContainsDate` postingDate2 p
matchesPosting (StatusQ s) p = postingStatus p == s
matchesPosting (Real v) p = v == isReal p
matchesPosting q@(Depth _) Posting{paccount=a} = q `matchesAccount` a
matchesPosting q@(DepthAcct _ _) Posting{paccount=a} = q `matchesAccount` a
matchesPosting q@(Amt _ _) Posting{pamount=as} = q `matchesMixedAmount` as
matchesPosting (Sym r) Posting{pamount=as} = any (matchesCommodity (Sym r) . acommodity) $ amountsRaw as
matchesPosting (Tag n v) p = case (reString n, v) of
  ("payee", Just v') -> maybe False (regexMatchText v' . transactionPayee) $ ptransaction p
  ("note", Just v') -> maybe False (regexMatchText v' . transactionNote) $ ptransaction p
  (_, mv) -> matchesTags n mv $ postingAllTags p
matchesPosting (Type _) _ = False

-- | Like matchesPosting, but if the posting's account's type is provided,
-- any type: terms in the query must match it (and any negated type: terms
-- must not match it).
matchesPostingExtra :: (AccountName -> Maybe AccountType) -> Query -> Posting -> Bool
matchesPostingExtra atype (Not q )  p = not $ matchesPostingExtra atype q p
matchesPostingExtra atype (Or  qs)  p = any (\q -> matchesPostingExtra atype q p) qs
matchesPostingExtra atype (And qs)  p = all (\q -> matchesPostingExtra atype q p) qs
matchesPostingExtra atype (AnyPosting  qs)  p = all (\q -> matchesPostingExtra atype q p) qs
matchesPostingExtra atype (AllPostings qs)  p = all (\q -> matchesPostingExtra atype q p) qs
matchesPostingExtra atype (Type ts) p =
  -- does posting's account's type, if we can detect it, match any of the given types ?
  (maybe False (\t -> any (t `isAccountSubtypeOf`) ts) . atype $ paccount p)
  -- or, try the same test with the original (pre-aliasing/pivoting) posting's account
  || (fromMaybe False $ do
      porig <- poriginal p
      let a = paccount porig
      t <- atype a
      Just $ any (t `isAccountSubtypeOf`) ts
  )
matchesPostingExtra _ q p             = matchesPosting q p

-- | Does the match expression match this transaction ?
matchesTransaction :: Query -> Transaction -> Bool
matchesTransaction (Not q) t = not $ q `matchesTransaction` t
matchesTransaction (Any) _ = True
matchesTransaction (None) _ = False
matchesTransaction (Or qs) t = any (`matchesTransaction` t) qs
matchesTransaction (And qs) t = all (`matchesTransaction` t) qs
matchesTransaction (AnyPosting  qs) t = any (\p -> all (`matchesPosting` p) qs) $ tpostings t
matchesTransaction (AllPostings qs) t = all (\p -> all (`matchesPosting` p) qs) $ tpostings t
matchesTransaction (Code r) t = regexMatchText r $ tcode t
matchesTransaction (Desc r) t = regexMatchText r $ tdescription t
matchesTransaction q@(Acct _) t = any (q `matchesPosting`) $ tpostings t
matchesTransaction (Date spn) t = spanContainsDate spn $ tdate t
matchesTransaction (Date2 spn) t = spanContainsDate spn $ transactionDate2 t
matchesTransaction (StatusQ s) t = tstatus t == s
matchesTransaction (Real v) t = v == hasRealPostings t
matchesTransaction q@(Amt _ _) t = any (q `matchesPosting`) $ tpostings t
matchesTransaction q@(Depth _) t = any (q `matchesPosting`) $ tpostings t
matchesTransaction q@(DepthAcct _ _) t = any (q `matchesPosting`) $ tpostings t
matchesTransaction q@(Sym _) t = any (q `matchesPosting`) $ tpostings t
matchesTransaction (Tag n v) t = case (reString n, v) of
  ("payee", Just v') -> regexMatchText v' $ transactionPayee t
  ("note", Just v') -> regexMatchText v' $ transactionNote t
  (_, v') -> matchesTags n v' $ transactionAllTags t
matchesTransaction (Type _) _ = False

-- | Like matchesTransaction, but if the journal's account types are provided,
-- any type: terms in the query must match at least one posting's account type
-- (and any negated type: terms must match none).
matchesTransactionExtra :: (AccountName -> Maybe AccountType) -> Query -> Transaction -> Bool
matchesTransactionExtra atype (Not  q) t = not $ matchesTransactionExtra atype q t
matchesTransactionExtra atype (Or  qs) t = any (\q -> matchesTransactionExtra atype q t) qs
matchesTransactionExtra atype (And qs) t = all (\q -> matchesTransactionExtra atype q t) qs
matchesTransactionExtra atype (AnyPosting  qs) t = any (\p -> all (\q -> matchesPostingExtra atype q p) qs) $ tpostings t
matchesTransactionExtra atype (AllPostings qs) t = all (\p -> all (\q -> matchesPostingExtra atype q p) qs) $ tpostings t
matchesTransactionExtra atype q@(Type _) t = any (matchesPostingExtra atype q) $ tpostings t
matchesTransactionExtra _ q t = matchesTransaction q t

-- | Does the query match this transaction description ?
-- Tests desc: terms, any other terms are ignored.
matchesDescription :: Query -> Text -> Bool
matchesDescription (Not q) d          = not $ q `matchesDescription` d
matchesDescription (Any) _            = True
matchesDescription (None) _           = False
matchesDescription (Or qs) d          = any (`matchesDescription` d) $ filter queryIsDesc qs
matchesDescription (And qs) d         = all (`matchesDescription` d) $ filter queryIsDesc qs
matchesDescription (AnyPosting  qs) d = all (`matchesDescription` d) $ filter queryIsDesc qs
matchesDescription (AllPostings qs) d = all (`matchesDescription` d) $ filter queryIsDesc qs
matchesDescription (Code _) _         = False
matchesDescription (Desc r) d         = regexMatchText r d
matchesDescription _ _                = False

-- | Does the query match this transaction payee ?
-- Tests desc: (and payee: ?) terms, any other terms are ignored.
-- XXX Currently an alias for matchDescription. I'm not sure if more is needed,
-- There's some shenanigan with payee: and "payeeTag" to figure out.
matchesPayeeWIP :: Query -> Payee -> Bool
matchesPayeeWIP = matchesDescription

-- | Does the query match the name and optionally the value of any of these tags ?
matchesTags :: Regexp -> Maybe Regexp -> [Tag] -> Bool
matchesTags namepat valuepat = any (matches namepat valuepat)
  where
    matches npat vpat (n,v) = regexMatchText npat n && maybe (const True) regexMatchText vpat v

-- | Does the query match this market price ?
matchesPriceDirective :: Query -> PriceDirective -> Bool
matchesPriceDirective (None) _           = False
matchesPriceDirective (Not q) p          = not $ matchesPriceDirective q p
matchesPriceDirective (Or qs) p          = any (`matchesPriceDirective` p) qs
matchesPriceDirective (And qs) p         = all (`matchesPriceDirective` p) qs
matchesPriceDirective (AnyPosting  qs) p = all (`matchesPriceDirective` p) qs
matchesPriceDirective (AllPostings qs) p = all (`matchesPriceDirective` p) qs
matchesPriceDirective q@(Amt _ _) p      = matchesAmount q (pdamount p)
matchesPriceDirective q@(Sym _) p        = matchesCommodity q (pdcommodity p)
matchesPriceDirective (Date spn) p       = spanContainsDate spn (pddate p)
matchesPriceDirective _ _                = True


-- tests

tests_Query = testGroup "Query" [
   testCase "simplifyQuery" $ do
     (simplifyQuery $ Or [Acct $ toRegex' "a"])      @?= (Acct $ toRegex' "a")
     (simplifyQuery $ Or [Any,None])      @?= (Any)
     (simplifyQuery $ And [Any,None])     @?= (None)
     (simplifyQuery $ And [Any,Any])      @?= (Any)
     (simplifyQuery $ And [Acct $ toRegex' "b",Any]) @?= (Acct $ toRegex' "b")
     (simplifyQuery $ And [Any,And [Date (DateSpan Nothing Nothing)]]) @?= (Any)
     (simplifyQuery $ And [Date (DateSpan Nothing (Just $ Exact $ fromGregorian 2013 01 01)), Date (DateSpan (Just $ Exact $ fromGregorian 2012 01 01) Nothing)])
       @?= (Date (DateSpan (Just $ Exact $ fromGregorian 2012 01 01) (Just $ Exact $ fromGregorian 2013 01 01)))
     (simplifyQuery $ And [Or [],Or [Desc $ toRegex' "b b"]]) @?= (Desc $ toRegex' "b b")

  ,testCase "parseQuery" $ do
     (parseQuery nulldate "acct:'expenses:autres d\233penses' desc:b") @?= Right (And [Acct $ toRegexCI' "expenses:autres d\233penses", Desc $ toRegexCI' "b"], [])
     parseQuery nulldate "inacct:a desc:\"b b\""                       @?= Right (Desc $ toRegexCI' "b b", [QueryOptInAcct "a"])
     parseQuery nulldate "inacct:a inacct:b"                           @?= Right (Any, [QueryOptInAcct "a", QueryOptInAcct "b"])
     parseQuery nulldate "desc:'x x'"                                  @?= Right (Desc $ toRegexCI' "x x", [])
     parseQuery nulldate "'a a' 'b"                                    @?= Right (Or [Acct $ toRegexCI' "a a",Acct $ toRegexCI' "'b"], [])
     parseQuery nulldate "\""                                          @?= Right (Acct $ toRegexCI' "\"", [])

  ,testCase "parseBooleanQuery" $ do
     parseBooleanQuery nulldate "(tag:'atag=a')"     @?= Right (Tag (toRegexCI' "atag") (Just $ toRegexCI' "a"), [])
     parseBooleanQuery nulldate "(  tag:\"atag=a\"  )"     @?= Right (Tag (toRegexCI' "atag") (Just $ toRegexCI' "a"), [])
     parseBooleanQuery nulldate "(acct:'expenses:food')"     @?= Right (Acct $ toRegexCI' "expenses:food", [])
     parseBooleanQuery nulldate "(((acct:'expenses:food')))" @?= Right (Acct $ toRegexCI' "expenses:food", [])
     parseBooleanQuery nulldate "acct:'expenses:food' AND desc:'b'" @?= Right (And [Acct $ toRegexCI' "expenses:food", Desc $ toRegexCI' "b"], [])
     parseBooleanQuery nulldate "((desc:'a') AND (desc:'b') OR (desc:'c'))" @?= Right (Or [And [Desc $ toRegexCI' "a", Desc $ toRegexCI' "b"], Desc $ toRegexCI' "c"], [])
     parseBooleanQuery nulldate "((desc:'a') OR (desc:'b') AND (desc:'c'))" @?= Right (Or [Desc $ toRegexCI' "a", And [Desc $ toRegexCI' "b", Desc $ toRegexCI' "c"]], [])
     parseBooleanQuery nulldate "((desc:'a') AND desc:'b' AND (desc:'c'))" @?= Right (And [Desc $ toRegexCI' "a", Desc $ toRegexCI' "b", Desc $ toRegexCI' "c"], [])
     parseBooleanQuery nulldate "(NOT (desc:'a') AND (desc:'b'))" @?= Right (And [Not $ Desc $ toRegexCI' "a", Desc $ toRegexCI' "b"], [])
     parseBooleanQuery nulldate "((desc:'a') AND (NOT desc:'b'))" @?= Right (And [Desc $ toRegexCI' "a", Not $ Desc $ toRegexCI' "b"], [])
     parseBooleanQuery nulldate "(desc:'a' AND desc:'b')" @?= Right (And [Desc $ toRegexCI' "a", Desc $ toRegexCI' "b"], [])
     parseBooleanQuery nulldate "(acct:'a' acct:'b')" @?= Right (Or [Acct $ toRegexCI' "a", Acct $ toRegexCI' "b"], [])
     parseBooleanQuery nulldate " acct:'a' acct:'b'" @?= Right (Or [Acct $ toRegexCI' "a", Acct $ toRegexCI' "b"], [])
     parseBooleanQuery nulldate "not:a" @?= Right (Not $ Acct $ toRegexCI' "a", [])
     parseBooleanQuery nulldate "expenses:food OR (tag:A expenses:drink)" @?= Right (Or [Acct $ toRegexCI' "expenses:food", And [Acct $ toRegexCI' "expenses:drink", Tag (toRegexCI' "A") Nothing]], [])
     parseBooleanQuery nulldate "not a" @?= Right (Not $ Acct $ toRegexCI' "a", [])
     parseBooleanQuery nulldate "nota" @?= Right (Acct $ toRegexCI' "nota", [])
     parseBooleanQuery nulldate "not (acct:a)" @?= Right (Not $ Acct $ toRegexCI' "a", [])

  ,testCase "words''" $ do
      (words'' [] "a b")                   @?= ["a","b"]
      (words'' [] "'a b'")                 @?= ["a b"]
      (words'' [] "not:a b")               @?= ["not:a","b"]
      (words'' [] "not:'a b'")             @?= ["not:a b"]
      (words'' [] "'not:a b'")             @?= ["not:a b"]
      (words'' ["desc:"] "not:desc:'a b'") @?= ["not:desc:a b"]
      (words'' queryprefixes "\"acct:expenses:autres d\233penses\"") @?= ["acct:expenses:autres d\233penses"]
      (words'' queryprefixes "\"")              @?= ["\""]

  ,testCase "filterQuery" $ do
     filterQuery queryIsDepth Any       @?= Any
     filterQuery queryIsDepth (Depth 1) @?= Depth 1
     filterQuery (not.queryIsDepth) (And [And [StatusQ Cleared,Depth 1]]) @?= StatusQ Cleared
     filterQuery queryIsDepth (And [Date nulldatespan, Not (Or [Any, Depth 1])]) @?= Any   -- XXX unclear

  ,testCase "parseQueryTerm" $ do
     parseQueryTerm nulldate "a"                                @?= Right (Acct $ toRegexCI' "a", [])
     parseQueryTerm nulldate "acct:expenses:autres d\233penses" @?= Right (Acct $ toRegexCI' "expenses:autres d\233penses", [])
     parseQueryTerm nulldate "not:desc:a b"                     @?= Right (Not $ Desc $ toRegexCI' "a b", [])
     parseQueryTerm nulldate "status:1"                         @?= Right (StatusQ Cleared, [])
     parseQueryTerm nulldate "status:*"                         @?= Right (StatusQ Cleared, [])
     parseQueryTerm nulldate "status:!"                         @?= Right (StatusQ Pending, [])
     parseQueryTerm nulldate "status:0"                         @?= Right (StatusQ Unmarked, [])
     parseQueryTerm nulldate "status:"                          @?= Right (StatusQ Unmarked, [])
     parseQueryTerm nulldate "payee:x"                          @?= (,[]) <$> payeeTag (Just "x")
     parseQueryTerm nulldate "note:x"                           @?= (,[]) <$> noteTag (Just "x")
     parseQueryTerm nulldate "real:1"                           @?= Right (Real True, [])
     parseQueryTerm nulldate "date:2008"                        @?= Right (Date $ DateSpan (Just $ Flex $ fromGregorian 2008 01 01) (Just $ Flex $ fromGregorian 2009 01 01), [])
     parseQueryTerm nulldate "date:from 2012/5/17"              @?= Right (Date $ DateSpan (Just $ Exact $ fromGregorian 2012 05 17) Nothing, [])
     parseQueryTerm nulldate "date:20180101-201804"             @?= Right (Date $ DateSpan (Just $ Exact $ fromGregorian 2018 01 01) (Just $ Flex $ fromGregorian 2018 04 01), [])
     parseQueryTerm nulldate "inacct:a"                         @?= Right (Any, [QueryOptInAcct "a"])
     parseQueryTerm nulldate "tag:a"                            @?= Right (Tag (toRegexCI' "a") Nothing, [])
     parseQueryTerm nulldate "tag:a=some value"                 @?= Right (Tag (toRegexCI' "a") (Just $ toRegexCI' "some value"), [])
     parseQueryTerm nulldate "amt:<0"                           @?= Right (Amt Lt 0, [])
     parseQueryTerm nulldate "amt:>10000.10"                    @?= Right (Amt AbsGt 10000.1, [])

  ,testCase "parseAmountQueryTerm" $ do
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

  ,testCase "queryStartDate" $ do
     let small = Just $ fromGregorian 2000 01 01
         big   = Just $ fromGregorian 2000 01 02
     queryStartDate False (And [Date $ DateSpan (Exact <$> small) Nothing, Date $ DateSpan (Exact <$> big) Nothing]) @?= big
     queryStartDate False (And [Date $ DateSpan (Exact <$> small) Nothing, Date $ DateSpan Nothing Nothing])         @?= small
     queryStartDate False (Or  [Date $ DateSpan (Exact <$> small) Nothing, Date $ DateSpan (Exact <$> big) Nothing]) @?= small
     queryStartDate False (Or  [Date $ DateSpan (Exact <$> small) Nothing, Date $ DateSpan Nothing Nothing])         @?= Nothing

  ,testCase "queryEndDate" $ do
     let small = Just $ fromGregorian 2000 01 01
         big   = Just $ fromGregorian 2000 01 02
     queryEndDate False (And [Date $ DateSpan Nothing (Exact <$> small), Date $ DateSpan Nothing (Exact <$> big)]) @?= small
     queryEndDate False (And [Date $ DateSpan Nothing (Exact <$> small), Date $ DateSpan Nothing Nothing])         @?= small
     queryEndDate False (Or  [Date $ DateSpan Nothing (Exact <$> small), Date $ DateSpan Nothing (Exact <$> big)]) @?= big
     queryEndDate False (Or  [Date $ DateSpan Nothing (Exact <$> small), Date $ DateSpan Nothing Nothing])         @?= Nothing

  ,testCase "matchesAccount" $ do
     assertBool "" $ (Acct $ toRegex' "b:c") `matchesAccount` "a:bb:c:d"
     assertBool "" $ not $ (Acct $ toRegex' "^a:b") `matchesAccount` "c:a:b"
     assertBool "" $ Depth 2 `matchesAccount` "a"
     assertBool "" $ Depth 2 `matchesAccount` "a:b"
     assertBool "" $ not $ Depth 2 `matchesAccount` "a:b:c"
     assertBool "" $ Date nulldatespan `matchesAccount` "a"
     assertBool "" $ Date2 nulldatespan `matchesAccount` "a"
     assertBool "" $ not $ Tag (toRegex' "a") Nothing `matchesAccount` "a"

  ,testCase "matchesAccountExtra" $ do
     let tagq = Tag (toRegexCI' "type") Nothing
     assertBool "" $ not $ matchesAccountExtra (const Nothing) (const []) tagq "a"
     assertBool "" $       matchesAccountExtra (const Nothing) (const [("type","")]) tagq "a"

  ,testGroup "matchesPosting" [
     testCase "positive match on cleared posting status"  $
      assertBool "" $ (StatusQ Cleared)  `matchesPosting` nullposting{pstatus=Cleared}
    ,testCase "negative match on cleared posting status"  $
      assertBool "" $ not $ (Not $ StatusQ Cleared)  `matchesPosting` nullposting{pstatus=Cleared}
    ,testCase "positive match on unmarked posting status" $
      assertBool "" $ (StatusQ Unmarked) `matchesPosting` nullposting{pstatus=Unmarked}
    ,testCase "negative match on unmarked posting status" $
      assertBool "" $ not $ (Not $ StatusQ Unmarked) `matchesPosting` nullposting{pstatus=Unmarked}
    ,testCase "positive match on true posting status acquired from transaction" $
      assertBool "" $ (StatusQ Cleared) `matchesPosting` nullposting{pstatus=Unmarked,ptransaction=Just nulltransaction{tstatus=Cleared}}
    ,testCase "real:1 on real posting" $ assertBool "" $ (Real True) `matchesPosting` nullposting{ptype=RegularPosting}
    ,testCase "real:1 on virtual posting fails" $ assertBool "" $ not $ (Real True) `matchesPosting` nullposting{ptype=VirtualPosting}
    ,testCase "real:1 on balanced virtual posting fails" $ assertBool "" $ not $ (Real True) `matchesPosting` nullposting{ptype=BalancedVirtualPosting}
    ,testCase "acct:" $ assertBool "" $ (Acct $ toRegex' "'b") `matchesPosting` nullposting{paccount="'b"}
    ,testCase "tag:" $ do
      assertBool "" $ not $ (Tag (toRegex' "a") (Just $ toRegex' "r$")) `matchesPosting` nullposting
      assertBool "" $ (Tag (toRegex' "foo") Nothing) `matchesPosting` nullposting{ptags=[("foo","")]}
      assertBool "" $ (Tag (toRegex' "foo") Nothing) `matchesPosting` nullposting{ptags=[("foo","baz")]}
      assertBool "" $ (Tag (toRegex' "foo") (Just $ toRegex' "a")) `matchesPosting` nullposting{ptags=[("foo","bar")]}
      assertBool "" $ not $ (Tag (toRegex' "foo") (Just $ toRegex' "a$")) `matchesPosting` nullposting{ptags=[("foo","bar")]}
      assertBool "" $ not $ (Tag (toRegex' " foo ") (Just $ toRegex' "a")) `matchesPosting` nullposting{ptags=[("foo","bar")]}
      assertBool "" $ not $ (Tag (toRegex' "foo foo") (Just $ toRegex' " ar ba ")) `matchesPosting` nullposting{ptags=[("foo foo","bar bar")]}
    ,testCase "a tag match on a posting also sees inherited tags" $ assertBool "" $ (Tag (toRegex' "txntag") Nothing) `matchesPosting` nullposting{ptransaction=Just nulltransaction{ttags=[("txntag","")]}}
    ,testCase "cur:" $ do
      let toSym = fst . either error' id . parseQueryTerm (fromGregorian 2000 01 01) . ("cur:"<>)
      assertBool "" $ not $ toSym "$" `matchesPosting` nullposting{pamount=mixedAmount $ usd 1} -- becomes "^$$", ie testing for null symbol
      assertBool "" $ (toSym "\\$") `matchesPosting` nullposting{pamount=mixedAmount $ usd 1} -- have to quote $ for regexpr
      assertBool "" $ (toSym "shekels") `matchesPosting` nullposting{pamount=mixedAmount nullamt{acommodity="shekels"}}
      assertBool "" $ not $ (toSym "shek") `matchesPosting` nullposting{pamount=mixedAmount nullamt{acommodity="shekels"}}
  ]

  ,testCase "matchesTransaction" $ do
     assertBool "" $ Any `matchesTransaction` nulltransaction
     assertBool "" $ not $ (Desc $ toRegex' "x x") `matchesTransaction` nulltransaction{tdescription="x"}
     assertBool "" $ (Desc $ toRegex' "x x") `matchesTransaction` nulltransaction{tdescription="x x"}
     -- see posting for more tag tests
     assertBool "" $ (Tag (toRegex' "foo") (Just $ toRegex' "a")) `matchesTransaction` nulltransaction{ttags=[("foo","bar")]}
     assertBool "" $ (Tag (toRegex' "payee") (Just $ toRegex' "payee")) `matchesTransaction` nulltransaction{tdescription="payee|note"}
     assertBool "" $ (Tag (toRegex' "note") (Just $ toRegex' "note")) `matchesTransaction` nulltransaction{tdescription="payee|note"}
     -- a tag match on a transaction also matches posting tags
     assertBool "" $ (Tag (toRegex' "postingtag") Nothing) `matchesTransaction` nulltransaction{tpostings=[nullposting{ptags=[("postingtag","")]}]}

     -- hledger print expr:'cash and amt:>0' means "show transactions with (at least one posting involving a cash account) and (at least one posting with a positive amount)"
     let exprq = And [Acct $ toRegex' "cash", Amt Gt 0]
     assertBool "" $ exprq `matchesTransaction` nulltransaction{tpostings=[nullposting{paccount="cash", pamount=1}]}
     assertBool "" $ exprq `matchesTransaction` nulltransaction{tpostings = [nullposting{paccount="cash"}, nullposting{paccount="food", pamount=1}]}

     -- hledger print any:'cash and amt:>0' means "show transactions where at least one posting posts a positive amount to a cash account".
     let anyq = AnyPosting [Acct $ toRegex' "cash", Amt Gt 0]
     assertBool "" $ anyq `matchesTransaction` nulltransaction{tpostings=[nullposting{paccount="cash", pamount=1}]}
     assertBool "" $ not $ anyq `matchesTransaction` nulltransaction{tpostings = [nullposting{paccount="cash"}, nullposting{paccount="food", pamount=1}]}

     -- hledger print all:'cash and amt:0' means "show transactions where all postings involve a cash account and have a zero amount".
     assertBool "" $ AllPostings [Amt Eq 0] `matchesTransaction`
      nulltransaction{tpostings = [nullposting{paccount = "cash"}, nullposting{paccount = "food"}]}
     assertBool "" $ not $ AllPostings [Acct $ toRegex' "cash", Amt Eq 0] `matchesTransaction`
      nulltransaction{tpostings = [nullposting{paccount = "cash"}, nullposting{paccount = "food"}]}
 ]
