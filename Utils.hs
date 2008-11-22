{-|

Utilities for top-level modules and/or ghci. See also "Ledger.Utils".

-}

module Utils
where
import qualified Data.Map as Map (lookup)
import Options
import Ledger


-- | get a RawLedger from the given file path
rawledgerfromfile :: FilePath -> IO RawLedger
rawledgerfromfile f = do
  parsed <- parseLedgerFile f
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | get a cached Ledger from the given file path
ledgerfromfile :: FilePath -> IO Ledger
ledgerfromfile f = do
  l  <- rawledgerfromfile f
  return $ cacheLedger $ filterRawLedger "" "" [] False False l

-- | get a RawLedger from the file your LEDGER environment variable
-- variable points to or (WARNING) an empty one if there was a problem.
myrawledger :: IO RawLedger
myrawledger = do
  parsed <- ledgerFilePathFromOpts [] >>= parseLedgerFile
  return $ either (\_ -> RawLedger [] [] [] "") id parsed

-- | get a cached Ledger from the file your LEDGER environment variable
-- variable points to or (WARNING) an empty one if there was a problem.
myledger :: IO Ledger
myledger = do
  l <- myrawledger
  return $ cacheLedger $ filterRawLedger "" "" [] False False l

-- | get a named account from your ledger file
myaccount :: AccountName -> IO Account
myaccount a = myledger >>= (return . fromMaybe nullacct . Map.lookup a . accountmap)

-- | Check if a set of ledger account/description patterns matches the
-- given account name or entry description.  Patterns are case-insensitive
-- regular expression strings; those beginning with - are anti-patterns.
matchpats :: [String] -> String -> Bool
matchpats pats str =
    (null positives || any match positives) && (null negatives || not (any match negatives))
    where
      (negatives,positives) = partition isnegativepat pats
      match "" = True
      match pat = matchregex (abspat pat) str

-- | Similar to matchpats, but follows the special behaviour of ledger
-- 2.6's balance command: positive patterns which do not contain : match
-- the account leaf name, other patterns match the full account name.
matchpats_balance :: [String] -> String -> Bool
matchpats_balance pats str = match_positive_pats pats str && (not $ match_negative_pats pats str)
--    (null positives || any match positives) && (null negatives || not (any match negatives))
--     where
--       (negatives,positives) = partition isnegativepat pats
--       match "" = True
--       match pat = matchregex (abspat pat) matchee
--           where 
--             matchee = if not (':' `elem` pat) && not (isnegativepat pat)
--                       then accountLeafName str
--                       else str

-- | Do the positives in these patterns permit a match for this string ?
match_positive_pats :: [String] -> String -> Bool
match_positive_pats pats str = (null ps) || (any match ps)
    where
      ps = positivepats pats
      match "" = True
      match p = matchregex (abspat p) matchee
          where 
            matchee | ':' `elem` p = str
                    | otherwise = accountLeafName str

-- | Do the negatives in these patterns prevent a match for this string ?
match_negative_pats :: [String] -> String -> Bool
match_negative_pats pats str = (not $ null ns) && (any match ns)
    where
      ns = map abspat $ negativepats pats
      match "" = True
      match p = matchregex (abspat p) str

matchregex pat str = containsRegex (mkRegexWithOpts pat True True) str
isnegativepat pat = (== [Ledger.negativepatternchar]) $ take 1 pat
abspat pat = if isnegativepat pat then drop 1 pat else pat
positivepats = filter (not . isnegativepat)
negativepats = filter isnegativepat
