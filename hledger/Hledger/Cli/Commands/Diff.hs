{-|

The @diff@ command compares two diff.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Diff (
  diffmode
 ,diff
) where

import Data.List
import Data.Function
import Data.Ord
import Data.Maybe
import Data.Time
import Data.Either
import qualified Data.Text as T
import System.Exit

import Hledger
import Prelude hiding (putStrLn)
import Hledger.Utils.UTF8IOCompat (putStrLn)
import Hledger.Cli.CliOptions

-- | Command line options for this command.
diffmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Diff.txt")
  []
  [generalflagsgroup2]
  []
  ([], Just $ argsFlag "-f FILE1 -f FILE2 FULLACCOUNTTNAME")

data PostingWithPath = PostingWithPath {
                     ppposting :: Posting,
                     pptxnidx :: Int,
                     pppidx :: Int }
                 deriving (Show)

instance Eq PostingWithPath where
    a == b = pptxnidx a == pptxnidx b
          && pppidx a == pppidx b

pptxn :: PostingWithPath -> Transaction
pptxn = fromJust . ptransaction . ppposting

ppamountqty :: PostingWithPath -> Quantity
ppamountqty = aquantity . head . amounts . pamount . ppposting

allPostingsWithPath :: Journal -> [PostingWithPath]
allPostingsWithPath j = do
    (txnidx, txn) <- zip [0..] $ jtxns j
    (pidx, p) <- zip [0..] $ tpostings txn
    return PostingWithPath { ppposting = p, pptxnidx = txnidx, pppidx = pidx }

binBy :: Ord b => (a -> b) -> [a] -> [[a]]
binBy f = groupBy ((==) `on` f) . sortBy (comparing f)

combine :: ([a], [b]) -> [Either a b]
combine (ls, rs) = map Left ls ++ map Right rs

combinedBinBy :: Ord b => (a -> b) -> ([a], [a]) -> [([a], [a])]
combinedBinBy f = map partitionEithers . binBy (either f f) . combine

greedyMaxMatching :: (Eq a, Eq b) => [(a,b)] -> [(a,b)]
greedyMaxMatching = greedyMaxMatching' []

greedyMaxMatching' :: (Eq a, Eq b) => [Either a b] -> [(a,b)] -> [(a,b)]
greedyMaxMatching' alreadyUsed ((l,r):rest)
  | Left l `elem` alreadyUsed || Right r `elem` alreadyUsed
      = greedyMaxMatching' alreadyUsed rest
  | otherwise = (l,r) : greedyMaxMatching' (Left l : Right r : alreadyUsed) rest
greedyMaxMatching' _ [] = []

dateCloseness :: (PostingWithPath, PostingWithPath) -> Integer
dateCloseness = negate . uncurry (diffDays `on` tdate.pptxn)

type Matching = [(PostingWithPath, PostingWithPath)]

matching :: [PostingWithPath] -> [PostingWithPath] -> Matching
matching ppl ppr = do
    (left, right) <- combinedBinBy ppamountqty (ppl, ppr) -- TODO: probably not a correct choice of bins
    greedyMaxMatching $ sortBy (comparing dateCloseness) [ (l,r) | l <- left, r <- right ]

readJournalFile' :: FilePath -> IO Journal
readJournalFile' fn =
    readJournalFile definputopts {ignore_assertions_ = True} fn >>= either error' return  -- PARTIAL:

matchingPostings :: AccountName -> Journal -> [PostingWithPath]
matchingPostings acct j = filter ((== acct) . paccount . ppposting) $ allPostingsWithPath j

pickSide :: Side -> (a,a) -> a
pickSide L (l,_) = l
pickSide R (_,r) = r

unmatchedtxns :: Side -> [PostingWithPath] -> Matching -> [Transaction]
unmatchedtxns s pp m =
    map pptxn $ nubBy ((==) `on` pptxnidx) $ pp \\ map (pickSide s) m

-- | The diff command.
diff :: CliOpts -> Journal -> IO ()
diff CliOpts{file_=[f1, f2], reportspec_=ReportSpec{rsQuery=Acct acctRe}} _ = do
  j1 <- readJournalFile' f1
  j2 <- readJournalFile' f2

  let acct = T.pack $ reString acctRe
  let pp1 = matchingPostings acct j1
  let pp2 = matchingPostings acct j2

  let m = matching pp1 pp2

  let unmatchedtxn1 = unmatchedtxns L pp1 m
  let unmatchedtxn2 = unmatchedtxns R pp2 m

  putStrLn "These transactions are in the first file only:\n"
  mapM_ (putStr . showTransaction) unmatchedtxn1

  putStrLn "These transactions are in the second file only:\n"
  mapM_ (putStr . showTransaction) unmatchedtxn2

diff _ _ = do
  putStrLn "Please specify two input files. Usage: hledger diff -f FILE1 -f FILE2 FULLACCOUNTNAME"
  exitFailure
