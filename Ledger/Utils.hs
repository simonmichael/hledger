{-|

Provide a number of standard modules and utilities.

-}

module Ledger.Utils (
module Char,
module Control.Monad,
module Data.List,
--module Data.Map,
module Data.Maybe,
module Data.Ord,
module Data.Time.Clock,
module Data.Time.Format,
module Data.Tree,
module Debug.Trace,
module Ledger.Utils,
module System.Locale,
module Text.Printf,
module Text.Regex,
module Test.HUnit,
)
where
import Char
import Control.Monad
import Data.List
--import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Format (ParseTime, parseTime, formatTime)
import Data.Tree
import Debug.Trace
import System.Locale (defaultTimeLocale)
import Test.HUnit
import Test.QuickCheck hiding (test, Testable)
import Text.Printf
import Text.Regex
import Text.ParserCombinators.Parsec (parse)


elideLeft width s =
    case length s > width of
      True -> ".." ++ (reverse $ take (width - 2) $ reverse s)
      False -> s

elideRight width s =
    case length s > width of
      True -> take (width - 2) s ++ ".."
      False -> s

-- regexps

instance Show Regex where show r = "a Regex"

containsRegex :: Regex -> String -> Bool
containsRegex r s = case matchRegex r s of
                      Just _ -> True
                      otherwise -> False

-- time

-- | Parse a date-time string to a time type, or raise an error.
parsedatetime :: ParseTime t => String -> t
parsedatetime s =
    parsetimewith "%Y/%m/%d %H:%M:%S" s $
    error $ printf "could not parse timestamp \"%s\"" s

-- | Parse a date string to a time type, or raise an error.
parsedate :: ParseTime t => String -> t
parsedate s = 
    parsetimewith "%Y/%m/%d" s $
    error $ printf "could not parse date \"%s\"" s

-- | Parse a time string to a time type using the provided pattern, or
-- return the default.
parsetimewith :: ParseTime t => String -> String -> t -> t
parsetimewith pat s def = fromMaybe def $ parseTime defaultTimeLocale pat s

-- lists

splitAtElement :: Eq a => a -> [a] -> [[a]]
splitAtElement e l = 
    case dropWhile (e==) l of
      [] -> []
      l' -> first : splitAtElement e rest
        where
          (first,rest) = break (e==) l'

-- trees

root = rootLabel
subs = subForest
branches = subForest

-- | get the sub-tree rooted at the first (left-most, depth-first) occurrence
-- of the specified node value
subtreeat :: Eq a => a -> Tree a -> Maybe (Tree a)
subtreeat v t
    | root t == v = Just t
    | otherwise = subtreeinforest v $ subs t

-- | get the sub-tree for the specified node value in the first tree in
-- forest in which it occurs.
subtreeinforest :: Eq a => a -> [Tree a] -> Maybe (Tree a)
subtreeinforest v [] = Nothing
subtreeinforest v (t:ts) = case (subtreeat v t) of
                             Just t' -> Just t'
                             Nothing -> subtreeinforest v ts
          
-- | remove all nodes past a certain depth
treeprune :: Int -> Tree a -> Tree a
treeprune 0 t = Node (root t) []
treeprune d t = Node (root t) (map (treeprune $ d-1) $ branches t)

-- | apply f to all tree nodes
treemap :: (a -> b) -> Tree a -> Tree b
treemap f t = Node (f $ root t) (map (treemap f) $ branches t)

-- | remove all subtrees whose nodes do not fulfill predicate
treefilter :: (a -> Bool) -> Tree a -> Tree a
treefilter f t = Node 
                 (root t) 
                 (map (treefilter f) $ filter (treeany f) $ branches t)
    
-- | is predicate true in any node of tree ?
treeany :: (a -> Bool) -> Tree a -> Bool
treeany f t = (f $ root t) || (any (treeany f) $ branches t)
    
-- treedrop -- remove the leaves which do fulfill predicate. 
-- treedropall -- do this repeatedly.

-- | show a compact ascii representation of a tree
showtree :: Show a => Tree a -> String
showtree = unlines . filter (containsRegex (mkRegex "[^ |]")) . lines . drawTree . treemap show

-- | show a compact ascii representation of a forest
showforest :: Show a => Forest a -> String
showforest = concatMap showtree

-- debugging

-- | trace a showable expression
strace a = trace (show a) a

p = putStr

-- testing

assertequal e a = assertEqual "" e a
assertnotequal e a = assertBool "expected inequality, got equality" (e /= a)

-- parsewith :: Parser a
parsewith p ts = parse p "" ts
fromparse = either (\_ -> error "parse error") id


