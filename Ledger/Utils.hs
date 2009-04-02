{-|

This is the bottom of the module hierarchy. It provides a number of
standard modules and utilities which are useful everywhere (or, are needed
low in the hierarchy). The "hledger prelude".

-}

module Ledger.Utils (
module Char,
module Control.Monad,
module Data.List,
--module Data.Map,
module Data.Maybe,
module Data.Ord,
module Data.Tree,
module Data.Time.Clock,
module Data.Time.Calendar,
module Data.Time.LocalTime,
module Debug.Trace,
module Ledger.Utils,
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
import Data.Tree
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Debug.Trace
import Test.HUnit
import Text.Printf
import Text.Regex
import Text.ParserCombinators.Parsec


-- strings

lowercase = map toLower
uppercase = map toUpper

strip = dropspaces . reverse . dropspaces . reverse
    where dropspaces = dropWhile (`elem` " \t")

elideLeft width s =
    case length s > width of
      True -> ".." ++ (reverse $ take (width - 2) $ reverse s)
      False -> s

elideRight width s =
    case length s > width of
      True -> take (width - 2) s ++ ".."
      False -> s

-- | Join multi-line strings as side-by-side rectangular strings of the same height, top-padded.
concatTopPadded :: [String] -> String
concatTopPadded strs = intercalate "\n" $ map concat $ transpose padded
    where
      lss = map lines strs
      h = maximum $ map length lss
      ypad ls = replicate (difforzero h (length ls)) "" ++ ls
      xpad ls = map (padleft w) ls where w | null ls = 0
                                           | otherwise = maximum $ map length ls
      padded = map (xpad . ypad) lss

-- | Join multi-line strings as side-by-side rectangular strings of the same height, bottom-padded.
concatBottomPadded :: [String] -> String
concatBottomPadded strs = intercalate "\n" $ map concat $ transpose padded
    where
      lss = map lines strs
      h = maximum $ map length lss
      ypad ls = ls ++ replicate (difforzero h (length ls)) ""
      xpad ls = map (padleft w) ls where w | null ls = 0
                                           | otherwise = maximum $ map length ls
      padded = map (xpad . ypad) lss

-- | Convert a multi-line string to a rectangular string top-padded to the specified height.
padtop :: Int -> String -> String
padtop h s = intercalate "\n" xpadded
    where
      ls = lines s
      sh = length ls
      sw | null ls = 0
         | otherwise = maximum $ map length ls
      ypadded = replicate (difforzero h sh) "" ++ ls
      xpadded = map (padleft sw) ypadded

-- | Convert a multi-line string to a rectangular string bottom-padded to the specified height.
padbottom :: Int -> String -> String
padbottom h s = intercalate "\n" xpadded
    where
      ls = lines s
      sh = length ls
      sw | null ls = 0
         | otherwise = maximum $ map length ls
      ypadded = ls ++ replicate (difforzero h sh) ""
      xpadded = map (padleft sw) ypadded

-- | Convert a multi-line string to a rectangular string left-padded to the specified width.
padleft :: Int -> String -> String
padleft w "" = concat $ replicate w " "
padleft w s = intercalate "\n" $ map (printf (printf "%%%ds" w)) $ lines s

-- | Convert a multi-line string to a rectangular string right-padded to the specified width.
padright :: Int -> String -> String
padright w "" = concat $ replicate w " "
padright w s = intercalate "\n" $ map (printf (printf "%%-%ds" w)) $ lines s

-- | Clip a multi-line string to the specified width and height from the top left.
cliptopleft :: Int -> Int -> String -> String
cliptopleft w h s = intercalate "\n" $ take h $ map (take w) $ lines s

-- | Clip and pad a multi-line string to fill the specified width and height.
fitto :: Int -> Int -> String -> String
fitto w h s = intercalate "\n" $ take h $ rows ++ repeat blankline
    where
      rows = map (fit w) $ lines s
      fit w s = take w $ s ++ repeat ' '
      blankline = replicate w ' '

-- math

difforzero :: (Num a, Ord a) => a -> a -> a
difforzero a b = maximum [(a - b), 0]

-- regexps

instance Show Regex where show r = "a Regex"

containsRegex :: Regex -> String -> Bool
containsRegex r s = case matchRegex r s of
                      Just _ -> True
                      otherwise -> False


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

-- | List just the leaf nodes of a tree
leaves :: Tree a -> [a]
leaves (Node v []) = [v]
leaves (Node _ branches) = concatMap leaves branches

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

-- | trace (print on stdout at runtime) a showable expression
-- (for easily tracing in the middle of a complex expression)
strace :: Show a => a -> a
strace a = trace (show a) a

-- | labelled trace - like strace, with a newline and a label prepended
ltrace :: Show a => String -> a -> a
ltrace l a = trace (l ++ ": " ++ show a) a

-- parsing

parsewith :: Parser a -> String -> Either ParseError a
parsewith p ts = parse p "" ts

fromparse :: Either ParseError a -> a
fromparse = either (\e -> error $ "parse error at "++(show e)) id

nonspace :: GenParser Char st Char
nonspace = satisfy (not . isSpace)

spacenonewline :: GenParser Char st Char
spacenonewline = satisfy (\c -> c `elem` " \v\f\t")

restofline :: GenParser Char st String
restofline = anyChar `manyTill` newline




isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft

