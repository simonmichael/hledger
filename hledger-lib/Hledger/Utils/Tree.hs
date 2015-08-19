module Hledger.Utils.Tree where

-- import Data.Char
import Data.List (foldl')
import qualified Data.Map as M
import Data.Tree
-- import Text.Parsec
-- import Text.Printf

import Hledger.Utils.Regex
-- import Hledger.Utils.UTF8IOCompat (error')

-- standard tree helpers

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
subtreeinforest _ [] = Nothing
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
treeany f t = f (root t) || any (treeany f) (branches t)

-- treedrop -- remove the leaves which do fulfill predicate.
-- treedropall -- do this repeatedly.

-- | show a compact ascii representation of a tree
showtree :: Show a => Tree a -> String
showtree = unlines . filter (regexMatches "[^ \\|]") . lines . drawTree . treemap show

-- | show a compact ascii representation of a forest
showforest :: Show a => Forest a -> String
showforest = concatMap showtree


-- | An efficient-to-build tree suggested by Cale Gibbard, probably
-- better than accountNameTreeFrom.
newtype FastTree a = T (M.Map a (FastTree a))
  deriving (Show, Eq, Ord)

emptyTree = T M.empty

mergeTrees :: (Ord a) => FastTree a -> FastTree a -> FastTree a
mergeTrees (T m) (T m') = T (M.unionWith mergeTrees m m')

treeFromPath :: [a] -> FastTree a
treeFromPath []     = T M.empty
treeFromPath (x:xs) = T (M.singleton x (treeFromPath xs))

treeFromPaths :: (Ord a) => [[a]] -> FastTree a
treeFromPaths = foldl' mergeTrees emptyTree . map treeFromPath


