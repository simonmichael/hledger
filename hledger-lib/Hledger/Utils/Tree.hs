module Hledger.Utils.Tree
( FastTree(..)
, treeFromPaths
) where

-- import Data.Char
import Data.List (foldl')
import qualified Data.Map as M

-- | An efficient-to-build tree suggested by Cale Gibbard, probably
-- better than accountNameTreeFrom.
newtype FastTree a = T (M.Map a (FastTree a))
  deriving (Show, Eq, Ord)

emptyTree :: FastTree a
emptyTree = T M.empty

mergeTrees :: (Ord a) => FastTree a -> FastTree a -> FastTree a
mergeTrees (T m) (T m') = T (M.unionWith mergeTrees m m')

treeFromPath :: [a] -> FastTree a
treeFromPath []     = T M.empty
treeFromPath (x:xs) = T (M.singleton x (treeFromPath xs))

treeFromPaths :: (Ord a) => [[a]] -> FastTree a
treeFromPaths = foldl' mergeTrees emptyTree . map treeFromPath
