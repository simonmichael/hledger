module Utils (
              module Utils,
              module Data.List,
              module Data.Tree,
              module Debug.Trace,
              module Text.Printf,
              module Text.Regex,
              quickCheck,
             )
where
import System.Directory
import Data.List
import Data.Tree
import Debug.Trace
import Test.QuickCheck (quickCheck)
import Text.Printf
import Text.Regex


splitAtElement :: Eq a => a -> [a] -> [[a]]
splitAtElement e l = 
    case dropWhile (e==) l of
      [] -> []
      l' -> first : splitAtElement e rest
        where
          (first,rest) = break (e==) l'

-- courtesy of allberry_b
tildeExpand              :: FilePath -> IO FilePath
tildeExpand ('~':[])     =  getHomeDirectory
tildeExpand ('~':'/':xs) =  getHomeDirectory >>= return . (++ ('/':xs))
-- ~name, requires -fvia-C or ghc 6.8
--import System.Posix.User
-- tildeExpand ('~':xs)     =  do let (user, path) = span (/= '/') xs
--                                pw <- getUserEntryForName user
--                                return (homeDirectory pw ++ path)
tildeExpand xs           =  return xs


-- tree tools

root = rootLabel
branches = subForest

-- remove all nodes past a certain depth
treeprune :: Int -> Tree a -> Tree a
treeprune 0 t = Node (root t) []
treeprune d t = 
    Node (root t) (map (treeprune $ d-1) $ branches t)

-- apply f to all tree nodes
treemap :: (a -> b) -> Tree a -> Tree b
treemap f t = Node (f $ root t) (map (treemap f) $ branches t)

-- remove all subtrees whose nodes do not fulfill predicate
treefilter :: (a -> Bool) -> Tree a -> Tree a
treefilter f t = Node 
                 (root t) 
                 (map (treefilter f) $ filter (treeany f) $ branches t)
    
-- is predicate true in any node of tree ?
treeany :: (a -> Bool) -> Tree a -> Bool
treeany f t = (f $ root t) || (any (treeany f) $ branches t)
    
-- treedrop -- remove the leaves which do fulfill predicate. 
-- treedropall -- do this repeatedly.

