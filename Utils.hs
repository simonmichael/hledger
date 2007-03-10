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


rhead = head . reverse 
rtail = reverse . tail . reverse 

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

