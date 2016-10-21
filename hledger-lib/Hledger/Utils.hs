{-|

Standard imports and utilities which are useful everywhere, or needed low
in the module hierarchy. This is the bottom of hledger's module graph.

-}

module Hledger.Utils (---- provide these frequently used modules - or not, for clearer api:
                          -- module Control.Monad,
                          -- module Data.List,
                          -- module Data.Maybe,
                          -- module Data.Time.Calendar,
                          -- module Data.Time.Clock,
                          -- module Data.Time.LocalTime,
                          -- module Data.Tree,
                          -- module Text.RegexPR,
                          -- module Test.HUnit,
                          -- module Text.Printf,
                          ---- all of this one:
                          module Hledger.Utils,
                          module Hledger.Utils.Debug,
                          module Hledger.Utils.Parse,
                          module Hledger.Utils.Regex,
                          module Hledger.Utils.String,
                          module Hledger.Utils.Text,
                          module Hledger.Utils.Test,
                          module Hledger.Utils.Tree,
                          -- Debug.Trace.trace,
                          -- module Data.PPrint,
                          -- module Hledger.Utils.UTF8IOCompat
                          SystemString,fromSystemString,toSystemString,error',userError',
                          -- the rest need to be done in each module I think
                          )
where
import Control.Monad (liftM)
-- import Data.Char
-- import Data.List
-- import Data.Maybe
-- import Data.PPrint
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time.Clock
import Data.Time.LocalTime
-- import Data.Text (Text)
-- import qualified Data.Text as T
import System.Directory (getHomeDirectory)
import System.FilePath((</>), isRelative)
import System.IO
-- import Text.Printf
-- import qualified Data.Map as Map

import Hledger.Utils.Debug
import Hledger.Utils.Parse
import Hledger.Utils.Regex
import Hledger.Utils.String
import Hledger.Utils.Text
import Hledger.Utils.Test
import Hledger.Utils.Tree
-- import Prelude hiding (readFile,writeFile,appendFile,getContents,putStr,putStrLn)
-- import Hledger.Utils.UTF8IOCompat   (readFile,writeFile,appendFile,getContents,putStr,putStrLn)
import Hledger.Utils.UTF8IOCompat (SystemString,fromSystemString,toSystemString,error',userError')


-- tuples

first3  (x,_,_) = x
second3 (_,x,_) = x
third3  (_,_,x) = x

first4  (x,_,_,_) = x
second4 (_,x,_,_) = x
third4  (_,_,x,_) = x
fourth4 (_,_,_,x) = x

first5  (x,_,_,_,_) = x
second5 (_,x,_,_,_) = x
third5  (_,_,x,_,_) = x
fourth5 (_,_,_,x,_) = x
fifth5  (_,_,_,_,x) = x

first6  (x,_,_,_,_,_) = x
second6 (_,x,_,_,_,_) = x
third6  (_,_,x,_,_,_) = x
fourth6 (_,_,_,x,_,_) = x
fifth6  (_,_,_,_,x,_) = x
sixth6  (_,_,_,_,_,x) = x

-- lists

splitAtElement :: Eq a => a -> [a] -> [[a]]
splitAtElement x l =
  case l of
    []          -> []
    e:es | e==x -> split es
    es          -> split es
  where
    split es = let (first,rest) = break (x==) es
               in first : splitAtElement x rest

-- text

-- time

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz t

getCurrentZonedTime :: IO ZonedTime
getCurrentZonedTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToZonedTime tz t

-- misc

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft

-- | Apply a function the specified number of times. Possibly uses O(n) stack ?
applyN :: Int -> (a -> a) -> a -> a
applyN n f = (!! n) . iterate f

-- | Convert a possibly relative, possibly tilde-containing file path to an absolute one,
-- given the current directory. ~username is not supported. Leave "-" unchanged.
-- Can raise an error.
expandPath :: FilePath -> FilePath -> IO FilePath -- general type sig for use in reader parsers
expandPath _ "-" = return "-"
expandPath curdir p = (if isRelative p then (curdir </>) else id) `liftM` expandPath' p
  where
    expandPath' ('~':'/':p)  = (</> p) <$> getHomeDirectory
    expandPath' ('~':'\\':p) = (</> p) <$> getHomeDirectory
    expandPath' ('~':_)      = ioError $ userError "~USERNAME in paths is not supported"
    expandPath' p            = return p

firstJust ms = case dropWhile (==Nothing) ms of
    [] -> Nothing
    (md:_) -> md

-- | Read a file in universal newline mode, handling any of the usual line ending conventions.
readFile' :: FilePath -> IO Text
readFile' name =  do
  h <- openFile name ReadMode
  hSetNewlineMode h universalNewlineMode
  T.hGetContents h

-- | Read a file in universal newline mode, handling any of the usual line ending conventions.
readFileAnyLineEnding :: FilePath -> IO Text
readFileAnyLineEnding path =  do
  h <- openFile path ReadMode
  hSetNewlineMode h universalNewlineMode
  T.hGetContents h

-- | Read the given file, or standard input if the path is "-", using
-- universal newline mode.
readFileOrStdinAnyLineEnding :: String -> IO Text
readFileOrStdinAnyLineEnding f = do
  h <- fileHandle f
  hSetNewlineMode h universalNewlineMode
  T.hGetContents h
  where
    fileHandle "-" = return stdin
    fileHandle f = openFile f ReadMode

-- | Total version of maximum, for integral types, giving 0 for an empty list.
maximum' :: Integral a => [a] -> a
maximum' [] = 0
maximum' xs = maximum xs
