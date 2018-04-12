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
                          module Hledger.Utils.Color,
                          module Hledger.Utils.Tree,
                          -- Debug.Trace.trace,
                          -- module Data.PPrint,
                          -- module Hledger.Utils.UTF8IOCompat
                          SystemString,fromSystemString,toSystemString,error',userError',usageError,
                          -- the rest need to be done in each module I think
                          )
where
import Test.HUnit

import Control.Monad (liftM, when)
-- import Data.Char
import Data.Default
import Data.List
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
import Hledger.Utils.Color
import Hledger.Utils.Tree
-- import Prelude hiding (readFile,writeFile,appendFile,getContents,putStr,putStrLn)
-- import Hledger.Utils.UTF8IOCompat   (readFile,writeFile,appendFile,getContents,putStr,putStrLn)
import Hledger.Utils.UTF8IOCompat (SystemString,fromSystemString,toSystemString,error',userError',usageError)


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

instance Default Bool where def = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight = not . isLeft

-- | Apply a function the specified number of times. Possibly uses O(n) stack ?
applyN :: Int -> (a -> a) -> a -> a
applyN n f = (!! n) . iterate f
-- from protolude, compare
-- applyN :: Int -> (a -> a) -> a -> a
-- applyN n f = X.foldr (.) identity (X.replicate n f)

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

-- | Read text from a file, 
-- handling any of the usual line ending conventions,
-- using the system locale's text encoding,
-- ignoring any utf8 BOM prefix (as seen in paypal's 2018 CSV, eg) if that encoding is utf8. 
readFilePortably :: FilePath -> IO Text
readFilePortably f =  openFile f ReadMode >>= readHandlePortably

-- | Like readFilePortably, but read from standard input if the path is "-". 
readFileOrStdinPortably :: String -> IO Text
readFileOrStdinPortably f = openFileOrStdin f ReadMode >>= readHandlePortably
  where
    openFileOrStdin :: String -> IOMode -> IO Handle
    openFileOrStdin "-" _ = return stdin
    openFileOrStdin f m   = openFile f m

readHandlePortably :: Handle -> IO Text
readHandlePortably h = do
  hSetNewlineMode h universalNewlineMode
  menc <- hGetEncoding h
  when (fmap show menc == Just "UTF-8") $  -- XXX no Eq instance, rely on Show
    hSetEncoding h utf8_bom
  T.hGetContents h

-- | Total version of maximum, for integral types, giving 0 for an empty list.
maximum' :: Integral a => [a] -> a
maximum' [] = 0
maximum' xs = maximumStrict xs

-- | Strict version of sum that doesn’t leak space
{-# INLINABLE sumStrict #-}
sumStrict :: Num a => [a] -> a
sumStrict = foldl' (+) 0

-- | Strict version of maximum that doesn’t leak space
{-# INLINABLE maximumStrict #-}
maximumStrict :: Ord a => [a] -> a
maximumStrict = foldl1' max

-- | Strict version of minimum that doesn’t leak space
{-# INLINABLE minimumStrict #-}
minimumStrict :: Ord a => [a] -> a
minimumStrict = foldl1' min

-- | This is a version of sequence based on difference lists. It is
-- slightly faster but we mostly use it because it uses the heap
-- instead of the stack. This has the advantage that Neil Mitchell’s
-- trick of limiting the stack size to discover space leaks doesn’t
-- show this as a false positive.
{-# INLINABLE sequence' #-}
sequence' :: Monad f => [f a] -> f [a]
sequence' ms = do
  h <- go id ms
  return (h [])
  where
    go h [] = return h
    go h (m:ms) = do
      x <- m
      go (h . (x :)) ms

{-# INLINABLE mapM' #-}
mapM' :: Monad f => (a -> f b) -> [a] -> f [b]
mapM' f = sequence' . map f

tests_Hledger_Utils :: Test
tests_Hledger_Utils = TestList [
    tests_Hledger_Utils_Text
    ]