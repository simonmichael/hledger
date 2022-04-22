{-|

Standard imports and utilities which are useful everywhere, or needed low
in the module hierarchy. This is the bottom of hledger's module graph.

-}
{-# LANGUAGE LambdaCase #-}

module Hledger.Utils (---- provide these frequently used modules - or not, for clearer api:
                          -- module Control.Monad,
                          -- module Data.List,
                          -- module Data.Maybe,
                          -- module Data.Time.Calendar,
                          -- module Data.Time.Clock,
                          -- module Data.Time.LocalTime,
                          -- module Data.Tree,
                          -- module Text.RegexPR,
                          -- module Text.Printf,
                          ---- all of this one:
                          module Hledger.Utils,
                          module Hledger.Utils.Debug,
                          module Hledger.Utils.Parse,
                          module Hledger.Utils.Regex,
                          module Hledger.Utils.String,
                          module Hledger.Utils.Text,
                          module Hledger.Utils.Test,
                          -- Debug.Trace.trace,
                          -- module Data.PPrint,
                          -- the rest need to be done in each module I think
                          )
where

import Control.Monad (when)
import Data.Char (toLower)
import Data.FileEmbed (makeRelativeToProject, embedStringFile)
import Data.List.Extra (foldl', foldl1', uncons, unsnoc)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Builder as TB
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (LocalTime, ZonedTime, getCurrentTimeZone,
                            utcToLocalTime, utcToZonedTime)
import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)
-- import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Q, Exp)
import Lens.Micro ((&), (.~))
import Lens.Micro.TH (DefName(TopName), lensClass, lensField, makeLensesWith, classyRules)
import System.Console.ANSI (Color,ColorIntensity,ConsoleLayer(..), SGR(..), setSGRCode)
import System.Directory (getHomeDirectory)
import System.FilePath (isRelative, (</>))
import System.IO
  (Handle, IOMode (..), hGetEncoding, hSetEncoding, hSetNewlineMode,
   openFile, stdin, universalNewlineMode, utf8_bom)

import Hledger.Utils.Debug
import Hledger.Utils.Parse
import Hledger.Utils.Regex
import Hledger.Utils.String
import Hledger.Utils.Text
import Hledger.Utils.Test
import Data.Tree (foldTree, Tree (Node, subForest))


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

-- currying

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f x y = f (x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f (x, y) = f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f w x y z = f (w, x, y, z)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (w, x, y, z) = f w x y z

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

-- trees

-- | Get the leaves of this tree as a list. 
-- The topmost node ("root" in hledger account trees) is not counted as a leaf.
treeLeaves :: Show a => Tree a -> [a]
treeLeaves Node{subForest=[]} = []
treeLeaves t = foldTree (\a bs -> (if null bs then (a:) else id) $ concat bs) t

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

-- | Apply a function the specified number of times,
-- which should be > 0 (otherwise does nothing).
-- Possibly uses O(n) stack ?
applyN :: Int -> (a -> a) -> a -> a
applyN n f | n < 1     = id
           | otherwise = (!! n) . iterate f
-- from protolude, compare
-- applyN :: Int -> (a -> a) -> a -> a
-- applyN n f = X.foldr (.) identity (X.replicate n f)

-- | Convert a possibly relative, possibly tilde-containing file path to an absolute one,
-- given the current directory. ~username is not supported. Leave "-" unchanged.
-- Can raise an error.
expandPath :: FilePath -> FilePath -> IO FilePath -- general type sig for use in reader parsers
expandPath _ "-" = return "-"
expandPath curdir p = (if isRelative p then (curdir </>) else id) <$> expandHomePath p
-- PARTIAL:

-- | Expand user home path indicated by tilde prefix
expandHomePath :: FilePath -> IO FilePath
expandHomePath = \case
    ('~':'/':p)  -> (</> p) <$> getHomeDirectory
    ('~':'\\':p) -> (</> p) <$> getHomeDirectory
    ('~':_)      -> ioError $ userError "~USERNAME in paths is not supported"
    p            -> return p

-- | Read text from a file,
-- converting any \r\n line endings to \n,,
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

-- | Like mapM but uses sequence'.
{-# INLINABLE mapM' #-}
mapM' :: Monad f => (a -> f b) -> [a] -> f [b]
mapM' f = sequence' . map f

-- | Find the number of digits of an 'Int'.
numDigitsInt :: Integral a => Int -> a
numDigitsInt n
    | n == minBound = 19  -- negate minBound is out of the range of Int
    | n < 0         = go (negate n)
    | otherwise     = go n
  where
    go a | a < 10                 = 1
         | a < 100                = 2
         | a < 1000               = 3
         | a < 10000              = 4
         | a >= 10000000000000000 = 16 + go (a `quot` 10000000000000000)
         | a >= 100000000         = 8  + go (a `quot` 100000000)
         | otherwise              = 4  + go (a `quot` 10000)
{-# INLINE numDigitsInt #-}

-- | Simpler alias for errorWithoutStackTrace
error' :: String -> a
error' = errorWithoutStackTrace . ("Error: " <>)

-- | A version of errorWithoutStackTrace that adds a usage hint.
usageError :: String -> a
usageError = error' . (++ " (use -h to see usage)")

-- | Like embedFile, but takes a path relative to the package directory.
-- Similar to embedFileRelative ?
embedFileRelative :: FilePath -> Q Exp
embedFileRelative f = makeRelativeToProject f >>= embedStringFile

-- -- | Like hereFile, but takes a path relative to the package directory.
-- -- Similar to embedFileRelative ?
-- hereFileRelative :: FilePath -> Q Exp
-- hereFileRelative f = makeRelativeToProject f >>= hereFileExp
--   where
--     QuasiQuoter{quoteExp=hereFileExp} = hereFile

-- | Wrap a string in ANSI codes to set and reset foreground colour.
color :: ColorIntensity -> Color -> String -> String
color int col s = setSGRCode [SetColor Foreground int col] ++ s ++ setSGRCode []

-- | Wrap a string in ANSI codes to set and reset background colour.
bgColor :: ColorIntensity -> Color -> String -> String
bgColor int col s = setSGRCode [SetColor Background int col] ++ s ++ setSGRCode []

-- | Wrap a WideBuilder in ANSI codes to set and reset foreground colour.
colorB :: ColorIntensity -> Color -> WideBuilder -> WideBuilder
colorB int col (WideBuilder s w) =
    WideBuilder (TB.fromString (setSGRCode [SetColor Foreground int col]) <> s <> TB.fromString (setSGRCode [])) w

-- | Wrap a WideBuilder in ANSI codes to set and reset background colour.
bgColorB :: ColorIntensity -> Color -> WideBuilder -> WideBuilder
bgColorB int col (WideBuilder s w) =
    WideBuilder (TB.fromString (setSGRCode [SetColor Background int col]) <> s <> TB.fromString (setSGRCode [])) w


-- | Make classy lenses for Hledger options fields.
-- This is intended to be used with BalancingOpts, InputOpt, ReportOpts,
-- ReportSpec, and CliOpts.
-- When run on X, it will create a typeclass named HasX (except for ReportOpts,
-- which will be named HasReportOptsNoUpdate) containing all the lenses for that type.
-- If the field name starts with an underscore, the lens name will be created
-- by stripping the underscore from the front on the name. If the field name ends with
-- an underscore, the field name ends with an underscore, the lens name will be
-- mostly created by stripping the underscore, but a few names for which this
-- would create too many conflicts instead have a second underscore appended.
-- ReportOpts fields for which updating them requires updating the query in
-- ReportSpec are instead names by dropping the trailing underscore and
-- appending NoUpdate to the name, e.g. querystring_ -> querystringNoUpdate.
--
-- There are a few reasons for the complicated rules.
-- - We have some legacy field names ending in an underscore (e.g. value_)
--   which we want to temporarily accommodate, before eventually switching to
--   a more modern style (e.g. _rsReportOpts)
-- - Certain fields in ReportOpts need to update the enclosing ReportSpec when
--   they are updated, and it is a common programming error to forget to do
--   this. We append NoUpdate to those lenses which will not update the
--   enclosing field, and reserve the shorter name for manually define lenses
--   (or at least something lens-like) which will update the ReportSpec.
-- cf. the lengthy discussion here and in surrounding comments:
-- https://github.com/simonmichael/hledger/pull/1545#issuecomment-881974554
makeHledgerClassyLenses :: Name -> DecsQ
makeHledgerClassyLenses x = flip makeLensesWith x $ classyRules
    & lensField .~ (\_ _ n -> fieldName $ nameBase n)
    & lensClass .~ (className . nameBase)
  where
    fieldName n | Just ('_', name) <- uncons n   = [TopName (mkName name)]
                | Just (name, '_') <- unsnoc n,
                  name `Set.member` queryFields  = [TopName (mkName $ name ++ "NoUpdate")]
                | Just (name, '_') <- unsnoc n,
                  name `Set.member` commonFields = [TopName (mkName $ name ++ "__")]
                | Just (name, '_') <- unsnoc n   = [TopName (mkName name)]
                | otherwise                      = []

    -- Fields which would cause too many conflicts if we exposed lenses with these names.
    commonFields = Set.fromList
        [ "empty", "drop", "color", "transpose"  -- ReportOpts
        , "anon", "new", "auto"                  -- InputOpts
        , "rawopts", "file", "debug", "width"    -- CliOpts
        ]

    -- When updating some fields of ReportOpts within a ReportSpec, we need to
    -- update the rsQuery term as well. To do this we implement a special
    -- HasReportOpts class with some special behaviour. We therefore give the
    -- basic lenses a special NoUpdate name to avoid conflicts.
    className "ReportOpts" = Just (mkName "HasReportOptsNoUpdate", mkName "reportOptsNoUpdate")
    className (x:xs)       = Just (mkName ("Has" ++ x:xs), mkName (toLower x : xs))
    className []           = Nothing

    -- Fields of ReportOpts which need to update the Query when they are updated.
    queryFields = Set.fromList ["period", "statuses", "depth", "date2", "real", "querystring"]

tests_Utils = testGroup "Utils" [
  tests_Text
  ]
