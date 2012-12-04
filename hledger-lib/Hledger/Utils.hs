{-# LANGUAGE CPP #-}
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
                          -- module Debug.Trace,
                          -- module Text.RegexPR,
                          -- module Test.HUnit,
                          -- module Text.Printf,
                          ---- all of this one:
                          module Hledger.Utils,
                          Debug.Trace.trace,
                          -- module Hledger.Utils.UTF8IOCompat
                          SystemString,fromSystemString,toSystemString,error',userError'
                          -- the rest need to be done in each module I think
                          )
where
import Control.Monad (liftM)
import Control.Monad.Error (MonadIO)
import Control.Monad.IO.Class (liftIO)
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Tree
import Debug.Trace
import System.Directory (getHomeDirectory)
import System.FilePath((</>), isRelative)
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.Printf
import Text.RegexPR
-- import qualified Data.Map as Map
-- 
-- import Prelude hiding (readFile,writeFile,appendFile,getContents,putStr,putStrLn)
-- import Hledger.Utils.UTF8IOCompat   (readFile,writeFile,appendFile,getContents,putStr,putStrLn)
import Hledger.Utils.UTF8IOCompat (SystemString,fromSystemString,toSystemString,error',userError')

-- strings

lowercase = map toLower
uppercase = map toUpper

strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t") :: String -> String
rstrip = reverse . lstrip . reverse

stripbrackets = dropWhile (`elem` "([") . reverse . dropWhile (`elem` "])") . reverse

elideLeft width s =
    if length s > width then ".." ++ reverse (take (width - 2) $ reverse s) else s

elideRight width s =
    if length s > width then take (width - 2) s ++ ".." else s

underline :: String -> String
underline s = s' ++ replicate (length s) '-' ++ "\n"
    where s'
            | last s == '\n' = s
            | otherwise = s ++ "\n"

-- | Wrap a string in single quotes, and \-prefix any embedded single
-- quotes, if it contains whitespace and is not already single- or
-- double-quoted.
quoteIfSpaced :: String -> String
quoteIfSpaced s | isSingleQuoted s || isDoubleQuoted s = s
                | not $ any (`elem` s) whitespacechars = s
                | otherwise = "'"++escapeSingleQuotes s++"'"

escapeSingleQuotes :: String -> String
escapeSingleQuotes = regexReplace "'" "\'"

escapeQuotes :: String -> String
escapeQuotes = regexReplace "([\"'])" "\\1"

-- | Quote-aware version of words - don't split on spaces which are inside quotes.
-- NB correctly handles "a'b" but not "''a''".
words' :: String -> [String]
words' = map stripquotes . fromparse . parsewith p
    where
      p = do ss <- (quotedPattern <|> pattern) `sepBy` many1 spacenonewline
             -- eof
             return ss
      pattern = many (noneOf whitespacechars)
      quotedPattern = between (oneOf "'\"") (oneOf "'\"") $ many $ noneOf "'\""

-- | Quote-aware version of unwords - single-quote strings which contain whitespace
unwords' :: [String] -> String
unwords' = unwords . map singleQuoteIfNeeded

-- | Single-quote this string if it contains whitespace or double-quotes
singleQuoteIfNeeded s | any (`elem` s) whitespacechars = "'"++s++"'"
                      | otherwise = s

whitespacechars = " \t\n\r"

-- | Strip one matching pair of single or double quotes on the ends of a string.
stripquotes :: String -> String
stripquotes s = if isSingleQuoted s || isDoubleQuoted s then init $ tail s else s

isSingleQuoted s@(_:_:_) = head s == '\'' && last s == '\''
isSingleQuoted _ = False

isDoubleQuoted s@(_:_:_) = head s == '"' && last s == '"'
isDoubleQuoted _ = False

unbracket :: String -> String
unbracket s
    | (head s == '[' && last s == ']') || (head s == '(' && last s == ')') = init $ tail s
    | otherwise = s

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
      xpad ls = map (padright w) ls where w | null ls = 0
                                            | otherwise = maximum $ map length ls
      padded = map (xpad . ypad) lss

-- | Compose strings vertically and right-aligned.
vConcatRightAligned :: [String] -> String
vConcatRightAligned ss = intercalate "\n" $ map showfixedwidth ss
    where
      showfixedwidth = printf (printf "%%%ds" width)
      width = maximum $ map length ss

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
cliptopleft w h = intercalate "\n" . take h . map (take w) . lines

-- | Clip and pad a multi-line string to fill the specified width and height.
fitto :: Int -> Int -> String -> String
fitto w h s = intercalate "\n" $ take h $ rows ++ repeat blankline
    where
      rows = map (fit w) $ lines s
      fit w = take w . (++ repeat ' ')
      blankline = replicate w ' '

-- math

difforzero :: (Num a, Ord a) => a -> a -> a
difforzero a b = maximum [(a - b), 0]

-- regexps

-- regexMatch :: String -> String -> MatchFun Maybe
regexMatch r s = matchRegexPR r s

-- regexMatchCI :: String -> String -> MatchFun Maybe
regexMatchCI r s = regexMatch (regexToCaseInsensitive r) s

regexMatches :: String -> String -> Bool
regexMatches r s = isJust $ matchRegexPR r s

regexMatchesCI :: String -> String -> Bool
regexMatchesCI r s = regexMatches (regexToCaseInsensitive r) s

containsRegex = regexMatchesCI

regexReplace :: String -> String -> String -> String
regexReplace r repl s = gsubRegexPR r repl s

regexReplaceCI :: String -> String -> String -> String
regexReplaceCI r s = regexReplace (regexToCaseInsensitive r) s

regexReplaceBy :: String -> (String -> String) -> String -> String
regexReplaceBy r replfn s = gsubRegexPRBy r replfn s

regexToCaseInsensitive :: String -> String
regexToCaseInsensitive r = "(?i)"++ r

-- lists

splitAtElement :: Eq a => a -> [a] -> [[a]]
splitAtElement e l = 
    case dropWhile (e==) l of
      [] -> []
      l' -> first : splitAtElement e rest
        where
          (first,rest) = break (e==) l'

-- trees

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


-- debugging

-- more:
-- http://hackage.haskell.org/packages/archive/TraceUtils/0.1.0.2/doc/html/Debug-TraceUtils.html
-- http://hackage.haskell.org/packages/archive/trace-call/0.1/doc/html/Debug-TraceCall.html
-- http://hackage.haskell.org/packages/archive/htrace/0.1/doc/html/Debug-HTrace.html
-- http://hackage.haskell.org/packages/archive/traced/2009.7.20/doc/html/Debug-Traced.html

-- | trace (print on stdout at runtime) a showable expression
-- (for easily tracing in the middle of a complex expression)
strace :: Show a => a -> a
strace a = trace (show a) a

-- | labelled trace showable - like strace, with a label prepended
lstrace :: Show a => String -> a -> a
lstrace l a = trace (l ++ ": " ++ show a) a

-- | monadic trace - like strace, but works as a standalone line in a monad
mtrace :: (Monad m, Show a) => a -> m a
mtrace a = strace a `seq` return a

-- | trace an expression using a custom show function
tracewith :: (a -> String) -> a -> a
tracewith f e = trace (f e) e

-- parsing

-- | Backtracking choice, use this when alternatives share a prefix.
-- Consumes no input if all choices fail.
choice' :: [GenParser tok st a] -> GenParser tok st a
choice' = choice . map Text.ParserCombinators.Parsec.try

parsewith :: Parser a -> String -> Either ParseError a
parsewith p = parse p ""

parseWithCtx :: b -> GenParser Char b a -> String -> Either ParseError a
parseWithCtx ctx p = runParser p ctx ""

fromparse :: Either ParseError a -> a
fromparse = either parseerror id

parseerror :: ParseError -> a
parseerror e = error' $ showParseError e

showParseError :: ParseError -> String
showParseError e = "parse error at " ++ show e

showDateParseError :: ParseError -> String
showDateParseError e = printf "date parse error (%s)" (intercalate ", " $ tail $ lines $ show e)

nonspace :: GenParser Char st Char
nonspace = satisfy (not . isSpace)

spacenonewline :: GenParser Char st Char
spacenonewline = satisfy (`elem` " \v\f\t")

restofline :: GenParser Char st String
restofline = anyChar `manyTill` newline

-- time

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ utcToLocalTime tz t

-- testing

-- | Get a Test's label, or the empty string.
testName :: Test -> String
testName (TestLabel n _) = n
testName _ = ""

-- | Flatten a Test containing TestLists into a list of single tests.
flattenTests :: Test -> [Test]
flattenTests (TestLabel _ t@(TestList _)) = flattenTests t
flattenTests (TestList ts) = concatMap flattenTests ts
flattenTests t = [t]

-- | Filter TestLists in a Test, recursively, preserving the structure.
filterTests :: (Test -> Bool) -> Test -> Test
filterTests p (TestLabel l ts) = TestLabel l (filterTests p ts)
filterTests p (TestList ts) = TestList $ filter (any p . flattenTests) $ map (filterTests p) ts
filterTests _ t = t

-- | Simple way to assert something is some expected value, with no label.
is :: (Eq a, Show a) => a -> a -> Assertion
a `is` e = assertEqual "" e a

-- | Assert a parse result is successful, printing the parse error on failure.
assertParse :: (Either ParseError a) -> Assertion
assertParse parse = either (assertFailure.show) (const (return ())) parse

-- | Assert a parse result is successful, printing the parse error on failure.
assertParseFailure :: (Either ParseError a) -> Assertion
assertParseFailure parse = either (const $ return ()) (const $ assertFailure "parse should not have succeeded") parse

-- | Assert a parse result is some expected value, printing the parse error on failure.
assertParseEqual :: (Show a, Eq a) => (Either ParseError a) -> a -> Assertion
assertParseEqual parse expected = either (assertFailure.show) (`is` expected) parse

printParseError :: (Show a) => a -> IO ()
printParseError e = do putStr "parse error at "; print e

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
expandPath :: MonadIO m => FilePath -> FilePath -> m FilePath -- general type sig for use in reader parsers
expandPath _ "-" = return "-"
expandPath curdir p = (if isRelative p then (curdir </>) else id) `liftM` expandPath' p
  where
    expandPath' ('~':'/':p)  = liftIO $ (</> p) `fmap` getHomeDirectory
    expandPath' ('~':'\\':p) = liftIO $ (</> p) `fmap` getHomeDirectory
    expandPath' ('~':_)      = error' "~USERNAME in paths is not supported"
    expandPath' p            = return p

firstJust ms = case dropWhile (==Nothing) ms of
    [] -> Nothing
    (md:_) -> md
