{-# LANGUAGE ScopedTypeVariables #-}
{-

Easy regular expression helpers.

These should
- have mnemonic names
- have simple monomorphic types
- work with strings
- support extended regular expressions
- support replacement
- support splitting
- support unicode
- be cross-platform, not requiring C libraries

They currently can
- die on malformed regexps
- be slow (regexpr)

-}

module Hledger.Utils.Regex (
  --  regexMatch
  -- ,regexMatchCI
   regexMatches
  ,regexMatchesCI
  ,containsRegex
  ,regexReplace
  ,regexReplaceCI
  ,regexReplaceBy
  ,regexToCaseInsensitive
  ,regexSplit
  ,toRegex
  )
where

import Data.List (foldl')
import Text.RegexPR (splitRegexPR)
import Text.Regex.TDFA (
  Regex, CompOption(..), ExecOption(..), defaultCompOpt, defaultExecOpt,
  makeRegexOpts, AllMatches(getAllMatches), match, (=~)
  )


type Regexp = String
type Replacement = String

containsRegex :: Regexp -> String -> Bool
containsRegex = regexMatchesCI

regexToCaseInsensitive :: Regexp -> Regexp
regexToCaseInsensitive r = "(?i)"++ r

-- regexpr - may be slow

regexSplit :: Regexp -> String -> [Regexp]
regexSplit = splitRegexPR

-- regexMatch :: Regexp -> String -> MatchFun Maybe
-- regexMatch r s = matchRegexPR r s

-- regexMatchCI :: Regexp -> String -> MatchFun Maybe
-- regexMatchCI r s = regexMatch (regexToCaseInsensitive r) s

-- regexMatches :: Regexp -> String -> Bool
-- regexMatches r s = isJust $ matchRegexPR r s

-- regexMatchesCI :: Regexp -> String -> Bool
-- regexMatchesCI r s = regexMatches (regexToCaseInsensitive r) s

-- regexReplace :: Regexp -> Replacement -> String -> String
-- regexReplace r repl s = gsubRegexPR r repl s

-- regexReplaceCI :: Regexp -> Replacement -> String -> String
-- regexReplaceCI r s = regexReplace (regexToCaseInsensitive r) s

-- regexReplaceBy :: Regexp -> (String -> Replacement) -> String -> String
-- regexReplaceBy r replfn s = gsubRegexPRBy r replfn s

-- regex-tdfa

compOpt :: CompOption
compOpt = defaultCompOpt

execOpt :: ExecOption
execOpt = defaultExecOpt

toRegex :: Regexp -> Regex
toRegex = makeRegexOpts compOpt execOpt

-- regexMatch' :: RegexContext Regexp String a => Regexp -> String -> a
-- regexMatch' r s = s =~ (toRegex r)

regexMatches :: Regexp -> String -> Bool
regexMatches = flip (=~)

regexMatchesCI :: Regexp -> String -> Bool
regexMatchesCI r = match (makeRegexOpts compOpt{caseSensitive=False} execOpt r)

regexReplace :: Regexp -> Replacement -> String -> String
regexReplace r repl = regexReplaceBy r (const repl)

regexReplaceCI :: Regexp -> Replacement -> String -> String
regexReplaceCI r s = regexReplace (regexToCaseInsensitive r) s

regexReplaceBy :: Regexp -> (String -> Replacement) -> String -> String
regexReplaceBy r = replaceAll (toRegex r)

-- from http://stackoverflow.com/questions/9071682/replacement-substition-with-haskell-regex-libraries
replaceAll :: Regex -> (String -> Replacement) -> String -> String
replaceAll re f s = start end
  where
    (_, end, start) = foldl' go (0, s, id) $ getAllMatches $ match re s
    go (ind,read,write) (off,len) =
      let (skip, start) = splitAt (off - ind) read 
          (matched, remaining) = splitAt len start 
      in (off + len, remaining, write . (skip++) . (f matched ++))

