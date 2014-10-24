{-# LANGUAGE ScopedTypeVariables #-}
{-|

Easy regular expression helpers, based on regex-tdfa and regexpr. These should

- have mnemonic names

- have simple monomorphic types

- work with strings

- support extended regular expressions

- support replacement

- support splitting

- support unicode

- be cross-platform, not requiring C libraries

-}

module Hledger.Utils.Regex (
   Regexp
  ,Replacement
  -- ,regexMatch
  -- ,regexMatchCI
  ,regexMatches
  ,regexMatchesCI
  ,containsRegex
  ,regexReplace
  ,regexReplaceCI
  ,regexReplaceBy
  -- ,regexpToCI
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

-- regexpr - may be slow

regexSplit :: Regexp -> String -> [Regexp]
regexSplit = splitRegexPR

-- regexMatch :: Regexp -> String -> MatchFun Maybe
-- regexMatch r s = matchRegexPR r s

-- regexMatchCI :: Regexp -> String -> MatchFun Maybe
-- regexMatchCI r s = regexMatch (regexpToCI r) s

-- regexMatches :: Regexp -> String -> Bool
-- regexMatches r s = isJust $ matchRegexPR r s

-- regexMatchesCI :: Regexp -> String -> Bool
-- regexMatchesCI r s = regexMatches (regexpToCI r) s

-- regexReplace :: Regexp -> Replacement -> String -> String
-- regexReplace r repl s = gsubRegexPR r repl s

-- regexReplaceCI :: Regexp -> Replacement -> String -> String
-- regexReplaceCI r s = regexReplace (regexpToCI r) s

-- regexReplaceBy :: Regexp -> (String -> Replacement) -> String -> String
-- regexReplaceBy r replfn s = gsubRegexPRBy r replfn s

-- regex-tdfa

-- | Convert our string-based regexps to real ones. Can fail if the
-- string regexp is malformed.
toRegex :: Regexp -> Regex
toRegex = makeRegexOpts compOpt execOpt

compOpt :: CompOption
compOpt = defaultCompOpt

execOpt :: ExecOption
execOpt = defaultExecOpt

-- regexMatch' :: RegexContext Regexp String a => Regexp -> String -> a
-- regexMatch' r s = s =~ (toRegex r)

regexMatches :: Regexp -> String -> Bool
regexMatches = flip (=~)

regexMatchesCI :: Regexp -> String -> Bool
regexMatchesCI r = match (toRegexCI r)

regexReplace :: Regexp -> Replacement -> String -> String
regexReplace r repl = regexReplaceBy r (const repl)

regexReplaceCI :: Regexp -> Replacement -> String -> String
regexReplaceCI r repl = regexReplaceByCI r (const repl)

regexReplaceBy :: Regexp -> (String -> Replacement) -> String -> String
regexReplaceBy r = replaceAll (toRegex r)

regexReplaceByCI :: Regexp -> (String -> Replacement) -> String -> String
regexReplaceByCI r = replaceAll (toRegexCI r)

toRegexCI :: Regexp -> Regex
toRegexCI = makeRegexOpts compOpt{caseSensitive=False} execOpt

-- regexpToCI :: Regexp -> Regexp
-- regexpToCI r = "(?i)"++ r

-- from
-- http://stackoverflow.com/questions/9071682/replacement-substition-with-haskell-regex-libraries
-- | Replace all occurrences of a regexp in a string using a replacer
-- function, which receives the matched string as its argument.
-- Does not support standard RE syntax such as \1.
replaceAll :: Regex -> (String -> Replacement) -> String -> String
replaceAll re f s = start end
  where
    (_, end, start) = foldl' go (0, s, id) $ getAllMatches $ match re s
    go (ind,read,write) (off,len) =
      let (skip, start) = splitAt (off - ind) read
          (matched, remaining) = splitAt len start
      in (off + len, remaining, write . (skip++) . (f matched ++))

