{-# LANGUAGE ScopedTypeVariables, PackageImports #-}
{-|

Easy regular expression helpers, currently based on regex-tdfa. These should:

- be cross-platform, not requiring C libraries

- support unicode

- support extended regular expressions

- support replacement, with backreferences etc.

- support splitting

- have mnemonic names

- have simple monomorphic types

- work with simple strings

Regex strings are automatically compiled into regular expressions the
first time they are seen, and these are cached. If you use a huge
number of unique regular expressions this might lead to increased
memory usage. Several functions have memoised variants (*Memo), which
also trade space for time.

Current limitations:

- (?i) and similar are not supported

-}

module Hledger.Utils.Regex (
   -- * type aliases
   Regexp
  ,Replacement
   -- * standard regex operations
  ,regexMatches
  ,regexMatchesCI
  ,regexReplace
  ,regexReplaceCI
  ,regexReplaceMemo
  ,regexReplaceCIMemo
  ,regexReplaceBy
  ,regexReplaceByCI
  )
where

import Data.Array
import Data.Char
import Data.List (foldl')
import Data.MemoUgly (memo)
import "regex-tdfa" Text.Regex.TDFA (
  Regex, CompOption(..), ExecOption(..), defaultCompOpt, defaultExecOpt,
  makeRegexOpts, AllMatches(getAllMatches), match, (=~), MatchText
  )

-- import Hledger.Utils.Debug
import Hledger.Utils.UTF8IOCompat (error')


-- | Regular expression. Extended regular expression-ish syntax ? But does not support eg (?i) syntax.
type Regexp = String

-- | A replacement pattern. May include numeric backreferences (\N).
type Replacement = String

-- | Convert our string-based regexps to real ones. Can fail if the
-- string regexp is malformed.
toRegex :: Regexp -> Regex
toRegex = memo (makeRegexOpts compOpt execOpt)

toRegexCI :: Regexp -> Regex
toRegexCI = memo (makeRegexOpts compOpt{caseSensitive=False} execOpt)

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

-- | Replace all occurrences of the regexp, transforming each match with the given function.
regexReplaceBy :: Regexp -> (String -> String) -> String -> String
regexReplaceBy r = replaceAllBy (toRegex r)

regexReplaceByCI :: Regexp -> (String -> String) -> String -> String
regexReplaceByCI r = replaceAllBy (toRegexCI r)

-- | Replace all occurrences of the regexp with the replacement
-- pattern. The replacement pattern supports numeric backreferences
-- (\N) but no other RE syntax.
regexReplace :: Regexp -> Replacement -> String -> String
regexReplace re = replaceRegex (toRegex re)

regexReplaceCI :: Regexp -> Replacement -> String -> String
regexReplaceCI re = replaceRegex (toRegexCI re)

-- | A memoising version of regexReplace. Caches the result for each
-- search pattern, replacement pattern, target string tuple.
regexReplaceMemo :: Regexp -> Replacement -> String -> String
regexReplaceMemo re repl = memo (regexReplace re repl)

regexReplaceCIMemo :: Regexp -> Replacement -> String -> String
regexReplaceCIMemo re repl = memo (regexReplaceCI re repl)

--

replaceRegex :: Regex -> Replacement -> String -> String
replaceRegex re repl s = foldl (replaceMatch repl) s (reverse $ match re s :: [MatchText String])

replaceMatch :: Replacement -> String -> MatchText String -> String
replaceMatch replpat s matchgroups = pre ++ repl ++ post
  where
    ((_,(off,len)):_) = elems matchgroups  -- groups should have 0-based indexes, and there should always be at least one, since this is a match
    (pre, post') = splitAt off s
    post = drop len post'
    repl = replaceAllBy (toRegex "\\\\[0-9]+") (replaceBackReference matchgroups) replpat

replaceBackReference :: MatchText String -> String -> String
replaceBackReference grps ('\\':s@(_:_)) | all isDigit s =
  case read s of n | n `elem` indices grps -> fst (grps ! n)
                 _                         -> error' $ "no match group exists for backreference \"\\"++s++"\""
replaceBackReference _ s = error' $ "replaceBackReference called on non-numeric-backreference \""++s++"\", shouldn't happen"

--

-- http://stackoverflow.com/questions/9071682/replacement-substition-with-haskell-regex-libraries :
-- | Replace all occurrences of a regexp in a string, transforming each match with the given function.
replaceAllBy :: Regex -> (String -> String) -> String -> String
replaceAllBy re f s = start end
  where
    (_, end, start) = foldl' go (0, s, id) $ (getAllMatches $ match re s :: [(Int, Int)])
    go (ind,read,write) (off,len) =
      let (skip, start) = splitAt (off - ind) read
          (matched, remaining) = splitAt len start
      in (off + len, remaining, write . (skip++) . (f matched ++))

