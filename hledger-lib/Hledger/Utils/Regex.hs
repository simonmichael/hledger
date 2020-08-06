{-# LANGUAGE ScopedTypeVariables #-}
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

Regex strings are automatically compiled into regular expressions the first
time they are seen, and these are cached. If you use a huge number of unique
regular expressions this might lead to increased memory usage. Several
functions have memoised variants (*Memo), which also trade space for time.

Currently two APIs are provided:

- The old partial one which will call error on any problem (eg with malformed
  regexps). This comes from hledger's origin as a command-line tool.

- The new total one (with _ suffixes) which will return an error message. This
  is better for long-running apps like hledger-web.

We are gradually replacing usage of the old API in hledger. Probably at some
point the suffixless names will be reclaimed for the new API.

Current limitations:

- (?i) and similar are not supported

-}

module Hledger.Utils.Regex (
   -- * type aliases
   Regexp
  ,Replacement
  ,RegexError
   -- * partial regex operations (may call error)
  ,regexMatches
  ,regexMatchesCI
  ,regexReplace
  ,regexReplaceCI
  ,regexReplaceMemo
  ,regexReplaceCIMemo
  ,regexReplaceBy
  ,regexReplaceByCI
   -- * total regex operations
  ,regexMatches_
  ,regexMatchesCI_
  ,regexReplace_
  ,regexReplaceCI_
  ,regexReplaceMemo_
  ,regexReplaceCIMemo_
  ,regexReplaceBy_
  ,regexReplaceByCI_
  ,toRegex_
  )
where

import Control.Monad (foldM)
import Data.Array
import Data.Char
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.MemoUgly (memo)
import Text.Regex.TDFA (
  Regex, CompOption(..), ExecOption(..), defaultCompOpt, defaultExecOpt,
  makeRegexOptsM, AllMatches(getAllMatches), match, (=~), MatchText
  )

import Hledger.Utils.UTF8IOCompat (error')


-- | Regular expression. Extended regular expression-ish syntax ? But does not support eg (?i) syntax.
type Regexp = String

-- | A replacement pattern. May include numeric backreferences (\N).
type Replacement = String

-- | An regular expression compilation/processing error message.
type RegexError = String

--------------------------------------------------------------------------------
-- old partial functions  -- PARTIAL:

-- regexMatch' :: RegexContext Regexp String a => Regexp -> String -> a
-- regexMatch' r s = s =~ (toRegex' r)

regexMatches :: Regexp -> String -> Bool
regexMatches = flip (=~)

regexMatchesCI :: Regexp -> String -> Bool
regexMatchesCI r = match (toRegexCI r)

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

-- | Replace all occurrences of the regexp, transforming each match with the given function.
regexReplaceBy :: Regexp -> (String -> String) -> String -> String
regexReplaceBy r = replaceAllBy (toRegex r)

regexReplaceByCI :: Regexp -> (String -> String) -> String -> String
regexReplaceByCI r = replaceAllBy (toRegexCI r)

-- helpers

-- | Convert our string-based Regexp to a real Regex.
-- Or if it's not well formed, call error with a "malformed regexp" message.
toRegex :: Regexp -> Regex
toRegex = memo (compileRegex defaultCompOpt defaultExecOpt)  -- PARTIAL:

-- | Like toRegex but make a case-insensitive Regex.
toRegexCI :: Regexp -> Regex
toRegexCI = memo (compileRegex defaultCompOpt{caseSensitive=False} defaultExecOpt)  -- PARTIAL:

compileRegex :: CompOption -> ExecOption -> Regexp -> Regex
compileRegex compopt execopt r =
  fromMaybe
  (error $ "this regular expression could not be compiled: " ++ show r) $  -- PARTIAL:
  makeRegexOptsM compopt execopt r

replaceRegex :: Regex -> Replacement -> String -> String
replaceRegex re repl s = foldl (replaceMatch repl) s (reverse $ match re s :: [MatchText String])
  where
    replaceMatch :: Replacement -> String -> MatchText String -> String
    replaceMatch replpat s matchgroups = pre ++ repl ++ post
      where
        ((_,(off,len)):_) = elems matchgroups  -- groups should have 0-based indexes, and there should always be at least one, since this is a match
        (pre, post') = splitAt off s
        post = drop len post'
        repl = replaceAllBy (toRegex "\\\\[0-9]+") (lookupMatchGroup matchgroups) replpat
          where
            lookupMatchGroup :: MatchText String -> String -> String
            lookupMatchGroup grps ('\\':s@(_:_)) | all isDigit s =
              case read s of n | n `elem` indices grps -> fst (grps ! n)
              -- PARTIAL:
                             _                         -> error' $ "no match group exists for backreference \"\\"++s++"\""
            lookupMatchGroup _ s = error' $ "lookupMatchGroup called on non-numeric-backreference \""++s++"\", shouldn't happen"

--------------------------------------------------------------------------------
-- new total functions

-- | Does this regexp match the given string ?
-- Or return an error if the regexp is malformed.
regexMatches_ :: Regexp -> String -> Either RegexError Bool
regexMatches_ r s = (`match` s) <$> toRegex_ r

-- | Like regexMatches_ but match case-insensitively.
regexMatchesCI_ :: Regexp -> String -> Either RegexError Bool
regexMatchesCI_ r s = (`match` s) <$> toRegexCI_ r

-- | Replace all occurrences of the regexp with the replacement
-- pattern, or return an error message. The replacement pattern
-- supports numeric backreferences (\N) but no other RE syntax.
regexReplace_ :: Regexp -> Replacement -> String -> Either RegexError String
regexReplace_ re repl s = toRegex_ re >>= \rx -> replaceRegex_ rx repl s

-- | Like regexReplace_ but match occurrences case-insensitively.
regexReplaceCI_ :: Regexp -> Replacement -> String -> Either RegexError String
regexReplaceCI_ re repl s = toRegexCI_ re >>= \rx -> replaceRegex_ rx repl s

-- | A memoising version of regexReplace_. Caches the result for each
-- search pattern, replacement pattern, target string tuple.
regexReplaceMemo_ :: Regexp -> Replacement -> String -> Either RegexError String
regexReplaceMemo_ re repl = memo (regexReplace_ re repl)

-- | Like regexReplaceMemo_ but match occurrences case-insensitively.
regexReplaceCIMemo_ :: Regexp -> Replacement -> String -> Either RegexError String
regexReplaceCIMemo_ re repl = memo (regexReplaceCI_ re repl)

-- | Replace all occurrences of the regexp, transforming each match
-- with the given function, or return an error message.
regexReplaceBy_ :: Regexp -> (String -> String) -> String -> Either RegexError String
regexReplaceBy_ r f s = toRegex_ r >>= \rx -> Right $ replaceAllBy rx f s

-- | Like regexReplaceBy_ but match occurrences case-insensitively.
regexReplaceByCI_ :: Regexp -> (String -> String) -> String -> Either RegexError String
regexReplaceByCI_ r f s = toRegexCI_ r >>= \rx -> Right $ replaceAllBy rx f s

-- helpers:

-- Convert a Regexp string to a compiled Regex, or return an error message.
toRegex_ :: Regexp -> Either RegexError Regex
toRegex_ = memo (compileRegex_ defaultCompOpt defaultExecOpt)

-- Like toRegex, but make a case-insensitive Regex.
toRegexCI_ :: Regexp -> Either RegexError Regex
toRegexCI_ = memo (compileRegex_ defaultCompOpt{caseSensitive=False} defaultExecOpt)

-- Compile a Regexp string to a Regex with the given options, or return an
-- error message if this fails.
compileRegex_ :: CompOption -> ExecOption -> Regexp -> Either RegexError Regex
compileRegex_ compopt execopt r =
  maybe (Left $ "this regular expression could not be compiled: " ++ show r) Right $
  makeRegexOptsM compopt execopt r

-- Replace this regular expression with this replacement pattern in this
-- string, or return an error message.
replaceRegex_ :: Regex -> Replacement -> String -> Either RegexError String
replaceRegex_ re repl s = foldM (replaceMatch_ repl) s (reverse $ match re s :: [MatchText String])
  where
    -- Replace one match within the string with the replacement text
    -- appropriate for this match. Or return an error message.
    replaceMatch_ :: Replacement -> String -> MatchText String -> Either RegexError String
    replaceMatch_ replpat s matchgroups =
      erepl >>= \repl -> Right $ pre ++ repl ++ post
      where
        ((_,(off,len)):_) = elems matchgroups  -- groups should have 0-based indexes, and there should always be at least one, since this is a match
        (pre, post') = splitAt off s
        post = drop len post'
        -- The replacement text: the replacement pattern with all
        -- numeric backreferences replaced by the appropriate groups
        -- from this match. Or an error message.
        erepl = toRegex_ "\\\\[0-9]+" >>= \rx -> replaceAllByM rx (lookupMatchGroup_ matchgroups) replpat
          where
            -- Given some match groups and a numeric backreference,
            -- return the referenced group text, or an error message.
            lookupMatchGroup_ :: MatchText String -> String -> Either RegexError String
            lookupMatchGroup_ grps ('\\':s@(_:_)) | all isDigit s = 
              case read s of n | n `elem` indices grps -> Right $ fst (grps ! n)
                             _                         -> Left $ "no match group exists for backreference \"\\"++s++"\""
            lookupMatchGroup_ _ s = Left $ "lookupMatchGroup called on non-numeric-backreference \""++s++"\", shouldn't happen"

-- helpers

-- adapted from http://stackoverflow.com/questions/9071682/replacement-substition-with-haskell-regex-libraries:

-- Replace all occurrences of a regexp in a string, transforming each match
-- with the given pure function.
replaceAllBy :: Regex -> (String -> String) -> String -> String
replaceAllBy re transform s = prependdone rest
  where
    (_, rest, prependdone) = foldl' go (0, s, id) matches
      where
        matches = getAllMatches $ match re s :: [(Int, Int)]  -- offset and length
        go :: (Int,String,String->String) -> (Int,Int) ->  (Int,String,String->String)
        go (pos,todo,prepend) (off,len) =
          let (prematch, matchandrest) = splitAt (off - pos) todo
              (matched, rest) = splitAt len matchandrest
          in (off + len, rest, prepend . (prematch++) . (transform matched ++))

-- Replace all occurrences of a regexp in a string, transforming each match
-- with the given monadic function. Eg if the monad is Either, a Left result
-- from the transform function short-circuits and is returned as the overall
-- result.
replaceAllByM :: forall m. Monad m => Regex -> (String -> m String) -> String -> m String
replaceAllByM re transform s =
  foldM go (0, s, id) matches >>= \(_, rest, prependdone) -> pure $ prependdone rest
  where
    matches = getAllMatches $ match re s :: [(Int, Int)]  -- offset and length
    go :: (Int,String,String->String) -> (Int,Int) -> m (Int,String,String->String)
    go (pos,todo,prepend) (off,len) =
      let (prematch, matchandrest) = splitAt (off - pos) todo
          (matched, rest) = splitAt len matchandrest
      in transform matched >>= \matched' -> pure (off + len, rest, prepend . (prematch++) . (matched' ++))

