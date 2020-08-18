{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
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
  -- * Regexp type and constructors
   Regexp(reString)
  ,toRegex_
  ,toRegexCI_
  ,toRegex'
  ,toRegexCI'
   -- * type aliases
  ,Replacement
  ,RegexError
   -- * partial regex operations (may call error)
--   ,regexMatches
--   ,regexMatchesCI
--   ,regexReplaceCI
--   ,regexReplaceCIMemo
--   ,regexReplaceByCI
   -- * total regex operations
  ,match
  ,regexReplace
  ,regexReplaceMemo_
--   ,replaceAllBy
--   ,regexMatches_
--   ,regexMatchesCI_
--   ,regexReplace_
--   ,regexReplaceCI_
--   ,regexReplaceMemo_
--   ,regexReplaceCIMemo_
  ,replaceAllBy
  )
where

import Control.Monad (foldM)
import Data.Aeson (ToJSON(..), Value(String))
import Data.Array ((!), elems, indices)
import Data.Char (isDigit)
import Data.Data (Data(..), mkNoRepType)
import Data.List (foldl')
import Data.MemoUgly (memo)
import qualified Data.Text as T
import Text.Regex.TDFA (
  Regex, CompOption(..), defaultCompOpt, defaultExecOpt,
  makeRegexOptsM, AllMatches(getAllMatches), match, MatchText,
  RegexLike(..), RegexMaker(..), RegexOptions(..), RegexContext(..)
  )

import Hledger.Utils.UTF8IOCompat (error')


-- | Regular expression. Extended regular expression-ish syntax ? But does not support eg (?i) syntax.
data Regexp
  = Regexp   { reString :: String, reCompiled :: Regex }
  | RegexpCI { reString :: String, reCompiled :: Regex }

instance Eq Regexp where
  Regexp   s1 _ == Regexp   s2 _ = s1 == s2
  RegexpCI s1 _ == RegexpCI s2 _ = s1 == s2
  _ == _ = False

instance Ord Regexp where
  Regexp   s1 _ `compare` Regexp   s2 _ = s1 `compare` s2
  RegexpCI s1 _ `compare` RegexpCI s2 _ = s1 `compare` s2
  Regexp _ _ `compare` RegexpCI _ _ = LT
  RegexpCI _ _ `compare` Regexp _ _ = GT

instance Show Regexp where
  showsPrec d r = showParen (d > app_prec) $ reCons . showsPrec (app_prec+1) (reString r)
    where app_prec = 10
          reCons = case r of Regexp   _ _ -> showString "Regexp "
                             RegexpCI _ _ -> showString "RegexpCI "

instance Read Regexp where
  readsPrec d r =  readParen (d > app_prec) (\r -> [(toRegexCI' m,t) |
                                                    ("RegexCI",s) <- lex r,
                                                    (m,t) <- readsPrec (app_prec+1) s]) r
                ++ readParen (d > app_prec) (\r -> [(toRegex' m, t) |
                                                    ("Regex",s) <- lex r,
                                                    (m,t) <- readsPrec (app_prec+1) s]) r
    where app_prec = 10

instance Data Regexp where
  toConstr _   = error' "No toConstr for Regex"
  gunfold _ _  = error' "No gunfold for Regex"
  dataTypeOf _ = mkNoRepType "Hledger.Utils.Regex"

instance ToJSON Regexp where
  toJSON (Regexp   s _) = String . T.pack $ "Regexp "   ++ s
  toJSON (RegexpCI s _) = String . T.pack $ "RegexpCI " ++ s

instance RegexLike Regexp String where
  matchOnce = matchOnce . reCompiled
  matchAll = matchAll . reCompiled
  matchCount = matchCount . reCompiled
  matchTest = matchTest . reCompiled
  matchAllText = matchAllText . reCompiled
  matchOnceText = matchOnceText . reCompiled

instance RegexContext Regexp String String where
  match = match . reCompiled
  matchM = matchM . reCompiled

-- Convert a Regexp string to a compiled Regex, or return an error message.
toRegex_ :: String -> Either RegexError Regexp
toRegex_ = memo $ \s -> mkRegexErr s (Regexp s <$> makeRegexM s)

-- Like toRegex_, but make a case-insensitive Regex.
toRegexCI_ :: String -> Either RegexError Regexp
toRegexCI_ = memo $ \s -> mkRegexErr s (RegexpCI s <$> makeRegexOptsM defaultCompOpt{caseSensitive=False} defaultExecOpt s)

-- | Make a nice error message for a regexp error.
mkRegexErr :: String -> Maybe a -> Either RegexError a
mkRegexErr s = maybe (Left errmsg) Right
  where errmsg = "this regular expression could not be compiled: " ++ s

-- Convert a Regexp string to a compiled Regex, throw an error
toRegex' :: String -> Regexp
toRegex' = either error' id . toRegex_

-- Like toRegex', but make a case-insensitive Regex.
toRegexCI' :: String -> Regexp
toRegexCI' = either error' id . toRegexCI_

-- | A replacement pattern. May include numeric backreferences (\N).
type Replacement = String

-- | An regular expression compilation/processing error message.
type RegexError = String

-- helpers

regexReplace :: Regexp -> Replacement -> String -> String
regexReplace re repl s = foldl (replaceMatch repl) s (reverse $ match (reCompiled re) s :: [MatchText String])
  where
    replaceMatch :: Replacement -> String -> MatchText String -> String
    replaceMatch replpat s matchgroups = pre ++ repl ++ post
      where
        ((_,(off,len)):_) = elems matchgroups  -- groups should have 0-based indexes, and there should always be at least one, since this is a match
        (pre, post') = splitAt off s
        post = drop len post'
        repl = replaceAllBy backrefRegex (lookupMatchGroup matchgroups) replpat
          where
            lookupMatchGroup :: MatchText String -> String -> String
            lookupMatchGroup grps ('\\':s@(_:_)) | all isDigit s =
              case read s of n | n `elem` indices grps -> fst (grps ! n)
              -- PARTIAL:
                             _                         -> error' $ "no match group exists for backreference \"\\"++s++"\""
            lookupMatchGroup _ s = error' $ "lookupMatchGroup called on non-numeric-backreference \""++s++"\", shouldn't happen"
    backrefRegex = toRegex' "\\\\[0-9]+"  -- PARTIAL: should not error happen

--------------------------------------------------------------------------------
-- new total functions

-- | A memoising version of regexReplace_. Caches the result for each
-- search pattern, replacement pattern, target string tuple.
regexReplaceMemo_ :: Regexp -> Replacement -> String -> Either RegexError String
regexReplaceMemo_ re repl = memo (replaceRegexUnmemo_ re repl)

-- helpers:

-- Replace this regular expression with this replacement pattern in this
-- string, or return an error message.
replaceRegexUnmemo_ :: Regexp -> Replacement -> String -> Either RegexError String
replaceRegexUnmemo_ re repl s = foldM (replaceMatch_ repl) s (reverse $ match (reCompiled re) s :: [MatchText String])
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
        erepl = replaceAllByM backrefRegex (lookupMatchGroup_ matchgroups) replpat
          where
            -- Given some match groups and a numeric backreference,
            -- return the referenced group text, or an error message.
            lookupMatchGroup_ :: MatchText String -> String -> Either RegexError String
            lookupMatchGroup_ grps ('\\':s@(_:_)) | all isDigit s = 
              case read s of n | n `elem` indices grps -> Right $ fst (grps ! n)
                             _                         -> Left $ "no match group exists for backreference \"\\"++s++"\""
            lookupMatchGroup_ _ s = Left $ "lookupMatchGroup called on non-numeric-backreference \""++s++"\", shouldn't happen"
    backrefRegex = toRegex' "\\\\[0-9]+"  -- PARTIAL: should not happen

-- helpers

-- adapted from http://stackoverflow.com/questions/9071682/replacement-substition-with-haskell-regex-libraries:

-- Replace all occurrences of a regexp in a string, transforming each match
-- with the given pure function.
replaceAllBy :: Regexp -> (String -> String) -> String -> String
replaceAllBy re transform s = prependdone rest
  where
    (_, rest, prependdone) = foldl' go (0, s, id) matches
      where
        matches = getAllMatches $ match (reCompiled re) s :: [(Int, Int)]  -- offset and length
        go :: (Int,String,String->String) -> (Int,Int) ->  (Int,String,String->String)
        go (pos,todo,prepend) (off,len) =
          let (prematch, matchandrest) = splitAt (off - pos) todo
              (matched, rest) = splitAt len matchandrest
          in (off + len, rest, prepend . (prematch++) . (transform matched ++))

-- Replace all occurrences of a regexp in a string, transforming each match
-- with the given monadic function. Eg if the monad is Either, a Left result
-- from the transform function short-circuits and is returned as the overall
-- result.
replaceAllByM :: forall m. Monad m => Regexp -> (String -> m String) -> String -> m String
replaceAllByM re transform s =
  foldM go (0, s, id) matches >>= \(_, rest, prependdone) -> pure $ prependdone rest
  where
    matches = getAllMatches $ match (reCompiled re) s :: [(Int, Int)]  -- offset and length
    go :: (Int,String,String->String) -> (Int,Int) -> m (Int,String,String->String)
    go (pos,todo,prepend) (off,len) =
      let (prematch, matchandrest) = splitAt (off - pos) todo
          (matched, rest) = splitAt len matchandrest
      in transform matched >>= \matched' -> pure (off + len, rest, prepend . (prematch++) . (matched' ++))

