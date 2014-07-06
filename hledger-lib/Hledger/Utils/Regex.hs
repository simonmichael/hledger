-- Regular expression helpers.
-- Currently using mostly regexpr and some regex-tdfa.
-- Note many of these will die on malformed regexps.

module Hledger.Utils.Regex (
   regexMatch
  ,regexMatchCI
  ,regexMatches
  ,regexMatchesCI
  ,containsRegex
  ,regexReplace
  ,regexReplaceCI
  ,regexReplaceBy
  ,regexToCaseInsensitive
  ,regexSplit
  ,regexMatchesRegexCompat
  ,regexMatchesCIRegexCompat
  )
where
import Data.Maybe
import Text.Regex.TDFA
import Text.RegexPR

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

regexSplit :: String -> String -> [String]
regexSplit = splitRegexPR

-- regex-compat (regex-posix) functions that perform better than regexpr.
regexMatchesRegexCompat :: String -> String -> Bool
regexMatchesRegexCompat = flip (=~)

regexMatchesCIRegexCompat :: String -> String -> Bool
regexMatchesCIRegexCompat r = match (makeRegexOpts defaultCompOpt { multiline = True, caseSensitive = False, newSyntax = True } defaultExecOpt r)

