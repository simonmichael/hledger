{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hledger.Cli.Commands.Registermatch (
  registermatchmode
 ,registermatch
)
where

import Data.Char (toUpper)
import Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Hledger
import Hledger.Cli.CliOptions
import Hledger.Cli.Commands.Register

registermatchmode = hledgerCommandMode
  $(embedFileRelative "Hledger/Cli/Commands/Registermatch.txt")
  []
  [generalflagsgroup1]
  hiddenflags
  ([], Just $ argsFlag "DESC")

registermatch :: CliOpts -> Journal -> IO ()
registermatch opts@CliOpts{rawopts_=rawopts,reportspec_=rspec} j =
  case listofstringopt "args" rawopts of
    [desc] -> do
        let ps = [p | (_,_,_,p,_) <- postingsReport rspec j]
        case similarPosting ps desc of
          Nothing -> putStrLn "no matches found."
          Just p  -> TL.putStr $ postingsReportAsText opts [pri]
                     where pri = (Just (postingDate p)
                                 ,Nothing
                                 ,tdescription <$> ptransaction p
                                 ,p
                                 ,nullmixedamt)
    _ -> putStrLn "please provide one description argument."

-- Identify the closest recent match for this description in the given date-sorted postings.
similarPosting :: [Posting] -> String -> Maybe Posting
similarPosting ps desc =
  let matches =
          sortBy compareRelevanceAndRecency
                     $ filter ((> threshold).fst)
                     [(maybe 0 (\t -> compareDescriptions desc (T.unpack $ tdescription t)) (ptransaction p), p) | p <- ps]
              where
                compareRelevanceAndRecency (n1,p1) (n2,p2) = compare (n2,postingDate p2) (n1,postingDate p1)
                threshold = 0
  in case matches of []  -> Nothing
                     m:_ -> Just $ snd m

-- -- Identify the closest recent match for this description in past transactions.
-- similarTransaction :: Journal -> Query -> String -> Maybe Transaction
-- similarTransaction j q desc =
--   case historymatches = transactionsSimilarTo j q desc of
--     ((,t):_) = Just t
--     []       = Nothing

compareDescriptions :: String -> String -> Double
compareDescriptions s t = compareStrings s' t'
    where s' = simplify s
          t' = simplify t
          simplify = filter (not . (`elem` ("0123456789"::String)))

-- | Return a similarity measure, from 0 to 1, for two strings.
-- This is Simon White's letter pairs algorithm from
-- http://www.catalysoft.com/articles/StrikeAMatch.html
-- with a modification for short strings.
compareStrings :: String -> String -> Double
compareStrings "" "" = 1
compareStrings [_] "" = 0
compareStrings "" [_] = 0
compareStrings [a] [b] = if toUpper a == toUpper b then 1 else 0
compareStrings s1 s2 = 2.0 * fromIntegral i / fromIntegral u
    where
      i = length $ intersect pairs1 pairs2
      u = length pairs1 + length pairs2
      pairs1 = wordLetterPairs $ uppercase s1
      pairs2 = wordLetterPairs $ uppercase s2

wordLetterPairs = concatMap letterPairs . words

letterPairs (a:b:rest) = [a,b] : letterPairs (b:rest)
letterPairs _ = []

