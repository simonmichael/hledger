{-|
Helpers for making error messages.
-}

{-# LANGUAGE OverloadedStrings #-}

module Hledger.Data.Errors (
  makeTransactionErrorExcerpt,
  makePostingErrorExcerpt,
  transactionFindPostingIndex,
)
where

import Data.Function ((&))
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import Hledger.Data.Transaction (showTransaction)
import Hledger.Data.Types
import Hledger.Utils

-- | Given a problem transaction and a function calculating the best
-- column(s) for marking the error region:
-- render it as a megaparsec-style excerpt, showing the original line number
-- on the transaction line, and a column(s) marker.
-- Returns the file path, line number, column(s) if known,
-- and the rendered excerpt, or as much of these as is possible.
-- A limitation: columns will be accurate for the rendered error message but not for the original journal data.
makeTransactionErrorExcerpt :: Transaction -> (Transaction -> Maybe (Int, Maybe Int)) -> (FilePath, Int, Maybe (Int, Maybe Int), Text)
makeTransactionErrorExcerpt t findtxnerrorcolumns = (f, tl, merrcols, ex)
  -- XXX findtxnerrorcolumns is awkward, I don't think this is the final form
  where
    (SourcePos f tpos _) = fst $ tsourcepos t
    tl = unPos tpos
    txntxt = showTransaction t & textChomp & (<>"\n")
    merrcols = findtxnerrorcolumns t
    ex = decorateTransactionErrorExcerpt tl merrcols txntxt

-- | Add megaparsec-style left margin, line number, and optional column marker(s).
decorateTransactionErrorExcerpt :: Int -> Maybe (Int, Maybe Int) -> Text -> Text
decorateTransactionErrorExcerpt l mcols txt =
  T.unlines $ ls' <> colmarkerline <> map (lineprefix<>) ms
  where
    (ls,ms) = splitAt 1 $ T.lines txt
    ls' = map ((T.pack (show l) <> " | ") <>) ls
    colmarkerline =
      [lineprefix <> T.replicate (col-1) " " <> T.replicate regionw "^"
      | Just (col, mendcol) <- [mcols]
      , let regionw = maybe 1 (subtract col) mendcol + 1
      ]
    lineprefix = T.replicate marginw " " <> "| "
      where  marginw = length (show l) + 1

-- | Given a problem posting and a function calculating the best
-- column(s) for marking the error region:
-- look up error info from the parent transaction, and render the transaction
-- as a megaparsec-style excerpt, showing the original line number
-- on the problem posting's line, and a column indicator.
-- Returns the file path, line number, column(s) if known,
-- and the rendered excerpt, or as much of these as is possible.
-- A limitation: columns will be accurate for the rendered error message but not for the original journal data.
makePostingErrorExcerpt :: Posting -> (Posting -> Transaction -> Text -> Maybe (Int, Maybe Int)) -> (FilePath, Int, Maybe (Int, Maybe Int), Text)
makePostingErrorExcerpt p findpostingerrorcolumns =
  case ptransaction p of
    Nothing -> ("-", 0, Nothing, "")
    Just t  -> (f, errabsline, merrcols, ex)
      where
        (SourcePos f tl _) = fst $ tsourcepos t
        tcommentlines = max 0 (length (T.lines $ tcomment t) - 1)
        mpindex = transactionFindPostingIndex (==p) t
        errrelline = maybe 0 (tcommentlines+) mpindex   -- XXX doesn't count posting coment lines
        errabsline = unPos tl + errrelline
        txntxt = showTransaction t & textChomp & (<>"\n")
        merrcols = findpostingerrorcolumns p t txntxt
        ex = decoratePostingErrorExcerpt errabsline errrelline merrcols txntxt

-- | Add megaparsec-style left margin, line number, and optional column marker(s).
decoratePostingErrorExcerpt :: Int -> Int -> Maybe (Int, Maybe Int) -> Text -> Text
decoratePostingErrorExcerpt absline relline mcols txt =
  T.unlines $ js' <> ks' <> colmarkerline <> ms'
  where
    (ls,ms) = splitAt (relline+1) $ T.lines txt
    (js,ks) = splitAt (length ls - 1) ls
    (js',ks') = case ks of
      [k] -> (map (lineprefix<>) js, [T.pack (show absline) <> " | " <> k])
      _   -> ([], [])
    ms' = map (lineprefix<>) ms
    colmarkerline =
      [lineprefix <> T.replicate (col-1) " " <> T.replicate regionw "^"
      | Just (col, mendcol) <- [mcols]
      , let regionw = 1 + maybe 0 (subtract col) mendcol
      ]
    lineprefix = T.replicate marginw " " <> "| "
      where  marginw = length (show absline) + 1

-- | Find the 1-based index of the first posting in this transaction
-- satisfying the given predicate.
transactionFindPostingIndex :: (Posting -> Bool) -> Transaction -> Maybe Int
transactionFindPostingIndex ppredicate = 
  fmap fst . find (ppredicate.snd) . zip [1..] . tpostings

