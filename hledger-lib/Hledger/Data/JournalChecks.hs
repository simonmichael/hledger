{-|
Various additional validation checks that can be performed on a Journal.
Some are called as part of reading a file in strict mode,
others can be called only via the check command.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hledger.Data.JournalChecks (
  journalCheckAccounts,
  journalCheckCommodities,
  journalCheckPayees,
  journalCheckPairedConversionPostings,
  journalCheckRecentAssertions,
  module Hledger.Data.JournalChecks.Ordereddates,
  module Hledger.Data.JournalChecks.Uniqueleafnames,
)
where

import Data.Char (isSpace)
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Safe (atMay, lastMay)
import Text.Printf (printf)

import Hledger.Data.Errors
import Hledger.Data.Journal
import Hledger.Data.JournalChecks.Ordereddates
import Hledger.Data.JournalChecks.Uniqueleafnames
import Hledger.Data.Posting (isVirtual, postingDate, postingStatus)
import Hledger.Data.Types
import Hledger.Data.Amount (amountIsZero, amountsRaw, missingamt)
import Hledger.Data.Transaction (transactionPayee, showTransactionLineFirstPart, partitionAndCheckConversionPostings)
import Data.Time (Day, diffDays)
import Data.List.Extra
import Hledger.Utils (chomp, textChomp, sourcePosPretty)

-- | Check that all the journal's postings are to accounts  with
-- account directives, returning an error message otherwise.
journalCheckAccounts :: Journal -> Either String ()
journalCheckAccounts j = mapM_ checkacct (journalPostings j)
  where
    checkacct p@Posting{paccount=a}
      | a `elem` journalAccountNamesDeclared j = Right ()
      | otherwise = Left $ printf (unlines [
           "%s:%d:"
          ,"%s"
          ,"Strict account checking is enabled, and"
          ,"account %s has not been declared."
          ,"Consider adding an account directive. Examples:"
          ,""
          ,"account %s"
          ,"account %s    ; type:A  ; (L,E,R,X,C,V)"
          ]) f l ex (show a) a a
        where
          (f,l,_mcols,ex) = makePostingAccountErrorExcerpt p

-- | Check that all the commodities used in this journal's postings have been declared
-- by commodity directives, returning an error message otherwise.
journalCheckCommodities :: Journal -> Either String ()
journalCheckCommodities j = mapM_ checkcommodities (journalPostings j)
  where
    checkcommodities p =
      case findundeclaredcomm p of
        Nothing -> Right ()
        Just (comm, _) ->
          Left $ printf (unlines [
           "%s:%d:"
          ,"%s"
          ,"Strict commodity checking is enabled, and"
          ,"commodity %s has not been declared."
          ,"Consider adding a commodity directive. Examples:"
          ,""
          ,"commodity %s1000.00"
          ,"commodity 1.000,00 %s"
          ]) f l ex (show comm) comm comm
          where
            (f,l,_mcols,ex) = makePostingErrorExcerpt p finderrcols
      where
        -- Find the first undeclared commodity symbol in this posting's amount
        -- or balance assertion amount, if any. The boolean will be true if
        -- the undeclared symbol was in the posting amount.
        findundeclaredcomm :: Posting -> Maybe (CommoditySymbol, Bool)
        findundeclaredcomm Posting{pamount=amt,pbalanceassertion} =
          case (findundeclared postingcomms, findundeclared assertioncomms) of
            (Just c, _) -> Just (c, True)
            (_, Just c) -> Just (c, False)
            _           -> Nothing
          where
            postingcomms = map acommodity $ filter (not . isIgnorable) $ amountsRaw amt
              where
                -- Ignore missing amounts and zero amounts without commodity (#1767)
                isIgnorable a = (T.null (acommodity a) && amountIsZero a) || a == missingamt
            assertioncomms = [acommodity a | Just a <- [baamount <$> pbalanceassertion]]
            findundeclared = find (`M.notMember` jcommodities j)

        -- Calculate columns suitable for highlighting the excerpt.
        -- We won't show these in the main error line as they aren't
        -- accurate for the actual data.

        -- Find the best position for an error column marker when this posting
        -- is rendered by showTransaction.
        -- Reliably locating a problem commodity symbol in showTransaction output
        -- is really tricky. Some examples:
        --
        --     assets      "C $" -1 @ $ 2
        --                            ^
        --     assets      $1 = $$1
        --                      ^
        --     assets   [ANSI RED]$-1[ANSI RESET]
        --              ^
        --
        -- To simplify, we will mark the whole amount + balance assertion region, like:
        --     assets      "C $" -1 @ $ 2
        --                 ^^^^^^^^^^^^^^
        -- XXX refine this region when it's easy
        finderrcols p' t txntxt =
          case transactionFindPostingIndex (==p') t of
            Nothing     -> Nothing
            Just pindex -> Just (amtstart, Just amtend)
              where
                tcommentlines = max 0 (length (T.lines $ tcomment t) - 1)
                errrelline = 1 + tcommentlines + pindex   -- XXX doesn't count posting coment lines
                errline = fromMaybe "" (T.lines txntxt `atMay` (errrelline-1))
                acctend = 4 + T.length (paccount p') + if isVirtual p' then 2 else 0
                amtstart = acctend + (T.length $ T.takeWhile isSpace $ T.drop acctend errline) + 1
                amtend = amtstart + (T.length $ T.stripEnd $ T.takeWhile (/=';') $ T.drop amtstart errline)

-- | Check that all the journal's transactions have payees declared with
-- payee directives, returning an error message otherwise.
journalCheckPayees :: Journal -> Either String ()
journalCheckPayees j = mapM_ checkpayee (jtxns j)
  where
    checkpayee t
      | payee `elem` journalPayeesDeclared j = Right ()
      | otherwise = Left $
        printf (unlines [
           "%s:%d:"
          ,"%s"
          ,"Strict payee checking is enabled, and"
          ,"payee %s has not been declared."
          ,"Consider adding a payee directive. Examples:"
          ,""
          ,"payee %s"
          ]) f l ex (show payee) payee
      where
        payee = transactionPayee t
        (f,l,_mcols,ex) = makeTransactionErrorExcerpt t finderrcols
        -- Calculate columns suitable for highlighting the excerpt.
        -- We won't show these in the main error line as they aren't
        -- accurate for the actual data.
        finderrcols t' = Just (col, Just col2)
          where
            col  = T.length (showTransactionLineFirstPart t') + 2
            col2 = col + T.length (transactionPayee t') - 1

-- | In each tranaction, check that any conversion postings occur in adjacent pairs.
journalCheckPairedConversionPostings :: Journal -> Either String ()
journalCheckPairedConversionPostings j =
  mapM_ (transactionCheckPairedConversionPostings (jaccounttypes j)) $ jtxns j

transactionCheckPairedConversionPostings :: M.Map AccountName AccountType -> Transaction -> Either String ()
transactionCheckPairedConversionPostings accttypes t =
  case partitionAndCheckConversionPostings True accttypes (zip [0..] $ tpostings t) of
    Left err -> Left $ T.unpack err
    Right _  -> Right ()

----------

-- | Information useful for checking the age and lag of an account's latest balance assertion.
data BalanceAssertionInfo = BAI {
    baiAccount                :: AccountName -- ^ the account
  , baiLatestAssertionPosting :: Posting     -- ^ the account's latest posting with a balance assertion
  , baiLatestAssertionDate    :: Day         -- ^ the posting date
  , baiLatestAssertionStatus  :: Status      -- ^ the posting status
  , baiLatestPostingDate      :: Day         -- ^ the date of this account's latest posting with or without a balance assertion
}

-- | Given a list of postings to the same account,
-- if any of them contain a balance assertion,
-- calculate the last asserted and posted dates.
balanceAssertionInfo :: [Posting] -> Maybe BalanceAssertionInfo
balanceAssertionInfo ps =
  case (mlatestp, mlatestassertp) of
    (Just latestp, Just latestassertp) -> Just $
      BAI{baiAccount                 = paccount latestassertp
          ,baiLatestAssertionDate    = postingDate latestassertp
          ,baiLatestAssertionPosting = latestassertp
          ,baiLatestAssertionStatus  = postingStatus latestassertp
          ,baiLatestPostingDate      = postingDate latestp
          }
    _ -> Nothing
  where
    ps' = sortOn postingDate ps
    mlatestp = lastMay ps'
    mlatestassertp = lastMay [p | p@Posting{pbalanceassertion=Just _} <- ps']

-- | The number of days allowed between an account's latest balance assertion 
-- and latest posting.
maxlag = 7

-- | The number of days between this balance assertion and the latest posting in its account.
baiLag BAI{..} = diffDays baiLatestPostingDate baiLatestAssertionDate

-- -- | The earliest balance assertion date which would satisfy the recentassertions check.
-- baiLagOkDate :: BalanceAssertionInfo -> Day
-- baiLagOkDate BAI{..} = addDays (-7) baiLatestPostingDate

-- | Check that this latest assertion is close enough to the account's latest posting.
checkRecentAssertion :: BalanceAssertionInfo -> Either (BalanceAssertionInfo, String) ()
checkRecentAssertion bai@BAI{..}
  | lag > maxlag =
    Left (bai, printf (chomp $ unlines [
       "the last balance assertion (%s) was %d days before"
      ,"the latest posting (%s)."
      ])
      (show baiLatestAssertionDate) lag (show baiLatestPostingDate)
      )
  | otherwise = Right ()
  where 
    lag = baiLag bai

-- | Check that all the journal's accounts with balance assertions have
-- an assertion no more than 7 days before their latest posting.
-- Today's date is provided for error messages.
journalCheckRecentAssertions :: Day -> Journal -> Either String ()
journalCheckRecentAssertions today j =
  let
    acctps = groupOn paccount $ sortOn paccount $ journalPostings j
    acctassertioninfos = mapMaybe balanceAssertionInfo acctps
  in
    case mapM_ checkRecentAssertion acctassertioninfos of
      Right () -> Right ()
      Left (BAI{..}, msg) -> Left errmsg
        where
          errmsg = chomp $ printf 
            (unlines [
              "%s:",
              "%s\n",
              "The recentassertions check is enabled, so accounts with balance assertions must",
              "have a balance assertion no more than %d days before their latest posting date.",
              "In account %s,",
              "%s",
              "",
              "%s"
              ])
            (maybe "(no position)"  -- shouldn't happen
              (sourcePosPretty . baposition) $ pbalanceassertion baiLatestAssertionPosting)
            (textChomp excerpt)
            maxlag
            baiAccount
            msg
            recommendation
            where
              (_,_,_,excerpt) = makeBalanceAssertionErrorExcerpt baiLatestAssertionPosting
              recommendation = unlines [
                "Consider adding a more recent balance assertion for this account. Eg:",
                "",
                printf "%s *\n    %s    $0 = $0  ; <- adjust" (show today) baiAccount
                ]

-- -- | Print the last balance assertion date & status of all accounts with balance assertions.
-- printAccountLastAssertions :: Day -> [BalanceAssertionInfo] -> IO ()
-- printAccountLastAssertions today acctassertioninfos = do
--   forM_ acctassertioninfos $ \BAI{..} -> do
--     putStr $ printf "%-30s  %s %s, %d days ago\n"
--       baiAccount
--       (if baiLatestClearedAssertionStatus==Unmarked then " " else show baiLatestClearedAssertionStatus)
--       (show baiLatestClearedAssertionDate)
--       (diffDays today baiLatestClearedAssertionDate)
