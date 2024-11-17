{-|
Various additional validation checks that can be performed on a Journal.
Some are called as part of reading a file in strict mode,
others can be called only via the check command.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hledger.Data.JournalChecks (
  journalStrictChecks,
  journalCheckAccounts,
  journalCheckBalanceAssertions,
  journalCheckCommodities,
  journalCheckPayees,
  journalCheckPairedConversionPostings,
  journalCheckRecentAssertions,
  journalCheckTags,
  module Hledger.Data.JournalChecks.Ordereddates,
  module Hledger.Data.JournalChecks.Uniqueleafnames,
)
where

import Data.Char (isSpace)
import Data.List.Extra
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Safe (atMay, lastMay, headMay)
import Text.Printf (printf)

import Hledger.Data.Errors
import Hledger.Data.Journal
import Hledger.Data.JournalChecks.Ordereddates
import Hledger.Data.JournalChecks.Uniqueleafnames
import Hledger.Data.Posting (isVirtual, postingDate, transactionAllTags, conversionPostingTagName, costPostingTagName, postingAsLines, generatedPostingTagName, generatedTransactionTagName, modifiedTransactionTagName)
import Hledger.Data.Types
import Hledger.Data.Amount (amountIsZero, amountsRaw, missingamt, oneLineFmt, showMixedAmountWith)
import Hledger.Data.Transaction (transactionPayee, showTransactionLineFirstPart, partitionAndCheckConversionPostings)
import Data.Time (diffDays)
import Hledger.Utils
import Data.Ord
import Hledger.Data.Dates (showDate)
import Hledger.Data.Balancing (journalBalanceTransactions, defbalancingopts)

-- | Run the extra -s/--strict checks on a journal, in order of priority,
-- returning the first error message if any of them fail.
journalStrictChecks :: Journal -> Either String ()
journalStrictChecks j = do
  -- keep the order of checks here synced with Check.md and Hledger.Cli.Commands.Check.Check.
  -- balanced is checked earlier, in journalFinalise
  journalCheckCommodities j
  journalCheckAccounts j

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
          ]) f l ex (show a) a
        where
          (f,l,_mcols,ex) = makePostingAccountErrorExcerpt p

-- | Check all balance assertions in the journal and return an error message if any of them fail.
-- (Technically, this also tries to balance the journal and can return balancing failure errors;
-- ensure the journal is already balanced (with journalBalanceTransactions) to avoid this.)
journalCheckBalanceAssertions :: Journal -> Either String ()
journalCheckBalanceAssertions = fmap (const ()) . journalBalanceTransactions defbalancingopts

-- | Check that all the commodities used in this journal's postings and P directives
-- have been declared by commodity directives, returning an error message otherwise.
journalCheckCommodities :: Journal -> Either String ()
journalCheckCommodities j = do
  mapM_ checkPriceDirectiveCommodities $ jpricedirectives j
  mapM_ checkPostingCommodities $ journalPostings j
  where
    firstUndeclaredOf comms = find (`M.notMember` jdeclaredcommodities j) comms

    errmsg = unlines [
        "%s:%d:"
      ,"%s"
      ,"Strict commodity checking is enabled, and"
      ,"commodity %s has not been declared."
      ,"Consider adding a commodity directive. Examples:"
      ,""
      ,"commodity %s1000.00"
      ,"commodity 1.000,00 %s"
      ]

    checkPriceDirectiveCommodities pd@PriceDirective{pdcommodity=c, pdamount=amt} =
      case firstUndeclaredOf [c, acommodity amt] of
        Nothing   -> Right ()
        Just comm -> Left $ printf errmsg f l ex (show comm) comm comm
          where (f,l,_mcols,ex) = makePriceDirectiveErrorExcerpt pd Nothing

    checkPostingCommodities p =
      case firstundeclaredcomm p of
        Nothing                    -> Right ()
        Just (comm, _inpostingamt) -> Left $ printf errmsg f l ex (show comm) comm comm
          where
            (f,l,_mcols,ex) = makePostingErrorExcerpt p finderrcols
      where
        -- Find the first undeclared commodity symbol in this posting's amount or balance assertion amount, if any.
        -- and whether it was in the posting amount.
        -- XXX The latter is currently unused, could be used to refine the error highlighting ?
        firstundeclaredcomm :: Posting -> Maybe (CommoditySymbol, Bool)
        firstundeclaredcomm Posting{pamount=amt,pbalanceassertion} =
          case (firstUndeclaredOf postingcomms, firstUndeclaredOf assertioncomms) of
            (Just c, _) -> Just (c, True)
            (_, Just c) -> Just (c, False)
            _           -> Nothing
          where
            assertioncomms = [acommodity a | Just a <- [baamount <$> pbalanceassertion]]
            postingcomms = map acommodity $ filter (not . isIgnorable) $ amountsRaw amt
              where
                isIgnorable a = a==missingamt || (amountIsZero a && T.null (acommodity a))  -- #1767

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

-- | Check that all the journal's tags (on accounts, transactions, postings..)
-- have been declared with tag directives, returning an error message otherwise.
journalCheckTags :: Journal -> Either String ()
journalCheckTags j = do
  mapM_ checkaccttags $ jdeclaredaccounts j
  mapM_ checktxntags  $ jtxns j
  where
    checkaccttags (a, adi) = mapM_ (checkaccttag.fst) $ aditags adi
      where
        checkaccttag tagname
          | tagname `elem` declaredtags = Right ()
          | otherwise = Left $ printf msg f l ex (show tagname) tagname
            where (f,l,_mcols,ex) = makeAccountTagErrorExcerpt (a, adi) tagname
    checktxntags txn = mapM_ (checktxntag . fst) $ transactionAllTags txn
      where
        checktxntag tagname
          | tagname `elem` declaredtags = Right ()
          | otherwise = Left $ printf msg f l ex (show tagname) tagname
            where
              (f,l,_mcols,ex) = makeTransactionErrorExcerpt txn finderrcols
                where
                  finderrcols _txn' = Nothing
                    -- don't bother for now
                    -- Just (col, Just col2)
                    -- where
                    --   col  = T.length (showTransactionLineFirstPart txn') + 2
                    --   col2 = col + T.length tagname - 1
    declaredtags = journalTagsDeclared j ++ builtinTags
    msg = (unlines [
      "%s:%d:"
      ,"%s"
      ,"Strict tag checking is enabled, and"
      ,"tag %s has not been declared."
      ,"Consider adding a tag directive. Examples:"
      ,""
      ,"tag %s"
      ])

-- | Tag names which have special significance to hledger, and need not be declared for `hledger check tags`.
-- Keep synced with check-tags.test and hledger manual > Special tags.
builtinTags = [
   "date"                   -- overrides a posting's date
  ,"date2"                  -- overrides a posting's secondary date
  ,"type"                   -- declares an account's type
  ,"t"                      -- appears on postings generated by timedot letters
  ,"assert"                 -- appears on txns generated by close --assert
  ,"retain"                 -- appears on txns generated by close --retain
  ,"start"                  -- appears on txns generated by close --migrate/--close/--open/--assign
  ]
  -- these tags are used in both hidden and visible form
  <> ts <> map toVisibleTagName ts
  where
    ts = [
       generatedTransactionTagName -- marks txns generated by periodic rule
      ,modifiedTransactionTagName  -- marks txns which have had auto postings added
      ,generatedPostingTagName     -- marks postings which have been generated
      ,costPostingTagName          -- marks equity conversion postings which have been matched with a nearby costful posting
      ,conversionPostingTagName    -- marks costful postings which have been matched with a nearby pair of equity conversion postings
      ]

-- | In each tranaction, check that any conversion postings occur in adjacent pairs.
journalCheckPairedConversionPostings :: Journal -> Either String ()
journalCheckPairedConversionPostings j =
  mapM_ (transactionCheckPairedConversionPostings conversionaccts) $ jtxns j
  where conversionaccts = journalConversionAccounts j

transactionCheckPairedConversionPostings :: [AccountName] -> Transaction -> Either String ()
transactionCheckPairedConversionPostings conversionaccts t =
  case partitionAndCheckConversionPostings True conversionaccts (zip [0..] $ tpostings t) of
    Left err -> Left $ T.unpack err
    Right _  -> Right ()

----------

-- | The number of days allowed between an account's latest balance assertion 
-- and latest posting (7).
maxlag = 7

-- | Check that accounts with balance assertions have no posting more
-- than maxlag days after their latest balance assertion.
journalCheckRecentAssertions :: Journal -> Either String ()
journalCheckRecentAssertions j =
  let acctps = groupOn paccount $ sortOn paccount $ journalPostings j
  in case mapMaybe findRecentAssertionError acctps of
    []         -> Right ()
    firsterr:_ -> Left firsterr

-- | Do the recentassertions check for one account: given a list of postings to the account,
-- if any of them contain a balance assertion, identify the latest balance assertion,
-- and if any postings are >maxlag days later than the assertion,
-- return an error message identifying the first of them.
-- Postings on the same date will be handled in parse order (hopefully).
findRecentAssertionError :: [Posting] -> Maybe String
findRecentAssertionError ps = do
  let rps = sortOn (Data.Ord.Down . postingDate) ps
  let (afterlatestassertrps, untillatestassertrps) = span (isNothing.pbalanceassertion) rps
  latestassertdate <- postingDate <$> headMay untillatestassertrps
  let withinlimit date = diffDays date latestassertdate <= maxlag
  firsterrorp <- lastMay $ dropWhileEnd (withinlimit.postingDate) afterlatestassertrps
  let lag = diffDays (postingDate firsterrorp) latestassertdate
  let acct = paccount firsterrorp
  let (f,l,_mcols,ex) = makePostingAccountErrorExcerpt firsterrorp
  -- let comm =
  --       case map acommodity $ amounts $ pamount firsterrorp of
  --         [] -> ""
  --         (t:_) | T.length t == 1 -> t
  --         (t:_) -> t <> " "
  Just $ chomp $ printf
    (unlines [
      "%s:%d:",
      "%s\n",
      -- "The recentassertions check is enabled, so accounts with balance assertions must",
      -- "have a balance assertion within %d days of their latest posting.",
      "The recentassertions check is enabled, so accounts with balance assertions",
      "must have a recent one, not more than %d days older than their latest posting.",
      "In account: %s",
      "the last assertion was on %s, %d days before this latest posting.",
      "Consider adding a new balance assertion to the above posting. Eg:",
      "",
      "%s = BALANCE"
      ])
    f
    l
    (textChomp ex)
    maxlag
    (bold' $ T.unpack acct)
    (showDate latestassertdate)
    lag
    (showposting firsterrorp)
    where
      showposting p =
        headDef "" $ first3 $ postingAsLines False True acctw amtw p{pcomment=""}
        where
          acctw = T.length $ paccount p
          amtw  = length $ showMixedAmountWith oneLineFmt $ pamount p

-- -- | Print the last balance assertion date & status of all accounts with balance assertions.
-- printAccountLastAssertions :: Day -> [BalanceAssertionInfo] -> IO ()
-- printAccountLastAssertions today acctassertioninfos = do
--   forM_ acctassertioninfos $ \BAI{..} -> do
--     putStr $ printf "%-30s  %s %s, %d days ago\n"
--       baiAccount
--       (if baiLatestClearedAssertionStatus==Unmarked then " " else show baiLatestClearedAssertionStatus)
--       (show baiLatestClearedAssertionDate)
--       (diffDays today baiLatestClearedAssertionDate)
